(load "mceval.scm")

(define (mc-eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((let? exp) (analyze (let->combination exp)))         ;; **
	((application? exp) (analyze-application exp))
	(else
	 (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
	  (cproc env)
	  (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(body-exps (scan-out-defines                               ;; **
		    (lambda-body exp))))                           ;; **
    (let ((bproc (analyze-sequence body-exps)))
      (lambda (env) (make-procedure vars bproc env body-exps)))))  ;; **

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
			   (map (lambda (aproc) (aproc env))
				aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))))
	(else
	 (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (make-procedure parameters body env body-exps)
  (list 'procedure parameters body env body-exps))     ;; **

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (if (eq? (car vals) '*unassigned*)           ;; **
		 (error "Value is unassigned" (car vars))
		 (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (make-assignment var exp) (list 'set! var exp))

(define (scan-out-defines body)
  (define bindings '())
  (define (body-loop body)
    (cond ((null? body)
	   (set! bindings (reverse bindings))
	   body)
	  (else
	   (cons (if (definition? (car body))
		     (begin
		       (set! bindings
			     (cons (make-let-binding
				    (definition-variable (car body))
				    ''*unassigned*)
				   bindings))
		       (make-assignment (definition-variable (car body))
					(definition-value (car body))))
		     (car body))
		 (body-loop (cdr body))))))
  (let ((new-body (body-loop body)))
    (if (null? bindings)
	body
	(list
	 (make-let bindings
		   new-body)))))

(define (procedure-body-exps p) (car (cddddr p)))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body-exps object)      ;; **
		     '<procedure-env>))
      (display object)))

;(driver-loop)
