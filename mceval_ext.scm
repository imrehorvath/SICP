
;; The Metacircular Evaluator

;; The Core of the Evaluator

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((and? exp) (mc-eval (and->if exp) env))
	((or? exp) (mc-eval (or->if exp) env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (mc-eval (cond->if exp) env))
	((let? exp) (mc-eval (let->combination exp) env))
	((let*? exp) (mc-eval (let*->nested-lets exp) env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- MC-EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment-for-proc-application
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- MC-APPLY" procedure))))

;; Load support

(define (mc-load file)
  (define last-val 'EMPTY-FILE)
  (define (loader)
    (let ((exp (read)))
      (if (eof-object? exp)
	  last-val
	  (begin (set! last-val (mc-eval exp
					 the-global-environment))
		 (loader)))))
  (with-input-from-file file loader))

;; Procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; Conditionals

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

;; Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
	(else (mc-eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (mc-eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;; Representing expression

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (make-assignment var exp) (list 'set! var exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-combination operator operands)
  (cons operator operands))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; Derived expressions

;; and

(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))

(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))

(define (expand-and-predicates preds)
  (if (null? preds)
      'true
      (let ((first (car preds))
	    (rest (cdr preds)))
	(if (null? rest)
	    first
	    (make-if first
		     (expand-and-predicates rest)
		     'false)))))

;; or

(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))

(define (or->if exp)
  (expand-or-predicates (or-predicates exp)))

(define (expand-or-predicates preds)
  (if (null? preds)
      'false
      (let ((first (car preds))
	    (rest (cdr preds))
	    (var (gensym)))
	(if (null? rest)
	    first
	    (make-let (make-let-bindings var first)
		      (make-if var
			       var
			       (expand-or-predicates rest)))))))

;; cond

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-arrow-clause? clause)
  (let ((actions (cond-actions clause)))
    (if (null? actions)
	#f
	(eq? (car actions) '=>))))

(define (cond-arrow-action clause) (caddr clause))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false            ;; no else clause
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND->IF"
			  clauses)))
	      ((cond-arrow-clause? first)
	       (let ((var (gensym)))
		 (make-let (make-let-bindings var (cond-predicate first))
			   (make-if var
				    (make-combination (cond-arrow-action first)
						      (list var))
				    (expand-clauses rest)))))
	      (else
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))

;; let

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-var exp) (car exp))
(define (let-binding-exp exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (make-let bindings . body) (cons 'let (cons bindings body)))
(define (make-let-bindings . bindings)
  (define (bindings-loop bindings)
    (cond ((null? bindings) bindings)
	  ((null? (cdr bindings))
	   (error "Let binding missess value expression" bindings))
	  (else
	   (cons (list (car bindings)
		       (cadr bindings))
		 (bindings-loop (cddr bindings))))))
  (bindings-loop bindings))
(define (make-let-binding var exp) (list var exp))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination-helper exp)
      (let->combination-helper exp)))

(define (let->combination-helper exp)
  (let ((bindings (let-bindings exp)))
    (let ((vars (let-vars bindings))
	  (exps (let-exps bindings)))
      (make-combination (make-lambda vars (let-body exp))
			exps))))

(define (let-vars bindings) (map car bindings))
(define (let-exps bindings) (map cadr bindings))

;; let*

(define (let*? exp) (tagged-list? exp 'let*))

(define (make-let* bindings body) (cons 'let* (cons bindings body)))

(define (let*->nested-lets exp)
  (let ((bindings (let-bindings exp))
	(body (let-body exp)))
    (define (bindings-loop bindings)
      (let ((first (car bindings))
	    (rest (cdr bindings)))
	(if (null? rest)
	    (apply make-let
		   (cons (make-let-bindings
			  (let-binding-var first)
			  (let-binding-exp first))
			 body))
	    (make-let (make-let-bindings
		       (let-binding-var first)
		       (let-binding-exp first))
		      (bindings-loop rest)))))
    (bindings-loop bindings)))

;; named let

(define (named-let? exp)
  (if (let? exp)
      (symbol? (cadr exp))
      #f))

(define (named-let-variable exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (make-named-let var bindings body)
  (cons 'let (cons var (cons bindings body))))

(define (named-let->combination-helper exp)
  (let ((var (named-let-variable exp))
	(bindings (named-let-bindings exp))
	(body (named-let-body exp)))
    (let ((vars (let-vars bindings))
	  (exps (let-exps bindings)))
      (make-let (make-let-bindings var ''*unassigned*)
		(make-assignment var (make-lambda vars body))
		(make-combination var exps)))))

;; Evaluator Data Structures

;; Testing of predicates

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;; Representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (scan-out-defines (caddr p)))
(define (procedure-environment p) (cadddr p))

;; Internal definitions

(define (scan-out-defines procedure-body)
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
  (let ((new-body (body-loop procedure-body)))
    (if (null? bindings)
	procedure-body
	(list (apply make-let
		     (cons bindings new-body))))))

;; Operations on Environments

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (extend-environment-for-proc-application vars vals base-env)
  (define (extend-loop params args)
    (cond ((null? args)
	   (error "Too few arguments supplied" vars vals))
	  ((symbol? (cdr params))
	   (list (list (car params) (cdr params))
		 (list (car args) (cdr args))))
	  (else
	   (let ((lists (extend-loop (cdr params) (cdr args))))
	     (list (cons (car params) (car lists))
		   (cons (car args) (cadr lists)))))))
  (cond ((symbol? vars)                            ;;; (lambda args <body>)
	 (cons (make-frame (list vars)
			   (list vals))
	       base-env))
	((list? vars)                              ;;; (lambda (a b c) <body>)
	 (extend-environment vars vals base-env))
	(else                                      ;;; (lambda (a b . c) <body>)
	 (let ((lists (extend-loop vars vals)))
	   (extend-environment (car lists)
			       (cadr lists)
			       base-env)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (if (eq? (car vals) '*unassigned*)
		 (error "Value is unassigned" (car vars))
		 (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;; Running the Evaluator as a Program

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list '< <)
	(list '> >)
	(list 'remainder remainder)
	(list 'modulo modulo)
	(list 'zero? zero?)
	(list 'cadr cadr)
	(list 'caddr caddr)
	(list 'cadddr cadddr)
	(list 'cddr cddr)
	(list 'list list)
	(list 'append append)
	(list 'length length)
	(list 'equal? equal?)
	(list 'assoc assoc)
	(list 'display display)
	(list 'newline newline)
	(list 'load mc-load)
	;; more primitives
	))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc)    ;; underlying apply
	 args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;; (driver-loop)

