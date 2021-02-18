(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        #f
        (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))
        (set-cdr! record value)))
  'ok)

(define (make-table)
  (list '*table*))

(define special-forms-table (make-table))

(insert! 'quote
	 (lambda (exp env) (text-of-quotation exp))
	 special-forms-table)

(insert! 'set!
	 (lambda (exp env) (eval-assignment exp env))
	 special-forms-table)

(insert! 'define
	 (lambda (exp env) (eval-definition exp env))
	 special-forms-table)

(insert! 'if
	 (lambda (exp env) (eval-if exp env))
	 special-forms-table)

(insert! 'lambda
	 (lambda (exp env)
	   (make-procedure (lambda-parameters exp)
			   (lambda-body exp)
			   env))
	 special-forms-table)

(insert! 'begin
	 (lambda (exp env)
	   (eval-sequence (begin-actions exp) env))
	 special-forms-table)

(insert! 'cond
	 (lambda (exp env)
	   (mc-eval (cond->if exp) env))
	 special-forms-table)

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((special-form? exp) (eval-special-form exp env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- MC-EVAL" exp))))

(define (special-form? exp)
  (if (pair? exp)
      (lookup (operator exp) special-forms-table)
      #f))

(define (eval-special-form exp env)
  (let ((proc (lookup (operator exp) special-forms-table)))
    (proc exp env)))
