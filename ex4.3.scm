;; The evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'eval-proc (operator exp))
	 ((get 'eval-proc (operator exp)) exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (operator exp) (car exp))

(put 'eval-proc quote-tag (lambda (exp env) (text-of-quotion exp)))
(put 'eval-proc assignment-tag (lambda (exp env) (eval-assignment exp env)))
;; ...

;;	((quoted? exp) (text-of-quotion exp))
;;	((assignment? exp) (eval-assignment exp env))
;;	((definition? exp) (eval-definition exp env))
;;	((if? exp) (eval-if exp env))
;;	((lambda? exp)
;;	 (make-procedure (lambda-parameters exp)
;;			 (lambda-body exp)
;;			 env))
;;	((begin? exp)
;;	 (eval-sequence (begin-actions exp) env))
;;	((cond? exp) (eval (cond->if exp) env))

