;; and and or special forms directly into the evaluator
;;
;; evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotion exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; and syntax procedures
(define (and? exp) (tagged-list? exp 'and))

(define (and-exps exp) (cdr exp))

;; and eval procedures
(define (eval-and exp env)
  (eval-and-exps (and-exps exp) env))

(define (eval-and-exps exps env)
  (cond ((null? exps) true)
	((last-exp? exps) (eval (first-exp exps) env))
	(else
	 (if (false? (eval (first-exp exps) env))
	     false
	     (eval-and-exps (rest-exps exps) env)))))

;; or syntax procedures
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))

;; or eval procedure
(define (eval-or exp env)
  (eval-or-exps (or-exps exp) env))

(define (eval-or-exps exps env)
  (if (null? exps)
      false
      (let ((val (eval (firs-exp exps) env)))
	(if (true? val)
	    val
	    (eval-or-exps (rest-exps exps) env)))))

;; =============================================================================

;; and and or special forms as derived expressions
;;
;; evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotion exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval (and->if exp) env))
	((or? exp) (eval (or->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; conversion of and to if
(define (and->if exp)
  (expand-and-exps (and-exps exp)))

(define (expand-and-exps exps)
  (cond ((null? exps) 'true)
	((last-exp? exps) (first-exp exps))
	(else (make-if (first-exp exps)
		       (expand-and-exps (rest-exps exps))
		       'false))))

;; conversion of or to if
(define (or->if exp)
  (expand-or-exps (or-exps exp)))

(define (expand-or-exps exps)
  (cond ((null? exps) 'false)
	((last-exp? exps) (first-exp exps))
	(else (make-if (first-exp exps)
		       (first-exp exps)
		       (expand-or-exps (rest-exps exps))))))
