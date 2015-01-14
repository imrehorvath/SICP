;; list-of-values with left to right evaluation
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
	(let ((rest-values (list-of-values (rest-operands exps) env)))
	  (cons first-value rest-values)))))

;; list-of-values with right to left evaluation
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
	(let ((first-value (eval (first-operand exps) env)))
	  (cons first-value rest-values)))))
