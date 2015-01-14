(define (cond-clause-extended? clause)
  (and (= (length clause) 3)
       (eq? (cadr clause) '=>)))

(define (cond-recipient clause) (cddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((fst (car clauses))
	    (rst (cdr clauses)))
	(cond ((cond-else-clause? fst)
	       (if (null? rst)
		   (sequence->exp (cond-actions fst))
		   (error "ELSE clause isn't last -- COND->IF"
			  clauses)))
	      ((cond-clause-extended? fst)
	       (cons 'let ))
	      (else
	       (make-if (cond-predicate fst)
			(sequence->exp (cond-actions fst))
			(expand-clauses rst)))))))
