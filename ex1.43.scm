(define (identity x) x)

(define (repeated f n)
  (compose f (if (> n 1)
		 (repeated f (- n 1))
		 identity)))
