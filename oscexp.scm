(define (c^2 x)
  (let ((z (make-connector)))
    (squarer x z)
    z))

(define (csqrt x)
  (let ((z (make-connector)))
    (squarer z x)
    z))

(define (tank-circuit-calculator x y)
  (c/ (cv 1)
      (c* (c^2 (c* (cv 6.28318530718)
		   x))
	  y)))

(define f (make-connector))
(define C (make-connector))
(define L (tank-circuit-calculator f C))

(probe "f Hz" f)
(probe "L Henries" L)
(probe "C Farads" C)
