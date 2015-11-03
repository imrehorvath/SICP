(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (c^2 x)
  (let ((z (make-connector)))
    (squarer x z)
    z))

(define (csqrt x)
  (let ((z (make-connector)))
    (squarer z x)
    z))

;;
;; L = 1/[(2 * PI * f)^2 * C]
;;
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
