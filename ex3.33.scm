(define (averager a b c)
  (let ((x (make-connector))
	(y (make-connector)))
    (adder a b y)
    (multiplier c x y)
    (constant 2 x)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "A" a)
(probe "B" b)
(probe "C" c)
