(define (or-gate o1 o2 output)
  (let ((a (make-wire))
	(b (make-wire))
	(c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (and-gate a b c)
    (inverter c output)
    'ok))

;;; delay is 7
