;;; Define the constraint network
(define (celsius-farenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9.0 w)
    (constant 5.0 x)
    (constant 32.0 y)
    'ok))

;;; Create external connectors
(define C (make-connector))
(define F (make-connector))

;;; Create a constraint network instance
;;; with the external connectors attached
(celsius-farenheit-converter C F)

;;; Attach probes
(probe "Celsius temp" C)
(probe "Farenheit temp" F)
