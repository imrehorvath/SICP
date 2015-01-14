;;; Define the constraint network
(define (oscillator-calculator hz h f)
  (let ((x (make-connector))
	(y (make-connector))
	(z (make-connector))
	(u (make-connector))
	(v (make-connector)))
    (multiplier hz y x)
    (multiplier z u y)
    (squarer u v)
    (multiplier h f v)
    (constant 1 x)
    (constant 6.28318530718 z)
    'ok))

;;; Create external connectors
(define Hz (make-connector))
(define H (make-connector))
(define F (make-connector))

;;; Create a constraint nerwork instance
;;; with the external connectors attached
(oscillator-calculator Hz H F)

;;; Attach probes
(probe "Frequency in Hz" Hz)
(probe "Inductivity in H" H)
(probe "Capacity in F" F)
