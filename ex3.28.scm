(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (= s1 0) (= s2 1)) 1)
	((and (= s1 1) (= s2 0)) 1)
	((and (= s1 1) (= s2 1)) 1)
	(else (error "Invalid signal" (list s1 s2)))))

(define or-gate-delay 5)

