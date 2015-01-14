;;; Procedural representation of pairs
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Only 0 or 1 is allowed as argument -- CONS" m))))
  dispatch)

(define (car z)
  (z 0))

(define (cdr z)
  (z 1))

;;; Procedural representation of pairs
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

