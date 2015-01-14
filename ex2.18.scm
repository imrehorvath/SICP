(define (reverse lst)
  (define (reverse-iter src dst)
    (if (null? src)
	dst
	(reverse-iter (cdr src) (cons (car src) dst))))
  (reverse-iter lst '()))

