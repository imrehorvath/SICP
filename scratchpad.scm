(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

(define (fib n)
  (let fib-iter ((a 1)
		 (b 0)
		 (count n))
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1)))))

(define (f x)
  (define (even? n)
    (if (= n 0)
	true
	(odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
	false
	(even? (- n 1))))
  (even? x))
