((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
	  1
	  (* k (ft ft (- k 1)))))))
 5)

;;; a.
((lambda (n)
   ((lambda (fibo)
      (fibo fibo n))
    (lambda (fo k)
      (if (< k 2)
	  1
	  (+ (fo fo (- k 1))
	     (fo fo (- k 2)))))))
 5)

;;; b.
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))


(letrec ((fact
	  (lambda (n)
	    (if (= n 1)
		1
		(* n (fact (- n 1)))))))
  (fact 10))
