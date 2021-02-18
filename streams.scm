;; 

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())
(define stream-null? null?)

;; 

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
	((pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(else (stream-filter pred (stream-cdr s)))))

;; 

(define (integers-starting-from n)
  (cons-stream n
	       (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (fib-gen a b)
  (cons-stream a
	       (fib-gen b (+ a b))))

(define fibs (fib-gen 0 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;; 

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)
