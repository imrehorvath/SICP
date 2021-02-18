(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (filter pred items)
  (cond ((null? items) '())
	((pred (car items))
	 (cons (car items)
	       (filter pred (cdr items))))
	(else (filter pred (cdr items)))))

(define (fold-right op init items)
  (if (null? items)
      init
      (op (car items)
	  (fold-right op init (cdr items)))))

(define (enumerate from to)
  (if (> from to)
      '()
      (cons from
	    (enumerate (1+ from) to))))

(define (separate pred items)
  (if (null? items)
      (list '() '())
      (let ((rest (separate pred (cdr items))))
	(if (pred (car items))
	    (list (cons (car items) (car rest))
		  (cadr rest))
	    (list (car rest)
		  (cons (car items) (cadr rest)))))))

(define (split n items)
  (if (zero? n)
      (list '() items)
      (let ((rest (split (- n 1) (cdr items))))
	(list (cons (car items) (car rest))
	      (cadr rest)))))

(define (square n) (* n n))
(define (1+ n) (+ 1 n))
