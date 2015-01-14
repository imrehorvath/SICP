(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque")
      (car (car (front-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque")
      (car (car (rear-ptr deque)))))

(define (front-insert-deque! deque item)
  (let ((new-pair1 (cons '() '()))
	(new-pair2 (cons '() '())))
    (set-car! new-pair2 item)
    (set-car! new-pair1 new-pair2)
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair1)
	   (set-rear-ptr! deque new-pair1))
	  (else
	   (set-cdr! new-pair1 (front-ptr deque))
	   (set-cdr! (car (front-ptr deque)) new-pair1)
	   (set-front-ptr! deque new-pair1)))
    'ok))

(define (rear-insert-deque! deque item)
  (let ((new-pair1 (cons '() '()))
	(new-pair2 (cons '() '())))
    (set-car! new-pair2 item)
    (set-car! new-pair1 new-pair2)
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair1)
	   (set-rear-ptr! deque new-pair1))
	  (else
	   (set-cdr! (rear-ptr deque) new-pair1)
	   (set-cdr! new-pair2 (rear-ptr deque))
	   (set-rear-ptr! deque new-pair1)))
    'ok))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-DELETE! called with an empty deque"))
	((one-element-deque? deque)
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '())
	 'ok)
	(else
	 (let ((next-ptr (cdr (front-ptr deque)))
	       (next-value-back-ptr-pair (cadr (front-ptr deque))))
	   (set-cdr! next-value-back-ptr-pair '())
	   (set-front-ptr! deque next-ptr)
	   'ok)))
  )

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-DELETE! called with an empty deque"))
	((one-element-deque? deque)
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '())
	 'ok)
	(else
	 (let ((prev-ptr (cdar (rear-ptr deque))))
	   (set-cdr! prev-ptr '())
	   (set-rear-ptr! deque prev-ptr)
	   'ok)))
  )

(define (one-element-deque? deque)
  (and (not (empty-deque? deque))
       (null? (cdr (front-ptr deque)))))

(define (print-deque deque)
  (define (loop item)
    (if (null? item)
	'done
	(begin
	  (display (caar item))
	  (loop (cdr item)))))
  (loop (front-ptr deque)))
