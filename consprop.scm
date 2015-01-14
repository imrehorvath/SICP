(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'connector-has-value)
	   (process-new-value))
	  ((eq? request 'connector-lost-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? product) (has-value? m1))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? product) (has-value? m2))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'connector-has-value)
	   (process-new-value))
	  ((eq? request 'connector-lost-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'connector-has-value)
	   (process-new-value))
	  ((eq? request 'connector-lost-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'connector-has-value)
	   (process-new-value))
	  ((eq? request 'connector-lost-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'connector-has-value))

(define (inform-about-no-value constraint)
  (constraint 'connector-lost-value))

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-connector-value newval requestor)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant requestor)
	     (for-each-except requestor
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-connector-value requestor)
      (cond ((eq? requestor informant)
	     (set! informant #f)
	     (for-each-except requestor
			      inform-about-no-value
			      constraints))
	    (else 'ignored)))
    (define (connect constraint)
      (if (not (memq constraint constraints))
	  (set! constraints
		(cons constraint constraints)))
      (if (has-value? me)
	  (inform-about-value constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-connector-value)
	    ((eq? request 'forget) forget-connector-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation -- CONNECTOR"
			 request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value requestor)
  ((connector 'set-value!) new-value requestor))

(define (forget-value! connector requestor)
  ((connector 'forget) requestor))

(define (connect connector constraint)
  ((connector 'connect) constraint))

(define (for-each-except except proc lst)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) except) (loop (cdr items)))
	  (else (proc (car items))
		(loop (cdr items)))))
  (loop lst))