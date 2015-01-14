(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;; PI/8 = 1/(1*3) + 1/(5*7) + 1/(9*11) + ...
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;;          (<name> (<next> a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; Int f[a-b] = [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2) + ...]dx
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
