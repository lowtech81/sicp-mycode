(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (product term a next b)
    (accumulate * 1 term a next b))

;below is test code for sum procedure.
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
    (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
    (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
    (define (pi-term x)
       (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4)) 
    (sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)

(integral cube 0 1 0.0001)

(integral square 0 1 0.001)

;below is test code for product
(define (factorial a b)
    (product identity a inc b))

(factorial 1 0)

(factorial 3 3)

(factorial 1 5)

(define (pi n)
    (define (pi-term x)
        (/ (* (- x 1)(+ x 1)) (square x)))
    (define (pi-next x) (+ x 2))
    (* 4.0 (product pi-term 3 pi-next (+ (* n 2) 1)))) 

(pi 100)

(pi 1000)

