(define (product a b term next)
    (if (> a b)
        1
        (* (term a) (product (next a) b term next))))      

(define (factorial a b)
    (define (inc n) (+ n 1))
    (define (identity x) x)
    (product a b identity inc))

(factorial 1 0)

(factorial 3 3)

(factorial 1 5)

(define (pi n)
    (define (pi-term x)
        (/ (* (- x 1)(+ x 1)) (square x)))
    (define (pi-next x) (+ x 2))
    (* 4.0 (product 3 (+ (* n 2) 1) pi-term pi-next)))

(pi 100)

(pi 1000)