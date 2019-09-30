(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

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