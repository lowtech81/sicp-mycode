(define (square x)
    (* x x)
)

(define (f a b c) 
(define (sum-square-larger-two a b c)
    (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
        ((and (< b a) (< b c)) (+ (square a) (square c)))
        ((and (= b a) (< b c)) (+ (square a) (square c)))
        (else (+ (square a) (square b))))
)
(sum-square-larger-two a b c)
)

(f 1 2 3)

(f 5 4 -6)