(define (square x)
    (* x x)
)

(define (sum-of-square x y)
    (+ (square x) (square y))
)

(define (larger a b)
    (if (> a b) a b)
)

(define (largest a b c)
    (larger a (larger b c))
)

(define (middle a b c)
    (cond ((= a (largest a b c)) (larger b c))
          ((= b (largest a b c)) (larger a c))
          ((= c (largest a b c)) (larger a b))
    )
)

(define (sum-square-larger-two x y z)
    (sum-of-square (largest x y z) (middle x y z))
)

(define (f x y z) (sum-square-larger-two x y z)
    )

(f 1 2 3)

(f 5 4 -6)
