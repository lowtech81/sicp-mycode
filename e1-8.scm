(define (cubert-iter guess x)
    (if (good-enough? guess x)
        guess
        (cubert-iter (improve guess x) x)))
(define (improve guess x)
    (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (good-enough? guess x)
    (< (abs (/ (- (cube guess) x) x)) 0.001))

(define (cube x)
    (* x x x))

(define (cubert x)
    (cubert-iter 1.0 x))            

(cubert 1)

(cubert 0.000001)

(cubert -27)