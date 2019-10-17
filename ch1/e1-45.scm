(load "../common.scm")

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (average-dump f)
    (lambda (x) (average x (f x))))

(define (sqrt x)
    (fixed-point (average-dump (lambda (y) (/ x y)))
                 1.0))

(sqrt 2)

(define (cube-root x)
    (fixed-point (average-dump (lambda (y) (/ x (square y))))
                 1.0))

(cube-root 5)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
        (if (= n 1) 
            f
            (compose f (repeated f (- n 1)))))

(define (power x n)
    (if (= 0 n)
        1
        (* x (power x (- n 1)))))

; 'n'th root of 'x' on repeated average-dump 'r' times
(define (root x n r) 
    (fixed-point 
        ((repeated 
            average-dump r) (lambda (y) (/ x (power y (- n 1)))))
        1.0))

(root 2 2 1)

(root 5 3 1)

(root 16 4 2) ; converge with twice average-dump 

(root 32 5 2) ; converge with twice average-dump 

(root 64 6 1)


