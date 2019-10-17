;implementing smooth function which is avergrage of f(x-dx),f(x) and f(x+dx)
; and n-fold-smooth for use smooth repeatedly for n times.

(load "../common.scm")

(define dx 0.00001)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
        (if (= n 1) 
            f
            (compose f (repeated f (- n 1)))))

(define (smooth f)
    (lambda (x) (average-of-3 
                    (f (- x dx)) 
                    (f x) 
                    (f (+ x dx)))))

((smooth square) 2)

(define (n-fold-smooth f n)
    ((repeated smooth n) f))

((n-fold-smooth square 3) 2)