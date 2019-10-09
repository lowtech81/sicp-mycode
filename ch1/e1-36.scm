(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (newline)
        (display guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (f)
    (fixed-point (lambda (x) (/ (log 1000) (log x)))
    2.0))

(define (f-avg-dump)
    (define (average a b) (/ (+ a b) 2))
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
    2.0))

(f)

(f-avg-dump)