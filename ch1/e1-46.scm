(load "../common.scm")

; define iterative-improve which takes two procedure 'good-enough' 'improve' as arguments
; these two procedure do what their names tell
(define (iterative-improve good-enough improve)
    (define (try guess) 
            (let ((next (improve guess)))
                (if (good-enough guess next)
                    next
                    (try next))))
    try)

; use iterative-improve to define sqrt
(define delta1 0.00001)

(define (sqrt x)
    ((iterative-improve
        (lambda (a b) (close-enough? a b delta1))
        (lambda (guess) (average guess (/ x guess)))) 1.0))

(sqrt 2)

; use iterative-improve to define fixed-point 
(define delta2 0.000005)

(define (fixed-point f first-guess)
    ((iterative-improve
        (lambda (a b) (close-enough? a b delta2))
        f) first-guess))

(fixed-point cos 1.0)