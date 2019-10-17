(load "../common.scm")

(define (compose f g)
    (lambda (x) (f (g x))))

; (define (inc x)
;     (+ x 1))

(define (repeated f n)
        (if (= n 1) 
            f
            (compose f (repeated f (- n 1)))
        ))

((repeated inc 9) 8)

((repeated square 2) 5)

((repeated square 3) 2)