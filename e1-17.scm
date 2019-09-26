(define (multi a b)
    (cond ((= b 0) 0)
           ((even? b) (multi (double a) (halve b)))
           (else (+ a (multi a (- b 1))))))

(define (double n)
    (+ n n))

(define (halve n)
    (/ n 2))

(define (even? n)
    (= (remainder n 2) 0))

(multi 5 6)

(multi 8 1024)

(multi 90 0)