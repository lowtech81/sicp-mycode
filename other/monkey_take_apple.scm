(define (find-m n k)
    (define (fit monkey m)
        (let ((result (and (= (remainder m n) k) (>= (/ (- m k) n) 1))))
            (if (= monkey 1)
                result
                (and result (fit (- monkey 1) (- m k (/ (- m k) n)))))))
    (define (iter guess)
        (if (fit n guess)
            guess
            (iter (+ 1 guess))))
    (iter (+ n k)))

(find-m 2 1)

(find-m 3 1)

(find-m 5 1)

(find-m 5 2)

(find-m 8 3)
; (find-m 9 1)

