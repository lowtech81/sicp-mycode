(define (fast-expt-iter b n)
    (define (iter a b counter)
        (if (= counter 0) 
            a
           (cond ((even? counter) (iter a (square b) (/ counter 2)))
                   (else (iter (* a b) b (- counter 1))))
        ))
    (iter 1 b n)    
    )

(define (even? n)
    (= (remainder n 2) 0))

(fast-expt-iter 2 32)