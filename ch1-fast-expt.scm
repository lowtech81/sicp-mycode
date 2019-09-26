(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (even? n)
    (= (remainder n 2) 0))

(fast-expt 2 16)
(fast-expt 2 32)