(define (f-recur n)
    (if (< n 3) 
        n
        (+ (f-recur (- n 1))
           (* 2 (f-recur (- n 2)))
           (* 3 (f-recur (- n 3))))))
           
(define (f-iter n)
    (define (iter a b c counter)
        (cond ((< n 3) n)
              ((= counter n) c)
              (else  
                (iter b c (+ (* 3 a)
                         (* 2 b)
                         c) (+ 1 counter)))))
        (iter 0 1 2 2))