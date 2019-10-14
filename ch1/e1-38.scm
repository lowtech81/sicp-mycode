 (define (cont-frac n d k) 
   (define (frac-rec i) 
     (/ (n i) 
        (+ (d i) 
           (if (= i k) 
               0 
               (frac-rec (+ i 1)))))) 
   (frac-rec 1)) 

(define (e k)
    (+ 2
       (cont-frac (lambda (i) 1.0)
                  (lambda (i) 
                  (cond ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))
                        (else 1)))
                  k)))

(e 10)

(e 100)

(e 1000)