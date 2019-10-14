 (define (cont-frac n d k) 
   (define (frac-rec i) 
     (/ (n i) 
        (+ (d i) 
           (if (= i k) 
               0 
               (frac-rec (+ i 1)))))) 
   (frac-rec 1)) 

(define (tan-cf x k)
       (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
                  (lambda (i) (- (* 2.0 i) 1))
                  k))

(define pi 3.1415926)

(tan-cf (/ pi 4) 10) ;0.99999...

(tan-cf (/ pi 6) 10) ;0.57735...

(tan-cf 2.5 10) ;-0.74702229...