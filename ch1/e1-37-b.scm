(define (cont-frac n d k)
    (define (iter result i)
        ; (newline)
        ; (display i)
        ; (newline)
        ; (display result)
        (if (< i 0)
            ;(/ (n i) (+ (d i) result)) 
            result
            (iter (/ (n i) (+ (d i) result)) 
                  (- i 1))))
    (iter 0 k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)