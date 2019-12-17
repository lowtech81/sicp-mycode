(load "../common.scm")

(define (product_minus_sum n)
    (let ((digits (extract n)))
    (- (accumulate * 1 digits) (accumulate + 0 digits))))

(define (extract n)
    (if (< n 10)
        (list n)
        (cons (remainder n 10) (extract (/ (- n (remainder n 10)) 10)))))

(product_minus_sum 234)

(product_minus_sum 4421)