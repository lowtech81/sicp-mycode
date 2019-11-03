; Exercise 2.8: Using reasoning analogous to Alyssa’s, describe 
; how the difference of two intervals may be computed. Define a 
; corresponding subtraction procedure, called sub-interval.

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
    (max (car interval) (cdr interval))
    )

(define (lower-bound interval)
    (min (car interval) (cdr interval))
    )

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p2 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval 
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))


(define x (make-interval 3 -2))

(define y (make-interval -1 5))

(sub-interval x y)