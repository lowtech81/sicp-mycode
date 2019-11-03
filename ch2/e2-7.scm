; Exercise 2.7: Alyssa’s program is incomplete because she has not 
; specified the implementation of the interval abstraction. Here is
; a definition of the interval constructor: 
; (define (make-interval a b) (cons a b)) 
; Define selectors upper-bound and lower-bound to complete the implementation.

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

(define x (make-interval 3 -2))

(upper-bound x)

(lower-bound x)

(define y (make-interval -1 5))

(add-interval x y)

(mul-interval x y)

(div-interval x y)