; Exercise 2.9: The width of an interval is half of the difference 
; between its upper and lower bounds. The width is a measure of the
; uof the number specified by the interval. For some arithmetic 
; operations the width of the result of combining two intervals is 
; a function only of the widths of the argument intervals, whereas 
; for others the width of the combination is not a function of the 
; widths of the argument intervals. Show that the width of the sum 
; (or difference) of two intervals is a function only of the widths 
; of the intervals being added (or subtracted). Give examples to 
; show that this is not true for multiplication or division.
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

;define width
(define (width interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2))


; samples below shows that width of the sum or difference of two intervals
; equals to the sum of the width of each one. This is not true for mul and div
(define x (make-interval 3 -2))

(define a (make-interval 6 1))

(width x)  

(width a) 

(define y (make-interval -1 5))

(define b (make-interval 2 8))

(width y) 

(width b) 

(add-interval x y) 

(width (add-interval x y)) 

(add-interval a b) 

(width (add-interval a b)) 

(sub-interval x y) 

(width (sub-interval x y)) 

(sub-interval a b)  

(width (sub-interval a b)) 

(mul-interval x y) 

(width (mul-interval x y)) 

(mul-interval a b)

(width (mul-interval a b))

(div-interval x y) 

(width (div-interval x y)) 

(div-interval a b)

(width (div-interval a b))

