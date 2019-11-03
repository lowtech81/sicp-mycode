; Exercise 2.11: In passing, Ben also cryptically comments: 
; “By testing the signs of the endpoints of the intervals, it is 
; possible to break mul-interval into nine cases, only one of which 
; requires more than two multiplications.” Rewrite this procedure 
; using Ben’s suggestion. After debugging her program, Alyssa shows 
; it to a potential user, who complains that her program solves the 
; wrong problem. He wants a program that can deal with numbers 
; represented as a center value and an additive tolerance; 
; for example, he wants to work with intervals such as 3.5±0.15 
; rather than [3.35, 3.65]. Alyssa returns to her desk and fixes 
; this problem by supplying an alternate constructor and alternate selectors:
; (define (make-center-width c w) 
;     (make-interval (- c w) (+ c w))) 
; (define (center i) 
;     (/ (+ (lower-bound i) (upper-bound i)) 2))
; (define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
; Unfortunately, most of Alyssa’s users are engineers. 
; Real engineering situations usually involve measurements with 
; only a small uncertainty, measured as the ratio of the width of 
; the interval to the midpoint of the interval. Engineers usually 
; specify percentage tolerances on the parameters of devices, as 
; in the resistor specifications given earlier.

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

; (define (mul-interval x y)
;     (let ((p1 (* (lower-bound x) (lower-bound y)))
;           (p2 (* (lower-bound x) (upper-bound y)))
;           (p3 (* (upper-bound x) (lower-bound y)))
;           (p4 (* (upper-bound x) (upper-bound y))))
;         (make-interval (min p1 p2 p3 p4)
;                        (max p2 p2 p3 p4))))

; (define (div-interval x y)
;     (mul-interval 
;     x
;     (make-interval (/ 1.0 (upper-bound y))
;                    (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

;define width
(define (width interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2))

; redefine div-interval that signals an error if divide by
; an interval that spans zero
(define (div-interval x y)
    (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
        (error "divide by an interval that spans zero" y)
        (mul-interval 
            x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))

(define x (make-interval 3 4))

;rewrite mul-interval by testing the sign of the endpoints
;and ensure that only one case in nine requires more than two 
;multiplicaitons
(define (mul-interval x y)
    (let ((lx (lower-bound x))
          (ux (upper-bound x))
          (ly (lower-bound y))
          (uy (upper-bound y)))
         (cond  ((and (> lx 0) (> ux 0) (> ly 0) (> uy 0)) 
                    (make-interval (* ux uy) (* lx ly)))
                ((and (> lx 0) (> ux 0) (< ly 0) (> uy 0)) 
                    (make-interval (* ux uy) (* ux ly)))
                ((and (> lx 0) (> ux 0) (< ly 0) (< uy 0)) 
                    (make-interval (* ux ly) (* lx uy)))
                ((and (< lx 0) (> ux 0) (> ly 0) (> uy 0)) 
                    (make-interval (* ux uy) (* lx uy)))
                ((and (< lx 0) (> ux 0) (< ly 0) (< uy 0)) 
                    (make-interval (* ux ly) (* ux ly)))
                ((and (< lx 0) (< ux 0) (> ly 0) (> uy 0)) 
                    (make-interval (* ux ly) (* lx uy)))
                ((and (< lx 0) (< ux 0) (< ly 0) (> uy 0)) 
                    (make-interval (* lx uy) (* lx ly)))
                ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0)) 
                    (make-interval (* ux uy) (* lx ly)))
                ((and (< lx 0) (> ux 0) (< ly 0) (> uy 0)) 
                    (make-interval (min (* lx uy) (* ux ly))
                                   (max (* ux uy) (* lx ly)))))))

(mul-interval (make-interval 1 -3) (make-interval 5 -2)) 