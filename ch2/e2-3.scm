; Exercise 2.3: Implement a representation for rectangles in a plane. 
; (Hint: You may want to make use of Exercise 2.2.) In terms of your 
; constructors and selectors, create procedures that compute the perimeter 
; and the area of a given rectangle. Now implement a different representation 
; for rectangles. Can you design your system with suitable abstraction barriers, 
; so that the same perimeter and area procedures will work using either representation?
(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (make-segment a b)
    (cons a b))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (len segment)
    (distance (start-segment segment) (end-segment segment)))

(define (distance pa pb)
    (sqrt (+ (square (- (x-point pa) (x-point pb))) (square (- (y-point pa) (y-point pb))))))

; represent rect with two adjacent edge
(define (make-rect1 edge-a edge-b)
    (cons edge-a edge-b))

(define (edge-a rect)
    (car rect))

(define (edge-b rect)
    (cdr rect))

(define (perimeter rect)
    (* (+ (len (edge-a rect)) (len (edge-b rect))) 2))

(define (area rect)
    (* (len (edge-a rect)) (len (edge-b rect))))

(define rect1 (make-rect1 (make-segment (make-point 0 0) (make-point 0 3)) (make-segment (make-point 0 0) (make-point 4 0))))

(perimeter rect1)

(area rect1)

;represent rect with diagonals 
(define (make-rect2 diagonal-a diagonal-b)
    (make-rect1 (make-segment (start-segment diagonal-a) (start-segment diagonal-b))
                (make-segment (start-segment diagonal-a) (end-segment diagonal-b))))

(define rect2 (make-rect2 (make-segment (make-point 0 0)(make-point 4 3)) (make-segment (make-point 0 3) (make-point 4 0))))

(perimeter rect2)

(area rect2)
