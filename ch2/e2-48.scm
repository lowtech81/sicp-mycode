; Exercise 2.48.  A directed line segment in the plane can be
; represented as a pair of vectors -- the vector running from the origin
; to the start-point of the segment, and the vector running from the
; origin to the end-point of the segment. Use your vector representation
; from exercise 2.46 to define a representation for segments with a
; constructor make-segment and selectors start-segment and end-segment.
; --------------------------------------------------

(load "e2-46.scm")

(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))