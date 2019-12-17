; Exercise 2.49.  Use segments->painter to define the following
; primitive painters:

; a.  The painter that draws the outline of the designated frame.

; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.

; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

; d.  The wave painter.
; --------------------------------------------------------

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list))

;; a
(define outline-painter
  (define outline-segment-list 
    (list
      (make-segment (make-vect 0 0) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 0 0))))
  (segments->painter outline-segment-list))

;; b
(define x-painter
  (define x-segment-list
    (list 
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))))
  (segments->painter x-segment-list))

;; c
(define diamond-painter
  (define diamond-segment-list
    (list 
      (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
  (segments->painter diamond-segment-list))