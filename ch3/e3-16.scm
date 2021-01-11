; Exercise 3.16.
;
; Ben Bitdiddle decides to write a procedure to count
; the number of pairs in any list structure. ``It's easy,'' he reasons.
; ``The number of pairs in any structure is the number in the car plus
; the number in the cdr plus one more to count the current pair.'' So
; Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Show that this procedure is not correct. In particular, draw
; box-and-pointer diagrams representing list structures made up of
; exactly three pairs for which Ben's procedure would return 3; return
; 4; return 7; never return at all.
; ------------------------------------------------------------

(define count3 (list 'a 'b 'c))

(count-pairs count3)

(define pair1 (cons 'a 'b ))

(define pair2 (cons 'a 'b ))

(define count4 (cons pair1 pair2))

(set-car! pair2 pair1)

(count-pairs count4)

(define pair1 (cons 'a 'b))

(define pair2 (cons pair1 pair1))

(define count7 (cons pair2 pair2))

(count-pairs count7)

(define never-return (list 'a 'b 'c))

(set-cdr! never-return never-return)

(count-pairs never-return)
