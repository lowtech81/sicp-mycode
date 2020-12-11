; Exercise 3.13.  Consider the following make-cycle procedure, which
; uses the last-pair procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (list 'a 'b 'c)))

; What happens if we try to compute (last-pair z)?
; ------------------------------------------------------------

;  ,-------------------,
;  |                   |
;  v                   |
; ( . ) -> ( . ) -> ( . )
;  |        |        |
;  v        v        v
;  'a       'b       'c

procedure make-cycle will make a circular list. 

tring to compute (last-pair z) will cause infinite recursion. 