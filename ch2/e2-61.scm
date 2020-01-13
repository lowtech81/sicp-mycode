; Exercise 2.61.
;
; Give an implementation of adjoin-set using the ordered
; representation. By analogy with element-of-set? show how to take
; advantage of the ordering to produce a procedure that requires on the
; average about half as many steps as with the unordered representation.
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.3-ordered-set.scm")

(define (adjoin-set x set)
    (cond ((null? set) (cons x set))
          ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
          ((= x (car set)) set)
          (else (cons x set))))

(adjoin-set 3 '())

(adjoin-set 3 '(1 2 3 4 5))

(adjoin-set 6 '(1 2 3 4 5))
