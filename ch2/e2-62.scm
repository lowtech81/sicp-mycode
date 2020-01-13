; Exercise 2.62.
;
; Give a O(n) implementation of union-set for sets
; represented as ordered lists.
; ------------------------------------------------------------
(load "2.3.3-ordered-set.scm")

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x1 (car set1)) (x2 (car set2)))
                     (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                           ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                           ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))))))) 

(union-set '() '())
(union-set '() '(1 2))
(union-set '(1 2) '())
(union-set '(1 3 5) '(2 4 6))