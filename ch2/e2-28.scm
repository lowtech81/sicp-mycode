; Exercise 2.28.
; Write a procedure fringe that takes as argument a tree
; (represented as a list) and returns a list whose elements are
; all the leaves of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

; (fringe x) -> (1 2 3 4)

; (fringe (list x x)) -> (1 2 3 4 1 2 3 4)
; -----------------------------------------

(load "../common.scm")
(load "../helpers.scm")

(define (fringe items)
    (cond ((null? items) nil)
          ((not (pair? items)) (list items))
          (else (append (fringe (car items))
                        (fringe (cdr items))))))

(fringe x)

(fringe (list (list (list 1 2) (list  3 4)) (list 5 6)))