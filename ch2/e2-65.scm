; Exercise 2.65.
;
; Use the results of exercises 2.63 and  2.64 to give O(n)
; implementations of union-set and intersection-set for sets implemented as
; (balanced) binary trees.
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.3-binary-trees.scm")
(load "e2-63.scm")
(load "e2-64.scm")

; uninon-set and intersection-set of ordered list representation, both costs are O(n)
(define (olist-union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x1 (car set1)) (x2 (car set2)))
                     (cond ((= x1 x2) (cons x1 (olist-union-set (cdr set1) (cdr set2))))
                           ((< x1 x2) (cons x1 (olist-union-set (cdr set1) set2)))
                           ((> x1 x2) (cons x2 (olist-union-set set1 (cdr set2))))))))) 

(define (olist-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (olist-intersection-set (cdr set1) set2))
              ((< x2 x1)
               (olist-intersection-set set1 (cdr set2)))))))

; the balanced binary trees edition union-set and intersection-set
(define (union-set set1 set2)
    (list->tree (olist-union-set (tree->list-1 set1) (tree->list-1 set2))))

(define (intersection-set set1 set2)
    (list->tree (olist-intersection-set (tree->list-1 set1) (tree->list-1 set2))))

;test
(union-set (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 12)))
(intersection-set (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 11)))