; Exercise 2.60.
;
; We specified that a set would be represented as a list
; with no duplicates. Now suppose we allow duplicates. For instance, the
; set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design
; procedures element-of-set?, adjoin-set, union-set, and
; intersection-set that operate on this representation. How does the
; efficiency of each compare with the corresponding procedure for the
; non-duplicate representation? Are there applications for which you
; would use this representation in preference to the non-duplicate one?
; ------------------------------------------------------------

(load "../common.scm")

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (cons x set))

(define (union-set set1 set2)
    (append set1 set2))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(define test-set '(2 1 1 2 3))
(element-of-set? 1 test-set)
(adjoin-set 1 test-set)
(union-set test-set test-set)
(intersection-set test-set '(3 1 1 2))

; adjoin and union will be more efficiency, element-of-set and intersection stay same as non-duplicate one. 