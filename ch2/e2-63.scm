; Exercise 2.63.
;
; Each of the following two procedures converts a binary
; tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; a. Do the two procedures produce the same result for every tree? If
; not, how do the results differ? What lists do the two procedures
; produce for the trees in figure 2.16?

; b. Do the two procedures have the same order of growth in the number
; of steps required to convert a balanced tree with n elements to a
; list? If not, which one grows more slowly?
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.3-binary-trees.scm")

; Answer of a: Yes. The two produce the same result for every tree. 
(define tree1 (make-tree 7 (make-tree 3 
                                      (make-tree 1 '() '())
                                      (make-tree 5 '() '()))
                           (make-tree 9
                                      '()
                                      (make-tree 11 '() '()))))
(tree->list-1 tree1)

(tree->list-2 tree1)

(define tree2 (make-tree 3 (make-tree 1 '() '()) 
                           (make-tree 7 
                                      (make-tree 5 '() '())
                                      (make-tree 9
                                                 '()
                                                 (make-tree 11 '() '())))))

(tree->list-1 tree2)

(tree->list-2 tree2)

(define tree3 (make-tree 5 (make-tree 3
                                      (make-tree 1 '() '())
                                      '())
                           (make-tree 9
                                      (make-tree 7 '() '())
                                      (make-tree 11 '() '()))))

(tree->list-1 tree3)

(tree->list-2 tree3)

; Answer of b:
; Let T(n) be the time taken by the procedure for a balanced tree with n nodes.

; For tree->list-1: 
; T(n) = 2*T(n/2) + O(n/2) (as the procedure append takes linear time)
; Solving above equation, we get T(n) = O(n * log n)

; For tree->list-2:
; T(n) = 2*T(n/2) + O(1)
; Solving the above equation, we get T(n) = O(n)


