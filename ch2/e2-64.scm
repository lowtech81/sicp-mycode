; Exercise 2.64.  The following procedure list->tree converts an ordered list
; to a balanced binary tree. The helper procedure partial-tree takes as
; arguments an integer n and list of at least n elements and constructs a
; balanced tree containing the first n elements of the list. The result
; returned by partial-tree is a pair (formed with cons) whose car is the
; constructed tree and whose cdr is the list of elements not included in the
; tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining as clearly as you can how partial-tree
; works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; b. What is the order of growth in the number of steps required by list->tree
; to convert a list of n elements?
; ------------------------------------------------------------

;a.
;   -------------------- 1. partial-tree  n ------------------------------
;  |                                                                      |
;  |                                                                      |
;  |------------------- 2.partial-tree left-size -------------------------|
;  |                                                                      | 
;  |                                                                      |
;  |                                      - 3. partial-tree  right-size --
;  |                                     |                                |
;  |                                     |                                |
; e1 e2 e3 ... e[(n-1)/2] e[(n-1)/2 +1] e[(n-1)/2 +2]...... e[n] ... e[length]
;  |                 |       |               |                |
;   ---  left-size--        node              -- right-size --
;           |                                         |
;       left-tree                                right-tree
;
;
;           list->tree for (1 3 5 7 9 11):
;
;                       5
;                      / \
;                     1   9
;                      \  |\
;                       3 7 11
;


;b. The procedure `parital-tree` is called O(2^d) times with respect to the depth d of the calling tree. 
; The terms of the the size of the input n, the calling tree looks like:
;        n
;       / \
;    n/2   n/2
;    / \   / \
; n/4 n/4 n/4 n/4
; ...   ...   ...
; d=log n and there are O(2^d) nodes, and the cost is O(1) at each node. Therefore, the total cost of `partial-tree` is Θ(n).
; The `length` cost is also Θ(n), so the final cost is Θ(n).