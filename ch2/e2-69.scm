; Exercise 2.69.
;
; The following procedure takes as its argument a list of
; symbol-frequency pairs (where no symbol appears in more than one pair) and
; generates a Huffman encoding tree according to the Huffman algorithm.

; (define (generate-huffman-tree pairs)
;   (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms the list of pairs
; into an ordered set of leaves. Successive-merge is the procedure you must
; write, using make-code-tree to successively merge the smallest-weight
; elements of the set until there is only one element left, which is the
; desired Huffman tree. (This procedure is slightly tricky, but not really
; complicated. If you find yourself designing a complex procedure, then you are
; almost certainly doing something wrong. You can take significant advantage of
; the fact that we are using an ordered set representation.)
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.4-huffman-tree.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; If there is only one object in the set, then the object is a leaf or a finished merged tree.
; Otherwise, since the set is ordered, we choose the first two objects make a huffman-tree for
; them and than add it to the rest objects in the set to form a new ordered set. Recursively
; do this, than we can get the complete huffman-tree. Slightly tricky indeed, and I think this
; is the best solution I can give.
(define (successive-merge set)
    (if (null? set)
        '()
        (if (null? (cdr set))
            (car set)
            (let ((tree (make-code-tree (car set) (cadr set))))
                (successive-merge (adjoin-set tree (cddr set)))))))

(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))