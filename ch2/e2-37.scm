; Exercise 2.37.
;
; Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences
; of vectors (the rows of the matrix). For example, the matrix:
;
; | 1 2 3 4 |
; | 4 5 6 6 |
; | 6 7 8 9 |


; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).
; With this representation, we can use sequence operations to concisely express the basic matrix
; and vector operations. These operations (which are described in any book on matrix algebra) are the following:
;
; (dot-product v w) ; returns the sum SUMi(vi*wi)
; (matrix-*-vector m v) ; returns vector t where ti = SUMj(mij*vj)
; (matrix-*-matrix m n) ; returns matrix p where pij = SUMk(mik*nkj)
; (transpose m) ; returns matrix n where nij = mji

; We can define the dot product as

; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

; (define (matrix-*-vector m v)
;   (map <??> m))
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map <??> m)))
;
; ------------------------------------------------------------------------------

(load "../common.scm")

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 2 2 2))

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(matrix-*-matrix (list (list 1 2) (list 3 4) (list 5 6)) (list (list 1 2 3) (list 4 5 6)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 1 2) (list 3 4) (list 5 6)))

(define mx (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(matrix-*-matrix mx (transpose mx))
(matrix-*-matrix (transpose mx) mx)