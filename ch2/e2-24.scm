; Exercise 2.24.
;
; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
; Give the result printed by the interpreter,
; the corresponding box-and-pointer structure,
; and the interpretation of this as a tree (as in figure 2.6).
; ------------------------------------------------------------------
(load "../common.scm")

(list 1 (list 2 (list 3 4)))
;   |.|.|->|.|/|
;    |      |      
;    1     |.|.|->|.|/|
;           |      |
;           2     |.|.|->|.|/|
;                  |      |
;                  3      4
;          
;            (1 (2 (3 4)))
;           /\ (2 (3 4))
;          1  /\ (3 4) 
;            2  /\
;              3  4

; (list 1 2 3 4) as below; box-and-pointer structure is very different from (list 1 (list 2 (list 3 4)))
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
;   |.|.|->|.|.|->|.|.|->|.|/|
;    |      |      |      |  
;    1      2      3      4