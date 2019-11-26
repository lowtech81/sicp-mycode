; Exercise 2.39.
;
; Complete the following definitions of reverse
; (exercise 2.18) in terms of fold-right and fold-left from exercise 2.38:

; (define (reverse sequence)
;   (fold-right (lambda (x y) <??>) nil sequence))
; (define (reverse sequence)
;   (fold-left (lambda (x y) <??>) nil sequence))
;
; ---------------------------------------

(load "../common.scm")
(load "e2-38.scm")

(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse (list 1 2 3 4 5))

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse (list 1 2 3 4 5))