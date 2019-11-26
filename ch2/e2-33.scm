; Exercise 2.33.
;
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:

; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))
; (define (append seq1 seq2)
;   (accumulate cons <??> <??>))
; (define (length sequence)
;   (accumulate <??> 0 sequence))
; ----------------------------------------

(load "../common.scm")

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y))  nil sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(map square (list 1 2 3 4 5))

(append (list 1 2 3) (list 4 5))

(length (list 1 2 3 4 5 6))