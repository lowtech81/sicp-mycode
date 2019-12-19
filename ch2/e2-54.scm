; Exercise 2.54.
;
; Two lists are said to be equal? if they contain equal elements
; arranged in the same order. For example,

; (equal? '(this is a list) '(this is a list))

; is true, but

; (equal? '(this is a list) '(this (is a) list))

; is false. To be more precise, we can define equal? recursively in
; terms of the basic eq? equality of symbols by saying that a and b are
; equal? if they are both symbols and the symbols are eq?, or if they
; are both lists such that (car a) is equal? to (car b) and (cdr a) is
; equal? to (cdr b). Using this idea, implement equal? as a procedure.
; ------------------------------------------------------------

(define (equal? list1 list2)
    (cond ((and (null? list1) (null? list2)) #t)
          ((and (not (null? list1)) (not (null? list2)) (eq? (car list1) (car list2)))
                (equal? (cdr list1) (cdr list2)))
          (else #f)))

;; test
(equal? '(this is a list) '(this (is a) list))
(equal? '(this is a list) '(this is a list))
(equal? '() '())
(equal? '() '(this is a list))

