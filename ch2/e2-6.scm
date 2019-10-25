; Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling enough, 
; consider that, in a language that can manipulate procedures, we can get by without 
; numbers (at least insofar as nonnegative integers are concerned) by implementing 0 
; and the operation of adding 1 as 
; (define zero (lambda (f) (lambda (x) x)))
;     (define (add-1 n) 
;         (lambda (f) (lambda (x) (f ((n f) x)))))
; This representation is known as Church numerals, after its inventor, Alonzo Church,
; the logician who invented the λ-calculus. Define one and two directly (not in 
; terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give 
; a direct definition of the addition procedure + (not in terms of repeated application
; of add-1).

(load "../common.scm")

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))


; BEGIN:for understanding fucking church numberals
((zero square) 2) ; 2
((zero sqrt) 2) ; 2
((zero 1+) 2) ;do 1+ zero time, 2
(((add-1 zero) 1+) 2) ; do 1+ once, result is 3
(((add-1 zero) square) 2) ; do square once , result is 4
(((add-1 (add-1 zero)) 1+) 2) ; do 1+ twice, result is 4
(((add-1 (add-1 zero)) square) 2) ; do square twice, result is 16
; END

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

((one 1+) 2)
((two 1+) 2)
((one square) 2)
((two square) 2)

(define (+ n1 n2)
    (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))

(define three (+ one two))
((three square) 2)
((three 1+) 2)