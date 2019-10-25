; Exercise 2.5: Show that we can represent pairs of nonnegative integers using 
; only numbers and arithmetic operations if we represent the pair a and b as the 
; integer that is the product 2^a3^b . Give the corresponding definitions of the 
; procedures cons, car, and cdr.
(load "../common.scm")
(define (cons a b)
    (define (pow x y)
        (if (= y 0)
            1
            (* x (pow x (- y 1)))))
    (* (pow 2 a) (pow 3 b)))

(define (car x)
    (define (extract2 a result)
        (if (not (even? a))
            result
            (extract2 (/ a 2) (+ result 1))
            ))
    (extract2 x 0))

(define (cdr x)
   (define (extract3 a result)
        (if (not (divides? 3 a))
            result
            (extract3 (/ a 3) (+ result 1))
            ))    
    (extract3 x 0))

(cons 2 3)

(car 108)

(cdr 108)

(car 72)

(cdr 72)

(car 1)

(cdr 1)