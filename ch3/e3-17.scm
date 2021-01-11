; Exercise 3.17.
;
; Devise a correct version of the count-pairs procedure of
; exercise 3.16 that returns the number of distinct pairs in any structure.
; (Hint: Traverse the structure, maintaining an auxiliary data structure that
; is used to keep track of which pairs have already been counted.)
; ------------------------------------------------------------

; from http://community.schemewiki.org/?sicp-ex-3.17 
 (define (count-pairs x) 
   (let ((encountered '())) 
     (define (helper x) 
       (if (or (not (pair? x)) (memq x encountered)) 
         0 
         (begin 
           (set! encountered (cons x encountered)) 
           (+ (helper (car x)) 
              (helper (cdr x)) 
              1)))) 
   (helper x))) 

#|  (define (count-pairs x) 
   (let ((counted '())) 
     (define (uncounted? x) 
       (if (memq x counted) 
           0 
           (begin 
             (set! counted (cons x counted)) 
             1))) 
      
     (define (count x) 
       (if (not (pair? x)) 
           0 
           (+ (count (car x)) 
              (count (cdr x)) 
              (uncounted? x)))) 
     (count x))) 
 |#


(define count3 (list 'a 'b 'c))

(count-pairs count3)

(define pair1 (cons 'a 'b ))

(define pair2 (cons 'a 'b ))

(define count4 (cons pair1 pair2))

(set-car! pair2 pair1)

(count-pairs count4)

(define pair1 (cons 'a 'b))

(define pair2 (cons pair1 pair1))

(define count7 (cons pair2 pair2))

(count-pairs count7)

(define never-return (list 'a 'b 'c))

(set-cdr! never-return never-return)

(count-pairs never-return)