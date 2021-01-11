; Exercise 3.18.
;
; Write a procedure that examines a list and
; determines whether it contains a cycle, that is, whether a
; program that tried to find the end of the list by taking
; successive cdrs would go into an infinite loop. Exercise 3.13
; constructed such lists.
; ------------------------------------------------------------

(load "e3-17.scm")

(define (cycle? l) 
  (let ((n (count-pairs l))) 
    (define (iter items i) 
      (cond ((not (pair? items)) #f) 
            ((> i n) #t) 
            (else (iter (cdr items) (+ i 1))))) 
    (iter l 0))) 

(cycle? count3)

(cycle? count4)

(cycle? count7)

(cycle? never-return)


