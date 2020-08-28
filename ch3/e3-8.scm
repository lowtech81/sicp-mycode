; Exercise 3.8.
;
; When we defined the evaluation model in section 1.1.3, we said
; that the first step in evaluating an expression is to evaluate its
; subexpressions. But we never specified the order in which the subexpressions
; should be evaluated (e.g., left to right or right to left). When we introduce
; assignment, the order in which the arguments to a procedure are evaluated can
; make a difference to the result. Define a simple procedure f such that
; evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated
; from left to right but will return 1 if the arguments are evaluated from
; right to left.
; ------------------------------------------------------------
; TODO: Seems Wrong
;  (define f 
;    (let ((count 1)) 
;      (lambda (x)  
;         (set! count (* count x)) 
;         count))) 


 (define f (let ((second-last-call 0) 
                 (last-call 0)) 
             (lambda (n) 
               (set! second-last-call last-call) 
               (set! last-call n) 
               second-last-call))) 
; test code

(+ (f 0) (f 1))

(+ (f 1) (f 0))

(+ (f 0) (f 1))

(+ (f 1) (f 0))