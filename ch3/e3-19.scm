; Exercise 3.19.
;
; Redo exercise 3.18 using an algorithm that takes only a
; constant amount of space. (This requires a very clever idea.)
; ------------------------------------------------------------

 (define loop '(foo bar baz))  
 (set-cdr! (cddr loop) loop)  
 ;          ,-------------------,  
 ;          |                   |  
 ;          v                   |  
 ; str4 -> ( . ) -> ( . ) -> ( . )  
 ;          |        |        |  
 ;          v        v        v  
 ;         'foo     'bar     'baz 
  
 (define (has-loop? x) 
     (define (check slow fast) 
       (cond ((eq? slow fast) 
              #t) 
             ((or (null? (cdr fast)) (null? (cddr fast))) 
              #f) 
             (else 
              (check (cdr slow) (cddr fast))))) 
     (check x (cdr x))) 
  
 (has-loop? loop) 
  
 (has-loop? (list 'a 'b 'c)) 