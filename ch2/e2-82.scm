; Exercise 2.82.
;
; Show how to generalize apply-generic to handle coercion in
; the general case of multiple arguments. One strategy is to attempt to coerce
; all the arguments to the type of the first argument, then to the type of the
; second argument, and so on. Give an example of a situation where this
; strategy (and likewise the two-argument version given above) is not
; sufficiently general. (Hint: Consider the case where there are some suitable
; mixed-type operations present in the table that will not be tried.)
; ------------------------------------------------------------

(load "e2-79.scm")


; newly added for put and get coercion

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))


; made me sick for long time, found a clear solution like this

(define (apply-generic op . args) 
  
     (define (type-tags args) 
         (map type-tag args)) 
  
     (define (try-coerce-to target) 
         (map (lambda (x) 
                 (let ((coercor (get-coercion (type-tag x) (type-tag target)))) 
                     (if coercor 
                         (coercor x) 
                         x))) 
              args)) 
      
     (define (iterate next) 
         (if (null? next)  
             (error "No coersion strategy for these types " (list op (type-tags args))) 
             (let ((coerced (try-coerce-to (car next)))) 
                 (let ((proc (get op (type-tags coerced)))) 
                     (if proc 
                         (apply proc (map contents coerced)) 
                         (iterate (cdr next))))))) 
  
     (let ((proc (get op (type-tags args)))) 
         (if proc 
             (apply proc (map contents args)) 
             (iterate args)))) 
  
 ; Situation where this is not sufficiently general: 
 ; types: A B C 
 ; registered op: (op some-A some-B some-B) 
 ; registered coercion: A->B C->B 
 ; Situation: Evaluating (apply-generic op A B C) will only try (op A B C), (op B B B) and fail  
 ; while we can just coerce C to B to evaluate (op A B B) instead 

;;;test code
(put-coercion 'number 'complex (lambda (n) (make-complex-from-real-imag n 0)))

(apply-generic 'add 4 5)

(get-coercion 'complex 'complex)

(apply-generic 'add 5 (make-complex-from-real-imag 3 4))