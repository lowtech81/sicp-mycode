; Exercise 2.17: Define a procedure last-pair that returns the list 
; that contains only the last element of a given (nonempty) list:
; (last-pair (list 23 72 149 34)) 
; (34)

; give the (n-1)th element in the list 
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

; give the length of list
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)

; concat two lists
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
    
(append squares odds)
(append odds squares)

; last-pair
(define (last-pair items)
    (let ((last-index (- (length items) 1)))
        (list (list-ref items last-index))))
(last-pair (list 23 72 149 34)) 
(last-pair squares)
