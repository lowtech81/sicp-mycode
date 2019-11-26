
; Exercise 2.35.
;
; Redefine count-leaves from section 2.2.2 as an accumulation:

; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))
; --------------------------------------------------------

(load "../common.scm")
(define (count-leaves t)
    (accumulate + 
                0 
                (map (lambda (sub-tree) 
                            (cond ((null? sub-tree) 0)
                                  ((not (pair? sub-tree)) 1)
                                  (else (count-leaves sub-tree)))) 
                            t)))

(count-leaves (list (list 1 2) (list 3 4) 5 6))

(count-leaves (list 1 2 (list 3 4 5) 4 5 (list 1 (list 3))))