; Exercise 2.41.  
;
; Write a procedure to find all ordered triples of distinct positive i,j, and k less than
; or equal to a given integer n that sum to a given integer s.
;----------------------------------------
(load "../common.scm")

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

; the permutations sample on the book
(define (permutations s)
    (if (null? s)
        (list nil)
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x s))))
                    s)))

(permutations (list 1 2 3))

; the find-triples procedure
(define (find-triples n s)
    (let ((seq (enumerate-interval 1 n)))
       (filter (lambda (item) (= s (accumulate + 0 item)))
               (flatmap (lambda (i) 
                            (flatmap (lambda (j)
                                        (map (lambda (k) (list i j k)) 
                                             (remove j (remove i seq))))
                                     (remove i seq)))
                        seq))))

(find-triples 6 9)