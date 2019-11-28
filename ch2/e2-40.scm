; Exercise 2.40.  
;
; Define a procedure unique-pairs that, given an integer n, generates the 
; sequence of pairs (i,j) with 1< j< i< n. Use unique-pairs to simplify 
; the definition of prime-sum-pairs given above.
; ----------------------------------------------

(load "../common.scm")
(load "../ch1/ch1-smallest-divisor.scm") ;for procedure prime?

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (flatmap
                                (lambda (i)
                                    (map (lambda (j) (list i j))
                                            (enumerate-interval 1 (- i 1))))
                                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

; simplify the definition of prime-sum-pairs given above
(define (unique-pairs n)
        (flatmap 
            (lambda (i) 
                (map (lambda (j) (list i j))
                    (enumerate-interval 2 (- i 1))))
                (enumerate-interval 2 (- n 1))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
