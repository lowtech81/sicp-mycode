; You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms 
; to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition.
; The resulting filtered-accumulate abstraction takes the same arguments as accumulate, to-gether with an additional predicate 
; of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using 
; filtered accumulate:
;a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

;define (fiterd-accumate ...)
(define (filtered-accumulate filter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) 
                (if (filter a) 
                (combiner (term a) result)
                result))))
    (iter a null-value))

;define (prime? n)
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))          

(define (prime? n)(= (smallest-divisor n) n))


;define and test (sum-prime-square a b)
(define (inc n) (+ n 1))

(define (sum-prime-square a b)
    (filtered-accumulate prime? + 0 square a inc b))

(sum-prime-square 2 7) ;87

