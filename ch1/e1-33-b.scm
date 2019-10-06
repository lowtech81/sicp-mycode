; You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms 
; to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition.
; The resulting filtered-accumulate abstraction takes the same arguments as accumulate, to-gether with an additional predicate 
; of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using 
; filtered accumulate:
; b. the product of all the positive integers less than that n are relatively prime to n (i.e., all positive integers i < n 
; such that gcd(i,n) = 1).

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

;define relativily prime (rel-prime? a b)
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (rel-prime? a b)
    (= (gcd a b) 1))


;define and test (sum-prime-square a b)
(define (inc n) (+ n 1))

(define (identity n) n)

(define (product_rp n)
    (define (rel? a)
        (rel-prime? a n))
    (filtered-accumulate rel? * 1 identity 1 inc n))

(product_rp 5)