; Exercise 2.57.
;
; Extend the differentiation program to handle sums and
; products of arbitrary numbers of (two or more) terms. Then the last example
; above could be expressed as

; (deriv '(* x y (+ x 3)) 'x)

; Try to do this by changing only the representation for sums and products,
; without changing the deriv procedure at all. For example, the addend of a sum
; would be the first term, and the augend would be the sum of the rest of the
; terms.
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.2.scm")


;; based on e2-56.scm
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ;new clause for exponentiation
        ((exponentiation? exp)
         (make-product 
                (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base e)
    (cadr e))

(define (exponent e)
    (caddr e))

(define (make-exponentiation b e)
    (cond ((=number? e 1) b)
          ((=number? e 0) 1)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '** b e))))

(define (exp b e)
    (if (= e 0)
        1
        (* b (exp b (- e 1)))))

;; The origin implementations as below.

; (define (sum? x)
;   (and (pair? x) (eq? (car x) '+)))

; (define (addend s) (cadr s))

; (define (augend s) (caddr s))

; (define (product? x)
;   (and (pair? x) (eq? (car x) '*)))

; (define (multiplier p) (cadr p))

; (define (multiplicand p) (caddr p))


; (define (make-sum a1 a2)
;   (cond ((=number? a1 0) a2)
;         ((=number? a2 0) a1)
;         ((and (number? a1) (number? a2)) (+ a1 a2))
;         (else (list '+ a1 a2))))

; (define (=number? exp num)
;   (and (number? exp) (= exp num)))

; (define (make-product m1 m2)
;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;         ((=number? m1 1) m2)
;         ((=number? m2 1) m1)
;         ((and (number? m1) (number? m2)) (* m1 m2))
;         (else (list '* m1 m2))))

;; We only need to change the implementation of prodedure 'augend' and 'multiplicand', others will stay as them were.
;; So we can get augend and multiplicand as sum and product expression too. We have elegance here :) 

(define (augend s)
    (let ((augend-part (cddr s)))
        (if (null? (cdr augend-part))
            (car augend-part)
            (cons '+ augend-part))))

(define (multiplicand p)
    (let ((m-part (cddr p)))
        (if (null? (cdr m-part))
            (car m-part)
            (cons '* m-part))))

(deriv '(* x y (+ x 3)) 'x)