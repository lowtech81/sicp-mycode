; Exercise 2.73.
;
; Section 2.3.2 described a program that performs
; symbolic differentiation:

; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         ; <more rules can be added here>
;         (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the ``type tag'' of the
; datum is the algebraic operator symbol (such as +) and the operation being
; performed is deriv. We can transform this program into data-directed style by
; rewriting the basic derivative procedure as

; (define (deriv exp var)
;    (cond ((number? exp) 0)
;          ((variable? exp) (if (same-variable? exp var) 1 0))
;          (else ((get 'deriv (operator exp)) (operands exp)
;                                             var))))
; (define (operator exp) (car exp))
; (define (operands exp) (cdr exp))

; a.  Explain what was done above. Why can't we assimilate the predicates
; number? and same-variable? into the data-directed dispatch?

; b.  Write the procedures for derivatives of sums and products, and the
; auxiliary code required to install them in the table used by the program
; above.

; c.  Choose any additional differentiation rule that you like, such as the one
; for exponents (exercise 2.56), and install it in this data-directed system.

; d.  In this simple algebraic manipulator the type of an expression is the
; algebraic operator that binds it together. Suppose, however, we indexed the
; procedures in the opposite way, so that the dispatch line in deriv looked
; like

; ((get (operator exp) 'deriv) (operands exp) var)

; What corresponding changes to the derivative system are required?
; ------------------------------------------------------------

; a.
; The differentiations for sums and products were installed in a data table.
; So we can dispatch corresponding deriv procedure for different expressions.
; But number?, same-variable? are predicates. there's nothing to dispatch.

; b. 
(load "../common.scm")
(load "2.3.2-deriv.scm")
(load "2.4.3-table.scm")


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-sum-package)

  (define (addend s) (car s))

  (define (augend s) (cadr s))

  (define (deriv-sum exp var)
  ;make-sum from 2.3.2-deriv.scm
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
              
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product-package)

  (define (multiplier p) (car p))

  (define (multiplicand p) (cadr p))

  (define (deriv-product exp var)
  ;make-sum and make-product from 2.3.2-deriv.scm
    (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
                         
  (put 'deriv '* deriv-product)
  'done)

(install-deriv-sum-package)
(install-deriv-product-package)

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;c.
(define (install-deriv-exponent-package)

  (define (base e) (car e))

  (define (exponent e) (cadr e))

  (define (make-exponentiation b e)
    (cond ((=number? e 1) b)
          ((=number? e 0) 1)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '** b e))))

  (define (exp b e)
    (if (= e 0)
        1
        (* b (exp b (- e 1)))))

  (define (deriv-exponent exp var)
    (make-product 
                (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))
  (put 'deriv '** deriv-exponent)
  'done)

(install-deriv-exponent-package)
(deriv '(** (+ (** x 2) 1) 3) 'x)
(deriv '(** (* x 3) 5) 'x)

; d.
; We need to swap the position of first two arguments of the  "put" procedure. 