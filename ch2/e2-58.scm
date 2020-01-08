; Exercise 2.58.
;
; Suppose we want to modify the differentiation program so that
; it works with ordinary mathematical notation, in which + and * are infix
; rather than prefix operators. Since the differentiation program is defined in
; terms of abstract data, we can modify it to work with different
; representations of expressions solely by changing the predicates, selectors,
; and constructors that define the representation of the algebraic expressions
; on which the differentiator is to operate.

; a. Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
; task, assume that + and * always take two arguments and that expressions are
; fully parenthesized.

; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such
; that our derivative program still works?
;------------------------------------------------------------

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

; a. Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
; task, assume that + and * always take two arguments and that expressions are
; fully parenthesized.

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; (deriv '(x + (3 * (x + (y + 2)))) 'x)
; (deriv '(x * (3 * (x + (y + 2)))) 'x)

; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such
; that our derivative program still works?

(define (sum? x)
    (and (pair? x) (not (null? (filter (lambda (item) (eq? item '+)) x)))))))

(define (addend s)
    (if (eq? (cadr s) '+)
        (car s)
        (let ((x (addend (cdr s))))
             (if (pair? x) 
                 (cons (car s) x)
                 (list (car s) x)))))

(define (augend s)
    (if (eq? (cadr s) '+)
        (if (null? (cdddr s))
            (caddr s)
            (cddr s))
        (augend (cdr s))))

(define (product? x)
    (and (pair? x) (not (sum? x)) (not (null? (filter (lambda (item) (eq? item '*)) x)))))


(define (multiplier s)
    (if (eq? (cadr s) '*)
        (car s)
        (let ((x (addend (cdr s))))
             (if (pair? x) 
                 (cons (car s) x)
                 (list (car s) x)))))

(define (multiplicand s)
    (if (eq? (cadr s) '*)
        (if (null? (cdddr s))
            (caddr s)
            (cddr s))
        (multiplicand (cdr s)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (pair? a1) (not (pair? a2))) (append a1 (list '+ a2)))
        ((and (not (pair? a1)) (pair? a2)) (append (list a1 '+) a2))
        ((and (pair? a1) (pair? a2)) (append a1 (cons '+ a2)))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (product? m1) (sum? m2)) (append (append m1 (list '*)) (list m2)))
        ((and (sum? m1) (product? m2)) (append (list m1 '*) m2))
        ((and (product? m1) (product? m2)) (append m1 cons('* m2)))
        ((and (product? m1) (not (pair? m2))) (append m1 (list '* m2)))
        ((and (not (pair? m1)) (product? m2)) (append (list m1 '*) m2))
        (else (list m1 '* m2))))
; (sum? '(a * b + c))

; (addend '(a * (b + c) * c + c * d + e)))

; (augend '(a * (b + c) * c + c * d + e))


; (sum? '(a * (b + c)))

; (product? '(a * b + c))

; (product? '(a * b * c))

; (multiplier '((a + b) * (c + d)))

; (multiplicand '((a + b) * (c + d) * (e + f)))

; (multiplicand (augend '(x + 3 * (x + y + 2))))

(make-sum '(x + y * z) '(x * (y + z)))

(make-product '(x + y * z) '(x * (y + z)))

(deriv '(x + 3 * (x + y + 2)) 'x)

(deriv '(x * (x * y + z) * 4 + x * 5 + x * x * 3 + y * 5) 'x)

(deriv '(x * (x * y + z) * 4 + x * 5 + x * x * 3 + y * 5) 'y)