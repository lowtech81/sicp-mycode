; Exercise 2.78.
;
; The internal procedures in the scheme-number package are
; essentially nothing more than calls to the primitive procedures +, -, etc. It
; was not possible to use the primitives of the language directly because our
; type-tag system requires that each data object have a type attached to it. In
; fact, however, all Lisp implementations do have a type system, which they use
; internally. Primitive predicates such as symbol? and number? determine
; whether data objects have particular types. Modify the definitions of
; type-tag, contents, and attach-tag from section 2.4.2 so that our generic
; system takes advantage of Scheme's internal type system. That is to say, the
; system should work as before except that ordinary numbers should be
; represented simply as Scheme numbers rather than as pairs whose car is the
; symbol scheme-number.
; ------------------------------------------------------------

(load "../common.scm")
(load "2.4.3-table.scm")

;;;SECTION 2.4.2 modified as below

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        ((symbol? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'number)
        ((symbol? datum) 'symbol)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        ((symbol? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))



;;;SECTION 2.5.1 modified as below

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (put 'add '(number number)
       (lambda (x y)  (+ x y)))
  (put 'sub '(number number)
       (lambda (x y)  (- x y)))
  (put 'mul '(number number)
       (lambda (x y)  (* x y)))
  (put 'div '(number number)
       (lambda (x y)  (/ x y)))
  'done)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; test code
(install-scheme-number-package)
(install-rational-package)

(add 3 4)

; make sure rationl works correctly
(define rat1 (make-rational 1 2))
(define rat2 (make-rational 2 4))

(add rat1 rat2)
