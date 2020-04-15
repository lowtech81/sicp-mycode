; Exercise 2.84.
;
; Using the raise operation of exercise 2.83, modify the
; apply-generic procedure so that it coerces its arguments to have the
; same type by the method of successive raising, as discussed in this
; section. You will need to devise a way to test which of two types is
; higher in the tower. Do this in a manner that is ``compatible'' with
; the rest of the system and will not lead to problems in adding new
; levels to the tower.
; ------------------------------------------------------------

(load "e2-83.scm")

; for simplicity, only consider operations between two arguments
(define (install-compare-type)
    (put 'lower '(number rational) true)
    (put 'lower '(rational complex) true)
    (put 'lower '(number complex) true)
    'done)

(define (lower? type1 type2)
    (get 'lower (list type1 type2)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (no-method-error)
        (error "No method for these types"
                     (list op type-tags)y))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (no-method-error)
                    (if (lower? type1 type2)
                        (apply-generic op (raise a1) a2)
                        (apply-generic op a1 (raise a2)))))
              (no-method-error))))))

;;; test code

(install-compare-type)

(lower? 'number 'rational)

(apply-generic 'add 1 (make-rational 1 3))

(apply-generic 'add 1 (make-complex-from-real-imag 1.5 2))

(apply-generic 'add (make-complex-from-real-imag 1.5 2) 1)

(apply-generic 'add (make-complex-from-real-imag 1.5 2) (make-rational 1 3))


