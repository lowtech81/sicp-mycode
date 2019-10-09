(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value)
                        (search f neg-point midpoint))
                            ((negtive? test-value)
                        (search f midpoint pos-point))
                      (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2.0))

(define (negtive? x)
    (< x 0))

(define (positive? x)
    (> x 0))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
                (search f a b))
              ((and (negative? b-value) (positive? a-value))
                (search f b a))
              (else 
                (error "Values are not of opposite sign" a b)))))


(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                        1.0  
                        2.0)