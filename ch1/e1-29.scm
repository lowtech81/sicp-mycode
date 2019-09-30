(define (sum term a next b)
    (if (> a b)
        0
        (+  (term a)
            (sum term (next a) next b))))

(define (cube x)
    (* x x x))

(define (integral f a b n)
    (define h (/ (- b a) n))
    (define (y-next x) (+ x (* 2 h )))
    (define (sum-odd-items)
        (* 4 (sum f (+ a h) y-next (- b h))))
    (define (sum-even-items)
        (* 2 (sum f (+ a (* 2 h)) y-next (- b (* 2 h)))))    
    (* (/ h 3.0) (+ (f a) (f b) (sum-odd-items) (sum-even-items)))
    )

(integral cube 0 1 100)

(integral cube 0 1 1000)

(integral square 0 1 100)

(integral sin 0 1 100)