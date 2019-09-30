(define (pascal-num row col)
    (cond ((and (> row 0) (or (= col 1) (= col row))) 1)
        ((or (< col 1) (> col row)) 0)
        (else (+ (pascal-num (- row 1) (- col 1)) (pascal-num (- row 1) col)))))