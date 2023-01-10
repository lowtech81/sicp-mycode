; a procedure that compute factorials
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

; a procedure that compute febonacci numbers

(define (febonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (febonacci (- n 1)) (febonacci (- n 2))))))

(febonacci 10)
