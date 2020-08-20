; Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by
; adding another local state variable so that, if an account is accessed
; more than seven consecutive times with an incorrect password, it
; invokes the procedure call-the-cops.
; ------------------------------------------------------------

(define (make-account balance password)
   (let ((cons-attempts 0))
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (call-the-cops)
        (lambda (x) "Seven consecutive attempts, cops called!"))
    (define (dispatch p m)
        (if (eq? p password)
            (begin (set! cons-attempts 0)
                   (cond ((eq? m 'withdraw) withdraw)
                         ((eq? m 'deposit) deposit)
                         (else (error "Unknown request -- MAKE-ACCOUNT"
                                m))))
            (begin (set! cons-attempts (+ 1 cons-attempts))
                   (if (= cons-attempts 7) 
                       (call-the-cops)
                       (lambda (x)  "Incorrect password")))))
    dispatch))

; test code
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'secret-password 'withdraw) 30) ; 30
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Seven consecutive attempts, cops called!"

((acc 'secret-password 'withdraw) 50) ; Insufficient funds 
    