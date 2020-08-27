; Exercise 3.7.
;
; Consider the bank account objects created by make-account,
; with the password modification described in exercise 3.3. Suppose that our
; banking system requires the ability to make joint accounts. Define a
; procedure make-joint that accomplishes this. Make-joint should take three
; arguments. The first is a password-protected account. The second argument
; must match the password with which the account was defined in order for the
; make-joint operation to proceed. The third argument is a new password.
; Make-joint is to create an additional access to the original account using
; the new password. For example, if peter-acc is a bank account with password
; open-sesame, then

; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))

; will allow one to make transactions on peter-acc using the name paul-acc and
; the password rosebud.You may wish to modify solution to exercise 3.3 to
; accommodate this new feature.
; ------------------------------------------------------------

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                     m)))
        (lambda (x)  "Incorrect account password")))
    dispatch)


(define (make-joint account account-password joint-password)
    (define (withdraw amount)
        ((account account-password 'withdraw) amount))
    (define (deposit amount)
        ((acount account-password 'deposit) amount))
    (define (dispatch p m)
        (if (eq? p joint-password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                        m)))
            (lambda (x)  "Incorrect joint password")))
    dispatch)


; test code 

(define peter-acc (make-account 100 'open-sesame))

((peter-acc 'open-sesame 'withdraw ) 20)

(define paul-acc
    (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw ) 30)

((peter-acc 'open-sesame 'withdraw ) 10)

((peter-acc 'open-sesame 'deposit) 100)

((paul-acc 'rosebud 'withdraw ) 40)