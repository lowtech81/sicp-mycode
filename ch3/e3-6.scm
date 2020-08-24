; Exercise 3.6.
;
; It is useful to be able to reset a random-number generator to
; produce a sequence starting from a given value. Design a new rand procedure
; that is called with an argument that is either the symbol generate or the
; symbol reset and behaves as follows: (rand 'generate) produces a new random
; number; ((rand 'reset) <new-value>) resets the internal state variable to the
; designated <new-value>. Thus, by resetting the state, one can generate
; repeatable sequences. These are very handy to have when testing and debugging
; programs that use random numbers.
; ------------------------------------------------------------
(define seed 20170705)
(define (rand symbol)
    (cond ((eq? symbol 'generate) 
                    (begin (set! seed 
                            (modulo (+ 5 (* 3 seed)) 19))
                            seed))
          ((eq? symbol 'reset) 
                    (lambda (new-value) (set! seed new-value)))
          (else (error "Unknown request:Rand"
                 symbol))))

;test code
(rand 'generate)   
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 235741)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 20170705)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

