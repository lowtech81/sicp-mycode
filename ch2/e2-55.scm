; Exercise 2.55.
;
; Eva Lu Ator types to the interpreter the expression

; (car ''abracadabra)

; To her surprise, the interpreter prints back quote. Explain.
;------------------------------------------------------------

(car ''abracadabra) ; quote

(cdr ''abracadabra) ; (abracadabra)

; It seems that ''abracadabra is equal to (quote abracadabra)

(cadr '''a) ; (quote a)

; so may be the ' is just grammar sugar of the procedure quote, let's test it

(quote (a b c)) ; (a b c)

(quote a) ; a