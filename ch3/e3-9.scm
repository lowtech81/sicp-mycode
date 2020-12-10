; Exercise 3.9.
;
; In section 1.2.1 we used the substitution model to
; analyze two procedures for computing factorials, a recursive version

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; and an iterative version

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Show the environment structures created by evaluating (factorial 6)
; using each version of the factorial procedure.14
;
; --------
;
; Solution to this is drawing of the environmental model for both of the
; executions. There is nothing special to see here except the fact that
; application of same function will produce new frames every time it is
; applied to the new arguments.

RECURSIVE VERSION

global  ________________________
env     | other var.            |
------->| factorial : *         |
        |             |         |
        |_____________|_________|
                      |     ^
                      |     |
                variables : n
                body: (if (= n 1) 1 (* n (factorial (- n 1))))

(factorial 6)

         _______            ^
  E1 -->| n : 6 |___________| GLOBAL
         -------
        (* 6 (factorial 5))
         _______            ^
  E2 -->| n : 5 |___________| GLOBAL
         -------
        (* 5 (factorial 4))
         _______            ^
  E3 -->| n : 4 |___________| GLOBAL
         -------
        (* 4 (factorial 3))
         _______            ^ 
  E4 -->| n : 3 |___________| GLOBAL
         -------
        (* 3 (factorial 2))
         _______            ^
  E5 -->| n : 2 |___________| GLOBAL
         -------
        (* 2 (factorial 1))
         _______            ^
  E6 -->| n : 1 |___________| GLOBAL
         -------
         1

ITERATIVE VERSON

global  ___________________________________
env     | other var.                       |
------->| factorial : *                    |
        | fact-iter : |               *    |
        |_____________|_______________|____|
                      |       ^       |  ^
                      |       |       |  |
                      |       |       variable : (product counter max-count)
                      |       |       body: (if (> counter max-count) 
                      |       |                 prod 
                      |       |                 (fact-iter (* counter product)
                      |       |                            (+ counter 1)
                      |       |                            max-count))
                      |       |
                variable: n
                body: (fact-iter 1 1 n)

(factorial 6)

         _______              ^
  E1 -->| n : 6 |_____________| GLOBAL  
         -------
         (fact-iter 1 1 n)

  E2 -->| product   : 1       ^
        | counter   : 1    ___| GLOBAL 
        | max-count : 6
         (fact-iter 1 2 6)

  E3 -->| product   : 1       ^
        | counter   : 2   ____| GLOBAL
        | max-count : 6
         (fact-iter 2 3 6)

  E4 -->| product   : 2       ^
        | counter   : 3  _____| GLOBAL
        | max-count : 6
         (fact-iter 6 4 6)

  E5 -->| product   : 6       ^
        | counter   : 4  _____| GLOBAL
        | max-count : 6
         (fact-iter 24 5 6)

  E6 -->| product   : 24      ^
        | counter   : 5  _____| GLOBAL
        | max-count : 6
         (fact-iter 120 6 6)

  E7 -->| product   : 120     ^
        | counter   : 6  _____| GLOBAL
        | max-count : 6
         (fact-iter 720 7 6)

  E8 -->| product   : 720     ^
        | counter   : 7  _____| GLOBAL
        | max-count : 6
         720