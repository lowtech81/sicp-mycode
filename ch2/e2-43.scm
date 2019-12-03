; Exercise 2.43.  Louis Reasoner is having a terrible time doing exercise 2.42.
; His queens procedure seems to work, but it runs extremely slowly. (Louis
; never does manage to wait long enough for it to solve even the 6× 6 case.)
; When Louis asks Eva Lu Ator for help, she points out that he has interchanged
; the order of the nested mappings in the flatmap, writing it as

(flatmap
(lambda (new-row)
  (map (lambda (rest-of-queens)
         (adjoin-position new-row k rest-of-queens))
       (queen-cols (- k 1))))
(enumerate-interval 1 board-size))

; Explain why this interchange makes the program run slowly. Estimate how long
; it will take Louis's program to solve the eight-queens puzzle, assuming that
; the program in exercise 2.42 solves the puzzle in time T.
; ------------------------------------------------------------
A solution found in: http://community.schemewiki.org/?sicp-ex-2.43

Where the guy "woofy" says:

Not solved yet but share my thinking process:

Q(k): number of solutions of board size n * k

T(k): number of steps required to calculate Q(k)

original approach: T(k) = T(k-1) + n * Q(k-1)

T(0) = 1

louis's approach: T(k) = n * (T(k-1) + Q(k-1)) = n * T(k-1) + n * Q(k-1)

T(0) = 1

So louis's approach requires signifcantly more steps. How many more? It's tempted to say a factor of N^N due to tree recursion but it may not be the actual case.

Found this analysis for reference: https://wernerdegroot.wordpress.com/2015/08/01/sicp-exercise-2-43/
