; Exercise 2.42.

; The ``eight-queens puzzle'' asks how to place eight queens on a
; chessboard so that no queen is in check from any other 
; (i.e., no two queens are in the collides row, column, or diagonal).
; One possible solution is shown in figure 2.8. One way to solve the
; puzzle is to work across the board, placing a queen in each column. Once
; we have placed k - 1 queens, we must place the kth queen in a position
; where it does not check any of the queens already on the board. We can
; formulate this approach recursively: Assume that we have already
; generated the sequence of all possible ways to place k - 1 queens in the
; first k - 1 columns of the board. For each of these ways, generate an
; extended set of positions by placing a queen in each row of the kth
; column. Now filter these, keeping only the positions for which the queen
; in the kth column is safe with respect to the other queens. This
; produces the sequence of all ways to place k queens in the first k
; columns. By continuing this process, we will produce not only one
; solution, but all solutions to the puzzle.

; We implement this solution as a procedure queens, which returns a
; sequence of all solutions to the problem of placing n queens on an n×
; n chessboard. Queens has an internal procedure queen-cols that returns
; the sequence of all ways to place queens in the first k columns of the
; board.

; (define (queens board-size)
;   (define (queen-cols k)
;     (if (= k 0)
;         (list empty-board)
;         (filter
;          (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (rest-of-queens)
;             (map (lambda (new-row)
;                    (adjoin-position new-row k rest-of-queens))
;                  (enumerate-interval 1 board-size)))
;           (queen-cols (- k 1))))))
;   (queen-cols board-size))

; In this procedure rest-of-queens is a way to place k - 1 queens in the
; first k - 1 columns, and new-row is a proposed row in which to place
; the queen for the kth column. Complete the program by implementing the
; representation for sets of board positions, including the procedure
; adjoin-position, which adjoins a new row-column position to a set of
; positions, and empty-board, which represents an empty set of
; positions. You must also write the procedure safe?, which determines
; for a set of positions, whether the queen in the kth column is safe
; with respect to the others. (Note that we need only check whether the
; new queen is safe -- the other queens are already guaranteed safe with
; respect to each other.)
; -------------------------------------------

(load "../common.scm")
(load "../helpers.scm")

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

(define empty-board nil)

; a board position is represent as a list of (col row) pair
; for example: ((2 1) (3 4) (1 3))
(define (adjoin-position new-row k rest-of-queens)
    (cons (list k new-row) rest-of-queens))

(define (safe? k positions)
    (let ((k-position (car positions))  ;the k-th column position
          (rest-positions (cdr positions)) ; the 1 to k-1 column position
          (k-row (cadr (car positions)))) ; the row num of the k-th position
        ; check if the position in this column is not safe for the new k-th 
        ; column posiiton 
        (define (check column)  
            (if (= column 0)
                #t
                (and (check (- column 1))
                     (null?
                        (filter
                            (lambda (pos)
                                (and (= (car pos) column) 
                                     (or (= (cadr pos) k-row)
                                         (= (cadr pos) (- k-row (- k column)))
                                         (= (cadr pos) (+ k-row (- k column))))))
                            rest-positions)))))
        (check (- k 1))))

; print (queens n) in a better look which every answer is like (row1 row2 row3) 
; where column represented by element index and corresponding row represented 
; by element itself  
(define (print-queens n)
    (newline)
    (map (lambda (x) 
            (display (accumulate cons nil (map (lambda (y) (cadr y)) x)))
            (newline)) 
         (queens n)))

(length (queens 8))

(print-queens 8)

(length (queens 4))

(print-queens 4)