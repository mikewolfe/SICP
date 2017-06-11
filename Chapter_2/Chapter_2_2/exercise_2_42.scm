; Exercise 2.42 The ‘‘eight-queens puzzle’’ asks how to place eight queens on
; a chessboard so that no queen is in check from any other (i.e., no two queens
; are in the same row, column, or diagonal). One possible solution is shown in
; figure 2.8. One way to solve the puzzle is to work across the board, placing
; a queen in each column. Once we have placed k - 1 queens, we must place the
; kth queen in a position where it does not check any of the queens already on
; the board. We can formulate this approach recursively: Assume that we have
; already generated the sequence of all possible ways to place k - 1 queens in
; the first k - 1 columns of the board. For each of these ways, generate an
; extended set of positions by placing a queen in each row of the kth column.
; Now filter these, keeping only the positions for which the queen in the kth
; column is safe with respect to the other queens. This produces the sequence of
; all ways to place k queens in the first k columns. By continuing this process,
; we will produce not only one solution, but all solutions to the puzzle.
;
;We implement this solution as a procedure queens, which returns a sequence of
;all solutions to the problem of placing n queens on an n× n chessboard. Queens
;has an internal procedure queen-cols that returns the sequence of all ways to
;place queens in the first k columns of the board.
;(define (queens board-size)
;  (define (queen-cols k)
;    (if (= k 0)
;        (list empty-board)
;        (filter
;         (lambda (positions) (safe? k positions))
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens))
;                 (enumerate-interval 1 board-size)))
;          (queen-cols (- k 1))))))
;  (queen-cols board-size))
;In this procedure rest-of-queens is a way to place k - 1 queens in the first
;k - 1 columns, and new-row is a proposed row in which to place the queen for
;the kth column. Complete the program by implementing the representation for
;sets of board positions, including the procedure adjoin-position, which adjoins
;a new row-column position to a set of positions, and empty-board, which
;represents an empty set of positions. You must also write the procedure safe?,
;which determines for a set of positions, whether the queen in the kth column is
;safe with respect to the others. (Note that we need only check whether the new
;queen is safe -- the other queens are already guaranteed safe with respect to
;each other.);

; Need all the helper functions:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; We can easily define an empty-board as an empty list.
(define empty-board '())

; We can also define a representation for board positions as pairs:
(define (position row col)
  (cons row col))
(define (row-position pos)
  (car pos))
(define (col-position pos)
  (cdr pos))

; Defining adjoin position is straight forward as well as we are just adding
; a new position to a list of old positions
(define (adjoin-position row column position-list)
  (append position-list (list (position row column))))

; safe? is the hardest procedure to implement. We need to check given a col
; location and a list of positions, whether the queen in col is safe or not.

; Here we can use list-ref to grab only the position for the col we are focusing
; on. This is adapted with some help from the Bill the Lizard SICP blog
(define (safe? col all-queens)
  (let ((this-queen (list-ref all-queens (- col 1)))
        (other-queens (filter (lambda (x) (not (= (col-position x) col)))
                              all-queens)))
    (define (bad q1 q2)
      (or (= (row-position q1) (row-position q2))
          (= (abs (- (row-position q1) (row-position q2)))
             (abs (- (col-position q1) (col-position q2))))))
    (fold-left (lambda (a b) (and a b)) #t 
               (map (lambda (y) (not (bad y this-queen)))
                    other-queens))))

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

;testing
; should have 1 solution
(queens 1)
; no solutions
(queens 2)
; no solutions
(queens 3)
; Two solutions
; (((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))
(queens 4)

