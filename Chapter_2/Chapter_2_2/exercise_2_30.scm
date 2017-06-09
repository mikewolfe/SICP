; Exercise 2.30. Define a procedure square-tree analogous to the square-list
; procedure of exercise 2.21. That is, square-list should behave as follows:
;
; (square-tree
;   (list 1
;         (list 2 (list 3 4) 5)
;         (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
;
; Define square-tree both directly (i.e., without using any higher-order
; procedures) and also by using map and recursion.
(define (square x)
  (* x x))

; This procedure is almost identical to the scale-tree procedure shown in the
; textbook
(define (square-tree items)
  (cond ((null? items) `())
        ((not (pair? items)) (square items))
        (else (cons (square-tree (car items))
                    (square-tree (cdr items))))))

; should be (1 (4 (9 16) 25) (36 49))
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

; Now defined by map and recursion. This is also identical to the map version of
; scale-tree in the book
(define (square-tree items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       items))

; should be (1 (4 (9 16) 25) (36 49))
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
