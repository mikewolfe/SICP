; Exercise 2.31 Abstract your answer to exercise 2.30 to produce a procedure
; tree-map with the property that square-tree could be defined as
;
; (define (square-tree tree) (tree-map square tree))

(define (square x)
  (* x x))

; Defined without higher order procedures. Essentially just replacing the 
; square-tree procedure from exercise 2.30 with any arbitrary procedure that
; acts on single elements 
(define (tree-map proc items)
  (cond ((null? items) `())
        ((not (pair? items)) (proc items))
        (else (cons (tree-map proc (car items))
                    (tree-map proc (cdr items))))))
; Testing
(define (square-tree tree) (tree-map square tree))
; should be (1 (4 (9 16) 25) (36 49))
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

