; Exercise 2.22. Louis Reasoner tries to rewrite the first square-list procedure
; of exercise 2.21 so that it evolves an iterative process:

(define (square item)
  (* item item))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
    (iter items `()))

(square-list (list 1 2 3 4 5))
; Unfortunately, defining square-list this way produces the answer list in the 
; reverse order of the one desired. Why?

; The way he has it written it will add items to the front of the list at every
; iteration.

; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items `()))
(square-list (list 1 2 3 4 5))

; When he does this, he is adding the previous list to the front of a new list
; at every iteration
