; Exercise 2.28. Write a procedure fringe that takes as argument a tree
; (represented as a list) and returns a list whose elements are all the leaves
; of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

; (fringe x)
; (1 2 3 4)
; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

; This is similar to the deep-reverse from the last problem, except this time
; we are not reversing the lists. Base conditions are to return null when you
; reach the end of the list and the value when it is not a list. For any list,
; we "fringe" down them until they are single values.
(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) 
                      (fringe (cdr l))))))
(fringe x)
