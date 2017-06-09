; Exercise 2.26. Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

; What is printed by the interpreter in response to evaluating each of the 
; following expressions:

; should be (1 2 3 4 5 6)
(append x y)
; I thought it should be ((1 2 3) (4 5 6))
(cons x y)
; But it ends up being ((1 2 3) 4 5 6)
;
; should be ((1 2 3) (4 5 6))
(list x y)
