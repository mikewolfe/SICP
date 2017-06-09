; Exercise 2.25. Give combinations of cars and cdrs that will pick 7 from each 
; of the following lists:

(define x (list 1 3 (list 5 7) 9))
x
; Pull out 7 with car and cdr
(car (cdr (car (cdr (cdr x)))))

(define x (list (list 7)))
x
(car (car x))

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
x
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

