; Exercise 2.18. Define a procedure reverse that takes a list as an argument and
; returns a list of the same elements in reverse order:
; (reverse (list 1 4 9 16 25))
; (25 16 9 4 1)

; Here we are going to use the definition of append from the text:
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Now we will use that definition to recursively reverse the list. Essentially
; all we need to do is append the car of the list to the reverse of the cdr
; of the list
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))
; The list call in (list (car l)) is there because the car of each pair in the
; data structure can be a single value. When the end of the list is reached
; append will return a single value (not a list) which cdr will then be applied
; and fail.

; tests
(reverse (list 1 4 9 16 25))

