; Exercise 2.27. Modify your reverse procedure of exercise 2.18 to produce a
; deep-reverse procedure that takes a list as argument and returns as its 
; value the list with its elements reversed and with all sublists deep-reversed
; as well. For example,

(define x (list (list 1 2) (list 3 4)))

; (reverse x)
; ((3 4) (1 2))
; (deep-reverse x)
; ((4 3) (2 1))

; Taken from Exercise 2.18:
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

; Lets make sure reverse is working:
(reverse x)

; Now lets define deep-reverse, all we need is an extra condition to check if
; the first element of each list is a list itself. Then we go ahead and reverse
; that.

(define (deep-reverse l)
  (if (null? l)
      l
      (append (deep-reverse (cdr l))
              (cond ((pair? (car l)) (list (deep-reverse (car l))))
                    (else (list (car l)))))))
(deep-reverse x)
