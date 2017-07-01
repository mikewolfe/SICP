;Exercise 2.53. What would the interpreter print in response to evaluating each of the following expressions?
;(list ’a ’b ’c)
;(list (list ’george))
;(cdr ’((x1 x2) (y1 y2)))
;(cadr ’((x1 x2) (y1 y2)))
;(pair? (car ’(a short list)))
;(memq ’red ’((red shoes) (blue socks)))
;(memq ’red ’(red shoes blue socks))

; should be (a b c)
(list 'a 'b 'c)
; should be ((george))
(list (list 'george))
; should be ((y1 y2))
(cdr '((x1 x2) (y1 y2)))
; should be (y1 y2)
(cadr '((x1 x2) (y1 y2)))
; should be #f
(pair? (car '(a short list)))
; should be #f
(memq 'red '((red shoes) (blue socks)))
; should be (red shoes blue socks)
(memq 'red '(red shoes blue socks))
