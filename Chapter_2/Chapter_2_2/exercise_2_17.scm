;Exercise 2.17. Define a procedure last-pair that returns the list that contains
;only the last element of a given (nonempty) list:
; (last-pair (list 23 72 149 34))
; (34)


; The solution here is pretty easy. We just need to check if the second element
; of the pair is null. If so, then it is the last element of the list (as long
; as the list is not the empty list)
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; test
; should be 34
(last-pair (list 23 72 149 34))
; should be 12
(last-pair (list 45 12))

