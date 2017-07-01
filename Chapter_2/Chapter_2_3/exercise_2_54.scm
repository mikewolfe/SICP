;Exercise 2.54. Two lists are said to be equal? if they contain equal elements
;arranged in the same order. For example,
;
;(equal? ’(this is a list) ’(this is a list))
;
;is true, but
;
;(equal? ’(this is a list) ’(this (is a) list))
;
;is false. To be more precise, we can define equal? recursively in terms of the
;basic eq? equality of symbols by saying that a and b are equal? if they are
;both symbols and the symbols are eq?, or if they are both lists such that (car
;a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea,
;implement equal? as a procedure.


(define (equal? a b)
  ; both symbols check equality
  (cond ((and (not (pair? a)) (not (pair? b))) 
          (eq? a b))
        ; both lists, check equality of car and cdr
        ((and (pair? a) (pair? b))
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))
        ; can't be equal if not both lists or symbols
        (else #f)))

; testing
(define x '(a b c))
(define y '(a b c d))
(define z '(d c b a))
; #f
(equal? x y)
; #f
(equal? y z)
; #t
(equal? x x)

; #t
(equal? '(this is a list) '(this is a list))
; #f
(equal? '(this is a list) '(this (is a) list))

