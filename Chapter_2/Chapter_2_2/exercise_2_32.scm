; Exercise 2.32. We can represent a set as a list of distinct elements, and
; we can represent the set of all subsets of the set as a list of lists. For
; example, if the set is (1 2 3), then the set of all subsets is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition
; of a procedure that generates the set of subsets of a set and give a clear
; explanation of why it works:

; The wikipedia article on powersets has a section on a recursive algorithm to
; return the powerset of any set. Here is its contents:
; ``` WIKIPEDIA
;If S is a finite set, there is a recursive algorithm to calculate P(S).
;
;Define the operation F (e, T) = {X ∪ {e} | X ∈ T}.
;
;In English, return the set with the element e added to each set X in T.
;
;If S = { }, then P(S) = { { } } is returned.  Otherwise: Let e be any single
;element of S.  Let T = S \ {e}, where S \ {e} denotes the relative complement of
;e in S.  And the result: P(S) = P(T) ∪ F (e, P(T)) is returned.  In other words,
;the power set of the empty set is the set containing the empty set and the power
;set of any other set is all the subsets of the set containing some specific
;element and all the subsets of the set not containing that specific element.
;``` WIKIPEDIA

; The main part that is already done by the function is the part in brackets
;Powerset(S) = [P(T) U] F(e, P(T))
; We just need to define the function F(e, P(T))
; F(e, T) = { X u {e} | X element of T}
; So we basically just need a function that will add an element to a set.

(define (subsets s)
  (if (null? s)
     (list `())
     (let ((rest (subsets (cdr s))))
       (append rest (map (lambda (X) (cons (car s) X)) 
                         rest)))))

; Testing
(subsets (list 1 2 3))


; Lets do this by hand to see how it works:
(subsets (list 2 3))
; (subsets (list 2 3))
; rest <- subsets (3, '())
;         rest <- (subsets '())
;                  rest <- '()
;         (append '() (map (lambda (X) (cons (car (3, '())) X)) '())
;         (append '() (cons 3 '()))
;         ('() (3))
; (append ('() (3)) (map (lambda (X) (cons 2 X)) ('() (3)))
; (append ('() (3)) ((2) (2 3)))
; ('() (3) (2) (2 3))
;
