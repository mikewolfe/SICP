; Exercise 2.60. We specified that a set would be represented as a list with no
; duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3}
; could be represented as the list (2 3 2 1 3 2 2). Design procedures
; element-of-set?, adjoin-set, union-set, and intersection-set that operate on
; this representation. How does the efficiency of each compare with the
; corresponding procedure for the non-duplicate representation? Are there
; applications for which you would use this representation in preference to the
; non-duplicate one?

; Copying over the procedures from the last exercise:

; this doesn't have to change
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; we can remove the requirement for checking if it is in the set already or not
; for the rest of the procedures
(define (adjoin-set x set)
      (cons x set))

; This definition doesn't have to change, but you may get different answers
; depending on the order you put the sets in?
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Here we remove the element-of-set? test. This is essential just appending
; set1 to set2
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (cons 
                (car set1) 
                (union-set (cdr set1) set2)))))

; Testing

(intersection-set '(1 2 3 3 3 3 4) '(5 6 3 7 2))
(intersection-set '(5 6 3 7 2) '(1 2 3 3 3 3 4))

; You could interpret this as asking the question, which elements of set1 are
; in set2
;
; Both element-of-set? and intersection-set have not changed so their
; performance is the same. Adjoin-set has gone from O(n) to O(1) and 
; union-set has gone from O(n^2) to O(n). This would be useful in the instance
; where you have collections of items rather than strict sets.
