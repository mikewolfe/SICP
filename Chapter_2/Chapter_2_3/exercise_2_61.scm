; Exercise 2.61. Give an implementation of adjoin-set using the ordered
; representation. By analogy with element-of-set? show how to take advantage of
; the ordering to produce a procedure that requires on the average about half as
; many steps as with the unordered representation.

; Here was both element-of-set? and intersection-set from the text for ordered
; list representation of a set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; Now lets define adjoin-set to take advantage of the ordered representation

; This was the old procedure:
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; We can't just check for membership of the set anymore, we need to put the
; new value into the sorted list
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; testing
(adjoin-set 3 '(1 2 3 4 5))
(adjoin-set 3 '())
(adjoin-set 3 '(4 5 6))
