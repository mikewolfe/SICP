; Exercise 2.59. Implement the union-set operation for the unordered-list
; representation of sets.

; First lets pull all the procedures from the text:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Now similarly define union-set

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons 
                (car set1) 
                (union-set (cdr set1) set2)))))

; lets do some testing
; should be '(1 2 3 4 5 6 9)
(union-set '(1 2 3 4) '(1 5 9 6 2))
; check the nulls
(union-set '(1 2 3 4) '())
(union-set '() '(1 5 9 6 2))
(union-set '() '())


; should be '(1 2)
(intersection-set '(1 2 3 4) '(1 5 9 6 2))
