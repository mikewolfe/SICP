; Exercise 2.62. Give a (n) implementation of union-set for sets represented as
; ordered lists.

; Copying over code from the last exercise:

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Lets look at what union-set was before it was ordered lists:
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons 
                (car set1) 
                (union-set (cdr set1) set2)))))

; Now that order matters, we need to build up the new list in order. This
; is basically the same thing as intersection-set except we will build
; up the list with all the elements that are NOT in both sets as well.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1)
                                       (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1)
                                       set2)))
                  ((< x2 x1)
                   (cons x2 (union-set set1
                                       (cdr set2)))))))))
;tests
(union-set '(1 2 3 4) '(1 2 3 4))
(union-set '(1 2 3 4) '(1 2 5 6 7 8))
(union-set '() '(1 2 3 4))
(union-set '(1 2 3 4) '())
(union-set '() '())
