;Exercise 3.18: Write a procedure that examines a list and determines whether
;it contains a cycle, that is, whether a program that tried to find the
;end of a list by taking successive cdrs would go into an infinite loop. 
;Exercise 3.13 constructed such lists.

(define (cycle? x)
  ; define an auxillary structure to store encountered pairs
  (define counted '())
  ; loop through the whole list checking if we've already encountered a pair
  ; and returning true if we have
  (define (check x)
    (cond ((not (pair? x)) #f)
          ((memq x counted) #t)
          (else
            (begin
              ; update the counted list with the current pair
              (set! counted (cons x counted))
              (or (check (car x)) (check (cdr x)))))))
    (check x))

; Make cycle procedure from exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

; test cycle predicate
(define x (list 'a 'b 'c))
(define z (make-cycle (list 'a 'b 'c)))
(cycle? x)
(cycle? z)


