; Exercise 2.35. Redefine count-leaves from section 2.2.2 as an accumulation:
;
; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

; accumulate procedure as before
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
; First lets pull over the old count-leaves procedure:

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Now lets convert this to using map and accumulate. We need a way to get the
; sequence of leaves from the tree. There was a procedure in the text that
; obtained this:

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
; This procedure gives us all the leaves of the tree in sequence. Now we just
; have to add up 1 for every value in the sequence.

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;Tests
; should be 6
(count-leaves (list 1 2 3 4 5 6))
; should be 11
(count-leaves (list (list 1 2 3 (list 4 5 6) 7 8 (list 9 10 11))))

