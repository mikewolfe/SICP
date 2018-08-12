; Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes only a 
; amount of space. (This requires a very clever idea.)

; I couldn't figure this out on my own, but apparently this is a well known
; problem that comes up in job inteviews. The canonical solution is the 
; tortoise and the hare algorithm. Adapted from here:
;http://community.schemewiki.org/?sicp-ex-3.19

(define (cycle? lst)
  ; allow cdring to be safe on non-pairs as well
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (chase tortoise hare)
          ; either reaches an end, then return false
    (cond ((or (not (pair? tortoise)) (not (pair? hare))) #f)
          ; reach the same location return true
          ((eq? tortoise hare) #t)
          (else (chase (safe-cdr tortoise) 
                       (safe-cdr (safe-cdr hare))))))
  (chase (safe-cdr lst) (safe-cdr (safe-cdr lst))))

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
; test all previous examples

; Test cases from 3.16
; return 3
;                             
; r3 -> | | |-> | | | -> | | |
;        |       |        |   
;        a       b        c   
;                             
(define r3 (list 'a 'b 'c))
r3
(cycle? r3)

; return 4
;                  
;  r4->| | |       
;       |          
;   y->| | |       
;       |_|        
;       |          
;   x->| | |       
;       |          
;       a          
(define x (list 'a))
(define y (cons x x))
(define r4 (list y))
r4
(cycle? r4)

; return 7
;                  
;  r7->| | |       
;       |_|        
;       |          
;   y->| | |       
;       |_|        
;       |          
;   x->| | |       
;       |          
;       a          
(define r7 (cons y y))
r7
(cycle? r7)

; infinite loop
;          _______________  
;         |               | 
;  rinf->| | |->| | |->| | |
;         |      |      |   
;         a      b      c   
(define rinf (list 'a 'b 'c))
(set-cdr! (cdr (cdr rinf)) rinf)
;rinf
(cycle? rinf)
