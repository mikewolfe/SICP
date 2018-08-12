; Exercise 3.17: Devise a correct version of the count-pairs procedure of
; Exercise 3.16 that returns the number of distinct pairs in any structure.
; (Hint: Traverse the structure, maintaining an auxiliary data structure that
; is used to keep track of which pairs have already been counted.)

(define (count-pairs x)
  ; define an auxillary structure to store encountered pairs
  (define counted '())
  ; loop through the whole list checking if we've already encountered a pair
  ; and skipping it if we have
  (define (check-and-count x)
    (if (or (not (pair? x)) (memq x counted))
        0
        (begin                
          ; update the counted list with the current pair
          (set! counted (cons x counted))
          (+ (check-and-count (car x))
             (check-and-count (cdr x))
             1))))
    (check-and-count x))
  
; Test cases from 3.16
; return 3
;                             
; r3 -> | | |-> | | | -> | | |
;        |       |        |   
;        a       b        c   
;                             
(define r3 (list 'a 'b 'c))
r3
(count-pairs r3)

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
(count-pairs r4)

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
(count-pairs r7)

; infinite loop
;          _______________  
;         |               | 
;  rinf->| | |->| | |->| | |
;         |      |      |   
;         a      b      c   
(define rinf (list 'a 'b 'c))
(set-cdr! (cdr (cdr rinf)) rinf)
;rinf
(count-pairs rinf)
