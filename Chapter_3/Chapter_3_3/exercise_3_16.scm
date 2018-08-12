; Exercise 3.16: Ben Bitdiddle decides to write a procedure to count the number
; of pairs in any list structure. "It's easy," he reasons, "The number of pairs
; in any structure is the number in the car plus the number in the cdr plus
; one more to count the current pair." So Ben writes the following procedure:
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Show that this procedure is not correct. In particular, draw box-and-pointer
; diagrams representing list structures made up of exactly three pairs for which
; Ben's procedure would return 3; return 4; return y; never return at all.

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
;(count-pairs rinf)
