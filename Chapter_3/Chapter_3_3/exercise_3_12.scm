; Exercise 3.12: The following procedure for appending lists was introduced
; in Section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
; append forms a new list by successively consing the elements of x onto y. The
; procedure append! is similar to append, but it is a mutator rather than
; a constructor It appends the lists by splicing them together, modifying the
; final pair of x so that its cdr is now y. (It is an error to call append!
; with an empty x.)
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; Here last-pair is a procedure that returns the last pair in an argument:
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

; Consider the interaction
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;(a b c d)
(cdr x)
; < response > ; response here should be (b)
(define w (append! x y))
w
; (a b c d)
(cdr x)
; < response > ; (b c d)
; What are the missing <response>s? Draw box-and-pointer diagrams to explain
; your answer.

; Original lists: x and y                    
;        x -> | | |-> b        y -> | | |-> d
;              |                     |       
;              a                     c       
;                                            
; Using append to make z does not impact x   
;      z-> | | | -> | | | -> | | | -> | | |  
;           |        |        |        |     
;           a        b        c        d     
;                                            
;                                            
; Using append! to make z                    
;                             y              
;                             |              
;   w,x -> | | | -> | | | -> | | | -> | | |  
;           |        |        |        |     
;           a        b        c        d     
;                                            
;                                            
; append! changes x's cdr
