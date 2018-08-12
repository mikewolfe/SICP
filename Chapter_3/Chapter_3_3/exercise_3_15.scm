; Exercise 3.15: Draw box-and-pointer diagreams to explain the effect of 
; set-to-wow! on the structures z1 and z2 above.

(define (set-to-wow! x) (set-car! (car x) 'wow') x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
z1
;((a b) a b)
(set-to-wow! z1)
;((wow b) wow b)
z2
;((a b) a b)
(set-to-wow! z2)
; ((wow b) a b)

; x box and pointer
;
; x -> | | | -> | | |
;       |        |
;       a        b
;
; z1 box and pointer
;
;  z1 -> | | |
;         |_|
;         |
;   x -> | | | -> | | |
;         |        |
;         a        b
; set to wow changes the car of z1 which changes a to wow for both parts of
; the list
;
; z2 box and pointer
;
; z2->| | | -> | | | -> | | |
;      |        |        |   
;      |        a        b   
;      |        |        |   
;      |______>| | | -> | | |
;
;here set-to-wow! only changes the car of z2 thereby only impacting the first
;
