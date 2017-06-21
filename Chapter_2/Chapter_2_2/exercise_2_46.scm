; Exercise 2.46 A two-dimensional vector v running from the origin to a point
; can be represented as a pair consisting of an x-coordinate and a y-coordinate.
; Implement a data abstraction for vectors by giving a constructor make-vect and
; corresponding selectors xcor-vect and ycor-vect. In terms of your selectors
; and constructor, implement procedures add-vect, sub-vect, and scale-vect that
; perform the operations vector addition, vector subtraction, and multiplying
; a vector by a scalar:
;
; (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
; (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
; s * (x, y) = (sx, sy)

; Vectors can just be simple pairs, the constructors and selectors are easy:

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

; The addition and subtraction procedures are also straight forward. As well
; as the scale-vect procedure

(define (add-vect x y)
  (make-vect (+ (xcor-vect x) (xcor-vect y))
             (+ (ycor-vect x) (ycor-vect y))))

(define (sub-vect x y)
  (make-vect (- (xcor-vect x) (xcor-vect y))
             (- (ycor-vect x) (ycor-vect y))))

(define (scale-vect s vect)
  (make-vect (* (xcor-vect vect) s)
             (* (ycor-vect vect) s)))

; Testing

(define vect1 (make-vect 1 2))
(define vect2 (make-vect 3 4))

; Should be 1
(xcor-vect vect1)
; Should be 2
(ycor-vect vect1)

; should be (4 6)
(add-vect vect1 vect2)

; should be (2 2)
(sub-vect vect2 vect1)

; should be (6 8)
(scale-vect 2 vect2)


