; Exercise 2.48. A directed line segment in the plane can be represented as
; a pair of vectors -- the vector running from the origin to the start-point of
; the segment, and the vector running from the origin to the end-point of the
; segment. Use your vector representation from exercise 2.46 to define
; a representation for segments with a constructor make-segment and selectors
; start-segment and end-segment.
;
; Here is my vector representation from 2.46:
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

; Making segments is as simple as making a pair of vectors
(define (make-segment x y)
  (cons x y))
(define (start-segment x)
  (car x))
(define (end-segment y)
  (cdr y))
