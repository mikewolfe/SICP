; Exercise 2.50. Define the transformation flip-horiz, which flips painters
; horizontally, and transformations that rotate painters counterclockwise by 180
; degrees and 270 degrees.

; From previous exercises:
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (get-origin frame)
  (car frame))
(define (get-edge1 frame)
  (car (cdr frame)))
(define (get-edge2 frame)
  (cdr (cdr frame)))
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

; From the text:
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; Now lets define flip-horiz, this is analagous to flipping vertical
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) ; new origin
                     (make-vect 0.0 0.0) ; new edge1 (1.0, 0.0)
                     (make-vect 1.0 1.0))) ; new edge2 (0.0, 1.0)

; Now lets define rotate counterclockwise by 180 degrees
(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0) ; new origin
                     (make-vect 0.0 1.0) ; new edge1
                     (make-vect 1.0 0.0))) ; new edge2

; Finally lets define rotate counterclockwise by 270 degrees
(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
