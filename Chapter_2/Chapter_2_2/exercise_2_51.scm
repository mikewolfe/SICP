; Exercise 2.51. Define the below operation for painters. Below takes two
; painters as arguments. The resulting painter, given a frame, draws with the
; first painter in the bottom of the frame and with the second painter in the
; top. Define below in two different ways -- first by writing a procedure that
; is analogous to the beside procedure given above, and again in terms of beside
; and suitable rotation operations (from exercise 2.50).

; Copying over the start of 2.50:

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

; Now lets try to define Below:

; Here is beside as a guide:

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; We will basically follow this scaffold for below, we simply just have to move
; the split point and the vectors correspondingly

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

; However it wants us to define it agin in terms of suitable rotation operations
; from the previous problem. Here are those rotations:

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
(define (rotate-90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; Now to define below in terms of rotations, we just need to rotate the pictures
; to set them "on top" of each other and rotate the final image to be "upright"
(define (below painter1 painter2)
  (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2))))



