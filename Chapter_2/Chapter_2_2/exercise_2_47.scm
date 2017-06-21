; Exercise 2.47 Here are two possible constructors for frames:
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
;For each constructor supply the appropriate selectors to produce an
;implementation for frames.

; Implementation 1:

(define (get-origin frame)
  (car frame))
(define (get-edge1 frame)
  (car (cdr frame)))
(define (get-edge2 frame)
  (car (cdr (cdr frame))))

; Implementation 2:

(define (get-origin frame)
  (car frame))
(define (get-edge1 frame)
  (car (cdr frame)))
(define (get-edge2 frame)
  (cdr (cdr frame)))

; This exercise just highlights the difference between lists and pairs
