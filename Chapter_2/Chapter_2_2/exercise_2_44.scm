; Exercise 2.44 Define the procedure up-split used by corner-split. It is
; similar to right-split, except that it switches the roles of below and beside.
;
; Here is the right-split procedure:
;
; (define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))
;
; Here is the full procedure for corner-split:
;
; (define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;        (let ((top-left (beside up up))
;              (bottom-right (below right right))
;              (corner (corner-split painter (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right corner)))))
;
; So then up-split would be the following procedure:
;
; (define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (-n 1))))
;        (below painter (beside smaller smaller)))))
;
; As the question suggests, we just switch beside and below.
