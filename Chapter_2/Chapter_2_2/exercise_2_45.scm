; Exercise 2.45. Right-split and up-split can be expressed as instances of
; a general splitting operation. Define a procedure split with the property that
; evaluating
; (define right-split (split beside below)) 
; (define up-split (split below beside))
; produces procedures right-split and up-split with the same behaviors as the
; ones already defined.
;
; First lets take all the information from the last exercise:
;
; Here is the right-split procedure:
;
; (define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))
;
; (define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (-n 1))))
;        (below painter (beside smaller smaller)))))
;
; Now lets define split
;
; (define (split proc1 proc2)
;   (lambda (painter n)
;     (if (= n 0)
;         painter
;         (let ((smaller ((split proc1 proc2) painter (- n 1))))
;           (proc1 painter (proc2 smaller smaller))))))
