;Exercise 2.52. Make changes to the square limit of wave shown in figure 2.9 by
;working at each of the levels described above. In particular: 
;a. Add some segments to the primitive wave painter of exercise 2.49 (to add
;a smile, for example). 
;b. Change the pattern constructed by corner-split (for example, by using only
;one copy of the up-split and right-split images instead of two).  
;c.Modify the version of square-limit that uses square-of-four so as to assemble
;the corners in a different pattern. (For example, you might make the big Mr.
;Rogers look outward from each corner of the square.)

; Since I don't have a working picture language on this computer (don't have 
; the draw line function) I will just describe how to do each change.
;
; a. Adding a smile is as simple as adding new line segments to the list
; b. Changing the pattern constructed by corner-split is also easy. Lets look
; at the function for corner-split:
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
; Here we could just make the (- n x) have different x's to change how much
; splitting actually occurs

; c. modifying square-limit can also be acheived easily:

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
; Here we could change any of the flip-horiz or flip-vert calls to change
; how the pattern looks.


