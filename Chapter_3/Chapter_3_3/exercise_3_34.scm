; Exercise 3.34: Louis Reasoner wants to build a squarer, a constraint device
; with two terminals such that the value of connector b on the second terminal
; will always be the square of the value a on the first terminal. He proposes
; the follwoing simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

; There is a serious flaw in this idea. Explain.
;
; Multiplier is set up so that, if it is given 2 of the values it will 
; determine the third. However, for a squarer you really only need one of the
; values to determine the other. As written above, the squarer would fail to
; find out the value of a if only b was given since multiplier is expecting
; all two of the three values to be set.
