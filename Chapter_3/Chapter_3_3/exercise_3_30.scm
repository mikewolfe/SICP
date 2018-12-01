; Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed by stringing
; together n full-adders. This is the simplest form of parallel adder for adding
; two n-bit binary numbers. The inputs A1, A2, A3, ..., An and B1, B2, B3, ...,
; Bn are the two binary numbers to be added (each Ak and Bk is a 0 or a 1). The
; circuit generates S1, S2, S3, ..., Sn, the n bits of the sum, and C, the carry
; from the addition. Write a procedure ripple-carry-adder that generates this
; circuit. The procedure should take as arguments three lists of n wires
; each-the Ak, the Bk, and the Sk-and also another wire C. The major drawback of
; the ripple-carry adder is the need to wait for the carry signals to propagate.
; What is the delay needed to obtain the complete output from an n-bit
; ripple-carry adder, expressed in terms of the delays for and-gates, or-gates,
; and inverters?


; In order to make the ripple-carry adder, we will need both the half-adder and
; the full-adder. We will assume that or-gate and-gate and inverters have
; already been defined. As well as make-wire
;
; First the half-adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; Next a full-adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Finally ripple carry adder
(define (ripple-carry-adder Ak Bk Sk C)
  (define (add-a-group a b cin s)
    ; need to make the new wire between full-adders each iteration
    (let ((cout (make-wire)))
      ; assumes Ak Bk Sk are all length k. no code to check that
      (cond ((or (null? a) (null? b) (null? s))
             ; make sure the last cout is set to 0
             (set-signal! cout 0)
             'ok)
            (else 
              ; connect a new full adder to the previous cout
              (full-adder (car a) (car b) cin (car s) cout)
              ; connect a new full adder to the cout
              (add-a-group (cdr a) (cdr b) cout (cdr s))))))
  ; start with cin as the first c
  (add-a-group Ak Bk C Sk)) 

; For one half-adder: or-gate and and-gate first (whichever is longer)
; half-adder = max(or-gate, and-gate + inverter) + and-gate
; full-adder = half_adder + half_adder + or-gate
; full-adder = max(or-gate, and-gate + inverter) + and-gate + max(or-gate, and-gate +
;              inverter) + and-gate + or-gate
;            = 2*(max(or-gate, and-gate + inverter) + and-gate) + or-gate
; ripple-carry-adder_n = n(full-adder)
;                      = 2n*(max(or-gate, and-gate + inverter) + and-gate) + n*or-gate
