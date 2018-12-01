; Exercise 3.29: Another way to construct an or-gate is as a compound digital
; logic device built from and-gates and inverters. Define a procedure or-gate
; that accomplishes this. What is the delay time of the or-gate in terms of
; and-gate-delay and inverter-delay.
;
;
; Copy over code from last problem:
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; there are quicker ways to write this with less comparisons but wanted to 
; keep it consistent with the logical-not procedure
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal s"))))

; We need to make an OR-gate with only AND and NOT
;
; S1  S2  AND  OR notS1  notS2  notS1 AND notS2    NOT(notS1 AND notS2)
; 1   1    1   1    0     0            0                     1
; 0   1    0   1    1     0            0                     1
; 1   0    0   1    0     1            0                     1
; 0   0    0   0    1     1            1                     1
;
; In gate format
;               ,\
;      S1 -----|  \   b   _____________
;              |  /o------|            `
;               `/        |             `
;                         |              `  d   ,\
;               ,\        |              |-----|  \         
;      S2 -----|  \   c   |             ,      |  /o------> output
;              |  /o------|            ,        `/        
;               `/        |___________,
;

(define (or-gate o1 o2 output)
  (let ((b (make-wire)) 
        (c (make-wire))
        (d (make-wire)))
    (inverter o1 b)
    (inverter o2 c)
    (and-gate b c d)
    (inverter d output)
    'ok))

; The amount of time to go through this or-gate would be inverter-delay +
; and-gate-delay + inverter-delay since the first two inverters happen
; at the same time
    

