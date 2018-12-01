; Exercise 3.32: The procedures to be run during each time segment of the agenda
; are kept in a queue. Thus, the procedures for each segment are called in the
; order in which they were added to the agenda (first in, first out). Explain
; why this order must be used. In particular, trace the behavior of an and-gate
; whose inputs change from 0, 1 to 1, 0 and say how the behavior would differ
; if we stored a segment's procedures in an ordinary list, adding and removing
; procedures only at the front (last in, first out).

; If a list was used instead of a queue, then the order of operations would
; occur in the opposite direction resulting in the following:

; here is the definition of an and gate as is
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

; the output is changed if either a1 or a2 changes but they would be added
; to a list or queue as follows
(set-signal! a1 0)
(set-signal! a2 1)
(propagate)
; to start there is one a2 in the agenda at time x, this is evaluated and
; doesn't matter
(set-signal! a1 1)
(set-signal! a2 0)
(propagate)
; now two and-action procedures are added to the queue
; a1s action has (logical-and 1 1) and sets the output to 1
; a2s action has (logical-and 1 0) and sets the output to 0
; If FIFO is done then the output will be 1
; If LIFO is done then the output will be correctly 0

; a1 -> 1
; output = (logical_and a1 a2)
; a2 -> 0
; output = (logical_and a1 a2)
;
; If it was a queue a change from 0, 1 to 1, 0 would do the following:
; initial (0, 1)
; change a1 (1, 1) 
; add logical_and to queue
; change a2 (1, 0)
; add logical_and to queue
