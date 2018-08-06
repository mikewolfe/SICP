;Exercise 3.2: In software-testing applications, it is useful to be able to
;count the number of times a given procedure is called during the course of
;a computation. Write a procedure make-monitored that takes as input
;a procedure, f, that itself takes one input. The result returned by
;make-monitored is a third procedure, say mf, that keeps track of the number of
;times it has been called by maintaining an internal counter. If the input to mf
;is the special symbol how-many-calls?, then mf returns the value of the
;counter. If the input is the special symbol reset-count, then mf rests the
;counter to zero. For any other input, mf returns the result of calling f on
;that input and increments the counter. For instance, we could make a monitored
;version of the sqrt procedure:
;
; (define s (make-monitored sqrt))
; (s 100)
; 10
; (s 'how-many-calls?)
; 1

(define (make-monitored proc)
  ; initialize count to zero. For reasons I don't understand fully yet, if you
  ; use the line
  ; (set! count 0)
  ; instead of whats below, the count variable is shared amongst the
  ; procedures
  (define count 0)
  ; define a dispatcher to allow for different things to be returned
  (define (mf m)
    ; return the count if requested
    (cond ((eq? m 'how-many-calls?) count)
          ; reset the count to zero
          ((eq? m 'reset-zero) (set! count 0))
          ; otherwise increment the count and run the procedure
          (else (begin (set! count (+ count 1))
                                   (proc m)))))
  mf)

; test to make sure the count is not shared amongst procedures
(define s (make-monitored sqrt))
(define q (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(q 'how-many-calls?)
(q 20)
(s 'how-many-calls?)
(q 'how-many-calls?)
(s 20)
(s 'how-many-calls?)
(q 'how-many-calls?)
(s 'reset-zero)
(s 'how-many-calls?)
(q 'how-many-calls?)
