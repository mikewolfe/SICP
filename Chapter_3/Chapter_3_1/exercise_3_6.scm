; Exercise 3.6: It is useful to be able to reset a random-number generator to
; produce a sequence starting from a given value. Design a new rand procedure
; that is called with an argument that is either the symbol generate or the
; symbole reset and behaves as follows: (rand 'generate) produces a new random
; number; ((rand 'reset) <new-value>) resets the internal state variable to
; the designated <new-value>. Thus, by resetting the state, one can generate
; repeatable sequences. These are very handy to have when testing and
; debugging programs that use random numbers.


; here was the definition for rand before
;(define rand (let ((x random-init))
;               (lambda ()
;                 (set! x (rand-update x))
;                 x)))
; However, since rand-update is never defined, we have to define it based
; on the footnote procedure. For now I will just choose a b and m based on
; nothing but the numbers that came to my head. But I will need to go track
; down chapter 3 in Knuth to see what are good choices of a b and m

(define (rand-update x)
  (let ((a 42) (b 27) (m 111))
    (modulo (+ (* a x) b) m)))

(define (rand seed)
  ; make a procedure to update the seed value
  (define (rand-reset value)
    (set! seed value))
  ; make a procedure to generate a random number
  (define (rand-generate)
    (set! seed (rand-update seed))
    seed)
  ; dispatcher for using procedures
  (define (dispatch char)
    (cond ((eq? char 'generate) (rand-generate))
          ((eq? char 'reset) rand-reset)
          (else (error "Unknown request: RAND"))))
  dispatch)

; ok lets test this and make sure it is working
(define rand-1 (rand 42))
(define rand-2 (rand 42))
(rand-1 'generate)
(rand-1 'generate)
(rand-1 'generate)
(rand-2 'generate)
((rand-1 'reset) 42)
(rand-1 'generate)
(rand-2 'generate)
