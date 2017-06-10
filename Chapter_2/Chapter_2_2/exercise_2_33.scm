; Exercise 2.33. Fill in the missing expressions to complete the following
; definitions of some basic list-manipulation operations as accumulations:
;
;(define (map p sequence)
;  (accumulate (lambda (x y) <??>) nil sequence))
;
;(define (append seq1 seq2)
;  (accumulate cons <??> <??>))
;
;(define (length sequence)
;  (accumulate <??> 0 sequence))

; First lets look at the accumulate procedure as defined in the book:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; redefine map in terms of accumulate
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

;Testing
(define (square x)
  (* x x))
(map square (list 1 2 3 4))

; redefine append in terms of accumulate
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; Testing
(append (list 1 2 3 4) (list 5 6 7 8))

; redfine length in terms of accumulate
(define (length sequence)
  (accumulate (lambda (x y) 
                (if (null? x) y)
                  (+ 1 y))
              0 sequence))
; Testing
(length (list 1 2 3 4))

