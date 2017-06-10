; Exercise 2.39. Complete the following definitions of reverse (exercise 2.18)
; in terms of fold-right and fold-left from exercise 2.38
;
; (define (reverse sequence)
;   (fold-right (lambda (x y) <??>) '() sequence))
;
; (define (reverse sequence)
;   (fold-left (lambda (x y) <??>) '() sequence))

; Here was the definition of reverse from exercise 2.18
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

; First lets tackle the fold-right version

; append adds to the end of a list. We just append each element to the end
; of the list as we go through every element in the list
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
; test
(reverse (list 1 2 3))

; fold-left starts at the end, so we can just construct the list in reverse
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
; test
(reverse (list 1 2 3))


