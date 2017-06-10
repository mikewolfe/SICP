; Exercise 2.36: The procedure accumulate-n is similar to accumulate except that
; it takes as its third argument a sequence of sequences, which are all assumed
; to have the same number of elements. It applies the designated accumulation
; procedure to combin all the first elements of the sequences, all the second
; elements of the sequences, and so on, and returns a sequence of the results.
; For instance, if s is a sequence containing four sequences:
; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
; should be the sequence (22 26 30). Fill in the missing expressions in the
; following definition of accumulate-n:
;
; (define (accumulate-n op init seqs)
;   (if (null? (car seqs))
;       '()
;       (cons (accumulate op init <??>)
;             (accumulate-n op init <??>))))

; Once again, pulling the definition for accumulate:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Now defining the accumulate-n procedure:
; We want to accumulate the sum of each element of each list together. If we
; map car across each of the sequences then we will create a sequence of the
; first element from each sequence. Similarly if we map cdr across the sequences
; then we will get the rest of the sub sequences from each sequence
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Testing
(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
x
; should be (22 26 30)
(accumulate-n + 0 x)

