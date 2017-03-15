; Exercise 1.32 a. Show that sum and product (exercise 1.31) are both special
; cases of a still more general notion called accumulate that combines a 
; collection of terms, using some general accumulation function:
;
; (accumulate combiner null-value term a next b)
;
; Accumulate takes as arguments the same term and range specifications as sum
; and product, together with a combine procedure (of two arguments) that 
; specifies how the current term is to be combined with the accumulation of the
; preceding terms and a null-value that specifies what base value to use when
; the terms run out. Write accumulate and show how sum and product can both
; be defined as simple calls to accumulate.


; here is the accumulate recursive version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (define (combine_f x y) (+ x y))
  (accumulate combine_f 0 term a next b))

(define (product term a next b)
  (define (combine_f x y) (* x y))
  (accumulate combine_f 1 term a next b))

(define (inc x) (+ x 1))
(define (identity x) x)
; testing
(sum identity 1 inc 5)
(product identity 1 inc 5)

; b. If your accumulate procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

(define (accumulate1 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum1 term a next b)
  (define (combine_f x y) (+ x y))
  (accumulate1 combine_f 0 term a next b))

(define (product1 term a next b)
  (define (combine_f x y) (* x y))
  (accumulate1 combine_f 1 term a next b))

; testing
(sum1 identity 1 inc 5)
(product1 identity 1 inc 5)
