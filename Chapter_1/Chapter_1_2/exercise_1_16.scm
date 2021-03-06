; Exercise 1.16 Design a procedure that evolves an iterative exponentiation
; process that uses succesive squaring and uses a logarithmic number of steps as
; does fast-expt. (Hint: Using the observation that (b^n/2)^2 = (b^2)^n/2, keep,
; along with the exponent n and the base b, an additional state variable a, and
; define the state transformation in such a way that the product a*b^n is 
; unchanged from state to state. At the beginning of the process a is taken to
; be 1, and the answer is given by the value of a at the end of the process.
; In general, the technique of defining an invariant quantity that remains 
; unchanged from state to state is a powerful way to think about the design of
; iterative algorithims.)

; Here is the old recursive procedure that is O(log(n)) in both space and time:

(define (fast-expt b n)
  (cond ((=n 0) 1)
        ((even? n) (square (fast-expt b(/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))

; Here is an example of iteration for exponents:

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))

; Here is the iteration solution for the first algorithim

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter base power product)
  (cond ((= power 0) product)
        ((even? power) (expt-iter (square base) (/ power 2) product))
        ( else (iter base (- power 1) (* product base)))))

; iterative version
; (expt 2 2)
; (expt-iter 2 2 1)
; (expt-iter 4 1 1)
; (expt-iter 4 0 4)
; 4
; recursive version
; (expt 2 2)
; (square (expt 2 1))
;         (* 2 (expt 2 0))
;               1



