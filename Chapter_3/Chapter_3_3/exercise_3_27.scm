; Exercise 3.27: Memoization (also called tabulation) is a technique that
; enables a procedure to record, in a local table, values that have previously
; been computed. This technique can make a vast difference in the performance of
; a program. A memoized procedure maintains a table in which values of the
; previous calls are stored using as keys the arguments that produced the
; values. When the memoized provedure is asked to compute a value, it first
; checks the table to see if the value is already there and, if so, just returns
; that value. Otherwise, it computes the new value in the ordinary way and
; stores this in the table. As an example of memoization, recall from Section
; 1.2.2 the exponential process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

; The memoized version of the same procedure is

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            (( = n 1) 1)
            (else (+ (memo-fib (-n 1))
                     (memo-fib (- n 2))))))))

;where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
              (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              (result)))))))

; Draw an environment diagram to analyze the computation of (memo-fib 3).
; Explain why memo-fib computes the nth Fibonacci number in a number of steps
; proportional to n. Would the scheme still work if we had simply defined
; memo-fib to be (memoize fib)?
;
;This environment diagram is super complicated and I won't draw it here. Since
;you only need to compute each fib once (the table stores subsequent calcs) and
;assuming the lookup time in the table scales as O(n) or better, this will make
;the calculation for fib O(n). If you try to just memoize fib then the internal
;calls to fib will not use the lookup table and you will not get this better
;scaling
