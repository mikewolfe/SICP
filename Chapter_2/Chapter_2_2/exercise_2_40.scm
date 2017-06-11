; Exercise 2.40. Define a procedure unique-pairs that, given an integer n,
; generates the sequence of pairs (i,j) with 1 <= j< i <= n. Use unique-pairs
; to simplify the definition of prime-sum-pairs given above.

; First I am grabbing some functions from the text:


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
; Test
(flatmap list (list 1 2 3))
(enumerate-interval 1 10)

; Here is how they do unique pairs in the text:

;(accumulate append
;            '()
;            ; here is your nested loop through i and j
;            (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 n)))

; Here is my solution using the flatmap procedure, it is just a rewriting of the
; book procedures in a more managable form
(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Testing
(unique-pairs 10)

; Now using this procedure to simplify prime-sum-pairs:

; I need to pull my fast-prime procedure from exercise 1.28:

(define (square-check x m)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (* x x) m) 1))
            0
        (remainder (* x x) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check (expmod base (/ exp 2) m)
                        m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime-sum? pair)
  (fast-prime? (+ (car pair) (cadr pair)) 10))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
; Testing
; should be ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
(prime-sum-pairs 6)


