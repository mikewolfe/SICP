; Exercise 2.41. Write a procedure to find all ordered triples of distinct
; positive intergers i, j, and k less than or equal to a given integer n that
; sum to a given integer s.

; I am pulling my example from 2.40 to build off of

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

(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; This basically follows the unique-pairs procedure, we just add an additional
; loop
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                         (map (lambda (k) (list i j k))
                              (enumerate-interval 1 (- j 1))))
             (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
; Testing
(unique-triples 4)

; Now we need to test for summing to a given integer
; Here we see if the sum of the triple equals the integer s that we are testing
(define (integer-sum? triple s)
  (= (accumulate + 0 triple) s))

; Now we just need to filter for the triples that actually meet that criteria

(define (find-unique-triple-sum n s)
  (filter (lambda (x) (integer-sum? x s))
          (unique-triples n)))

; Testing
(find-unique-triple-sum 4 8)
(find-unique-triple-sum 5 12)
(find-unique-triple-sum 10 6)
(find-unique-triple-sum 12 12)



