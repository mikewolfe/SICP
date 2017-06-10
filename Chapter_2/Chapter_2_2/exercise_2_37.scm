; Exercise 2.37. Suppose we represent vectors v = (vi) as sequences of numbers,
; and matrices m = (mij) as sequences of vectors (the rows of the matrix). For
; example, the matrix
;  _     _
;| 1 2 3 4 |
;| 4 5 6 6 |
;| 6 7 8 9 |
; _      _
;
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
; representation, we can use sequence operations to concisely express the basic
; matrix and vector operations. These operations (which are described in any
; book on matrix algebra) are the following:
;
; (dot-product v w) returns the sum of vi*wi
;
; (matrix-*-vector m v) returns the vector t, where ti = sum(m_ij * vj)
;
; (matrix-*-matrix m n) returns the matrix p, where pij = sum(m_ik*n_kj)
;
; (transpose m) returns the matrix n, where n_ij = m_ji
;
; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure accumulate-n is defined in exercise
; 2.36.)
;
; (define (matrix-*-vector m v)
;   (map <??> m))
;
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
;
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map <??> m)))

; First lets copy over the accumulate-n function:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Now the matrix-*-vector function
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

; Tests
(define x (list 1 2 3))
(define y (list 4 5 6))
; should be 32
(dot-product x y)

(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; 1 2 3   1    14
; 4 5 6 * 2  = 32
; 7 8 9   3    50
(matrix-*-vector A x)

; Next the transpose function, all we need to do here is reconstruct the matrix
; by converting the rows into columns and columns into rows. The accumulate 
; function will construct a new column from a row. The accumulate-n then pulls
; them together into a new matrix
(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) '() mat))

; 1 2 3    1 4 7
; 4 5 6 '= 2 5 8
; 7 8 9    3 6 9
;
;((1 4 7) (2 5 8) (3 6 9))
; Tests
(transpose A)

; Finally the matrix multiplication function

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

; 1 2 3   1 2 3   30  36  42
; 4 5 6 * 4 5 6 = 66  81  96 
; 7 8 9   7 8 9   102 126 150

(matrix-*-matrix A A)

