; Exercise 2.83. Suppose you are designing a generic arithmetic system for
; dealing with the tower of types shown in figure 2.25: integer, rational, real,
; complex. For each type (except complex), design a procedure that raises
; objects of that type one level in the tower. Show how to install a generic
; raise operation that will work for each type (except complex).

; Rolled back the tag handling code to before we had it use numbers
; Tag handling code:

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

; Operation table code (taken from stack overflow since its not defined yet)
(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

; Rolled back the apply-generic code to the example of before having coercion
; apply-generic code and removed coercion table. Raise will be defined in
; each package (though it will be dependent on the other packages, so this
; may cause an issue)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; install normal numbers, both integer and float flavored
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tab (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag (round x))))
  ;;; added equality
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  ;; added =zero?
  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))
  ;; add exponentiation only to number package
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; add raise function
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  'done)

;external constructor
(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tab (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  ;;; added equality
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  ;; added =zero?
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  ;; add exponentiation only to number package
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; add raise function
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  'done)


;external constructor
(define (make-real n)
  ((get 'make 'real) n))

; install rational numbers

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;;; added for equ (does not consider simplification, but since gcd is called
  ;;; when the number is made, we don't have to)
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;;; added =zero? function
  (define (=zero?-rat x)
    (= 0 (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;;; added equality
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  ;; added =zero?
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  ;; added numer and denom selectors
  (put 'numer '(rational) (lambda (r) (numer r)))
  (put 'denom '(rational) (lambda (r) (denom r)))
  ;; add raise function
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  'done)

; external constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
; external selector
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

; pull the entire complex number package:

(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Define selectors globally as follows
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
; Define constructors globally as
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; Now install the complex number package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;;; add equality function (does not consider numerical error)
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;;; add =zero? function
  (define (=zero?-complex z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;;; add equ function
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
  ;;; add =zero? function
  (put '=zero? '(complex)
       (lambda (z) (=zero?-complex z)))
  ; added selectors
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

; accessing the constructors from outside complex
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; generic operators
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
;; add the generic zero predicate
(define (=zero? x) (apply-generic '=zero? x))
;; add generic exponentiation
(define (exp x y) (apply-generic 'exp x y))
;; add generic raise operation
(define (raise x) (apply-generic 'raise x))

; Testing
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; try raising all the way up
(raise (make-integer 1))
(raise (raise (make-integer 1)))
(raise (raise (raise (make-integer 1))))
; (raise (raise (raise (raise (make-integer 1)))))
