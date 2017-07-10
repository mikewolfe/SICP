; Exercise 2.81. Louis Reasoner has noticed that apply-generic may try to coerce
; the arguments to each other’s type even if they already have the same type.
; Therefore, he reasons, we need to put procedures in the coercion table to
; "coerce" arguments of each type to their own type. For example, in addition to
; the scheme-number->complex coercion shown above, he would do:
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion ’scheme-number ’scheme-number
;              scheme-number->scheme-number)
;(put-coercion ’complex ’complex complex->complex)
;
;a. With Louis’s coercion procedures installed, what happens if apply-generic is
;called with two arguments of type scheme-number or two arguments of type
;complex for an operation that is not found in the table for those types? For
;example, assume that we’ve defined a generic exponentiation operation:
;(define (exp x y) (apply-generic ’exp x y))
;and have put a procedure for exponentiation in the Scheme-number package but
;not in any other package:
;; following added to Scheme-number package
;(put ’exp ’(scheme-number scheme-number)
;(lambda (x y) (tag (expt x y)))) ; using primitive expt
;What happens if we call exp with two complex numbers as arguments?


;b. Is Louis correct that something had to be done about coercion with arguments
;of the same type, or does apply-generic work correctly as is?


;c. Modify apply-generic so that it doesn’t try coercion if the two arguments
;have the same type.

;; Pull all previous code. Modify apply generic to the new one with coersion,
;; add coersion to the system

;;;; MODIFIED TAG CODE TO USE SCHEME'S TYPING FOR NUMBERS ;;;
; all we need to do is add a number? clause to each of the tag procedures
; such that all of the code can handle normal numbers without adding tags
; to them
; Tag handling code:
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
  (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

; Operation table code (taken from stack overflow since its not defined yet)
(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

; Define a coercion table
(define *co-table* (make-hash-table))
(define (put-coercion type1 type2 proc)
  (hash-table/put! *co-table* (list type1 type2) proc))
(define (get-coercion type1 type2)
  (hash-table/get *co-table* (list type1 type2) #f))

; apply-generic code WITH COERCION
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else
                          (error "No method for these types"
                                 (list op type-tags))))))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags)))))))

; install normal numbers
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;;; added equality
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ;; added =zero?
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  ;; add exponentiation only to number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt

  'done)


;external constructor
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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
  'done)

; external constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))

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

;; add coercion of scheme number to complex number
(put-coercion 'scheme-number 'complex
              (lambda (x) (make-complex-from-real-imag (contents x) 0)))

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

; Testing
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; try coercion
(add (make-complex-from-real-imag 3 4) 3)
(exp 3 3)
;(exp (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))

; without his procedures put in, it raises the following error:
;No method for these types (exp (complex complex))

;With his procedures put in, it gets stuck in an infinite recursion, constantly
;converting the type to itself
;(put-coercion 'scheme-number 'scheme-number (lambda (n) n))
;(put-coercion 'complex 'complex (lambda(z) z))
;(add (make-complex-from-real-imag 3 4) 3)
;(exp 3 3)
;(exp (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))

; b. apply generic works correctly as is, as long as there is no attempt to
; convert to the same type.

; c. In order to make the infinite recursion not occur, apply generic can be
; modified like so:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ; add an equal type checker
                (if (eq? type1 type2)
                  (error "No method for these types" (list op type-tags)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else
                          (error "No method for these types"
                                 (list op type-tags))))))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags)))))))

(put-coercion 'scheme-number 'scheme-number (lambda (n) n))
(put-coercion 'complex 'complex (lambda(z) z))
(add (make-complex-from-real-imag 3 4) 3)
(exp 3 3)
(exp (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))

