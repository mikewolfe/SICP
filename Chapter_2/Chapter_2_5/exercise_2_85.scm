;Exercise 2.85. This section mentioned a method for ‘‘simplifying’’ a data
;object by lowering it in the tower of types as far as possible. Design
;a procedure drop that accomplishes this for the tower described in exercise
;2.83. The key is to decide, in some general way, whether an object can be
;lowered. For example, the complex number 1.5 + 0i can be lowered as far as
;real, the complex number 1 + 0i can be lowered as far as integer, and the
;complex number 2 + 3i cannot be lowered at all. Here is a plan for determining
;whether an object can be lowered: Begin by defining a generic
;operation project that ‘‘pushes’’ an object down in the tower. For example,
;projecting a complex number would involve throwing away the imaginary part.
;Then a number can be dropped if, when we project it and raise the result back
;to the type we started with, we end up with something equal to what we started
;with. Show how to implement this idea in detail, by writing a drop procedure
;that drops an object as far as possible. You will need to design the various
;projection operations and install project as a generic operation in the system.
;You will also need to make use of a generic equality predicate, such as
;described in exercise 2.79. Finally, use drop to rewrite apply-generic from
;exercise 2.84 so that it ‘‘simplifies’’ its answers.

; Copying all the code from 2.84

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

; adding a helper function to raise all types to the highest type
(define (find-highest-level args)
  (define (iter args highest-level)
    (if (null? args)
        highest-level
        (let ((this-level (level (car args))))
          (if (>= this-level  highest-level)
            (iter (cdr args) this-level)
            (iter (cdr args) highest-level)))))
  (iter args 0))

;helper function to raise a list of args to a particular level
(define (raise-to this-level args)
  (define (iter args final-args)
    (if (null? args)
      (reverse final-args)
      (let ((this-arg (car args)))
        (cond ((= (level this-arg) this-level)
              (iter (cdr args) (cons this-arg final-args)))
              ((< (level this-arg) this-level)
              (iter (cons (raise this-arg) (cdr args)) final-args))
              (else (error "Cannot raise argument to lower type" 
                           (list this-arg this-level))))))) 
  (iter args '()))

; drop helper function. Drops a type down as far as it will go.
(define (drop num)
  (if (= (level num) 1)
    num
    (let ((projection (project num)))
      (if (equ? (raise projection) num)
        (drop projection)
        num))))
  

; tests if every element in a list is the same. If it is then return true. 
; assumes only lists are ever passed into it
(define (all-eq? lst)
  (cond ((null? (cdr lst))
         #t)
        ; if every value is the same as the value next to it then the
        ; entire list is the same
        ((eq? (car lst) (cadr lst))
         (all-eq? (cdr lst)))
        (else #f)))
; Since I don't want all of my generic function dropping their type when they
; don't have to, I won't add the drop here, but rather to each of the functions
; that I want to simplify on.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (all-eq? type-tags)
            (error "No method for these types" (list op type-tags))
          ; otherwise we try to coerce all the arguments to the same type
            (apply apply-generic 
                   (cons op (raise-to (find-highest-level args) args))))))))

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
  ;; add level function
  (put 'level '(integer) (lambda (x) 1))
  (put 'project '(integer) (lambda (x) x))
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
  ;; add level function
  (put 'level '(real) (lambda (x) 3))
  ;; add projection function pulled this one from a blog as I wanted to know
  ;; how to convert real numbers to rational numbers using some built-ins
  (put 'project '(real) (lambda (x) (let ((value (inexact->exact x)))
                                      (cond ((integer? value)
                                             (make-rational value 1))
                                            ((rational? value)
                                             (make-rational (numerator value)
                                                            (denominator value)))
                                             (else (make-rational 
                                                     (round value) 1))))))
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
  ;; add level function
  (put 'level '(rational) (lambda (x) 2))
  ;; add projection function
  (put 'project '(rational) (lambda (x) (make-integer (round (/ (numer x)
                                                                (denom x))))))
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
  (put 'level '(complex) (lambda (x) 4))
  ; add projection function
  (put 'project '(complex) (lambda (x) (make-real (real-part x))))
  'done)

; accessing the constructors from outside complex
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; generic operators
; add the drops here for simplification, that way it won't drop types on me
; when I am trying to project, raise, determine level, etc.
(define (add x y) (drop (apply-generic 'add x y)))
(define (sub x y) (drop (apply-generic 'sub x y)))
(define (mul x y) (drop (apply-generic 'mul x y)))
(define (div x y) (drop (apply-generic 'div x y)))
(define (equ? x y) (apply-generic 'equ? x y))
;; add the generic zero predicate
(define (=zero? x) (apply-generic '=zero? x))
;; add generic exponentiation
(define (exp x y) (drop (apply-generic 'exp x y)))
;; add generic raise operation
(define (raise x) (apply-generic 'raise x))
;; add generic level
(define (level x) (apply-generic 'level x))
;; add generic projection
(define (project x) (apply-generic 'project x))

; Testing
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(project (project (project (make-complex-from-real-imag 3 4))))
(raise (project (make-complex-from-real-imag 3 4)))
(add (make-complex-from-real-imag 3 0) (make-integer 1))
(drop (make-integer 3))
(project (make-rational 2 2))
(drop (make-rational 2 2))
(drop (make-complex-from-real-imag 3 0))
(div (make-complex-from-real-imag 10 0) (make-rational 2 4))
