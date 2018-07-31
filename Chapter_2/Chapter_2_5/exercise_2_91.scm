; Exercise 2.91. A univariate polynomial can be divided by another
; one to produce a polynomial quotient and a polynomial remainder. For
; example:
;
;         x^5-1/x^2-1 = x^3 + x, remainder x-l
;
; Division can be performed via long division. That is, divide the highest-order
; term of the dividend by the highest-order term of the divisor. The result
; is the first term of the quotient. Next, multiply the result by the divisor,
; subtract from the divident, and produce the rest of the answer by 
; recursivley dividing the difference by the divisor. Stop when the order of the
; divisor exceeds the order of the dividend and declare the dividend to be the
; remainder. Also, if the dividend ever becomes zero, return zero as both 
; quotient and remainder
;
; We can design a div-poly procedure on the model of add-poly and mul-poly.
; The procedure checks to see if the two polys have the same variable. If so,
; div-poly strips off the variable and passes the problem to div-terms, which
; performs the division operation on term lists. div-poly finally reattaches
; the variable to the results supplied by div-terms. It is convenient to design
; div-terms to compute both the quotient and remainder of a division. div-terms
; can take two term lists as arguments and return a list of the quotient term
; list and the remainder list.
;
; Complete the following definition of div-terms by filling in the missing
; expressions. Use this to implement div-poly which takes two polys as arguments
; and returns a list of the quotient and remainder polys.
;(define (div-terms L1 L2) 
;  (if (empty-termlist? L1)
;    (list (the-empty-termlist) (the-empty-termlist)) 
;    (let ((t1 (first-term L1))
;          (t2 (first-term L2)))
;      (if (> (order t2) (order t1))
;          (list (the-empty-termlist) L1)
;          (let ((new-c (div (coeff t1) (coeff t2)))
;                (new-o (- (order t1) (order t2)))) 
;            (let ((rest-of-result
;                    ⟨compute rest of result recursively⟩ )) 
;              ⟨form complete result⟩ ))))))


;; COPY COMPLETE SYSTEM FROM 2.90 INCLUDING COMMENTS AND TESTS
; Once again, I am following along with the changes suggested here and just
; commenting through how everything works rather than coming up with the 
; functions completely on my own:
; http://jots-jottings.blogspot.com/2012/05/sicp-exercise-290-supporting-dense-and_31.html
; This was a very difficult problem
; In order to do this I also had to go back and change how my tower of types
; and coercion works to match the above link in order to make the system behave
; better. The end result is a clean implementation of everything from the ground
; up for the whole system.


;; CODE TO DEAL WITH TOWER HEIRARCHY AND COERCION OF TYPES
(define tower-of-types '(integer rational real complex polynomial
                         dense-terms sparse-terms))

; make a test to see if type is in tower
(define (in-tower? value)
  ; true if in tower false otherwise
  (and (pair? value) (memq (type-tag value) tower-of-types)))

; make a second test to see if a type is lower than a type
(define (is-lower? value type)
  ; here we get everything in the tower-of-types of type and higher
  (let ((type-and-higher (memq type tower-of-types)))
    ; if the value is in the tower and the type is in the tower
    (if (and type-and-higher
             (in-tower? value))
        ; return #f if value is = to type or higher #t otherwise
        (not (memq (type-tag value) type-and-higher))
        ; if type or value is not in tower raise an error
        (error "Either value's type or type is not in tower-of-types --IS-LOWER?"
               (list value type)))))

; helper function for raising types. Workhorse of raising and lowering types
(define (apply-raise x types)
  ; if not in the tower of types raise an error
  (cond ((null? types)
         (error "Type not found in the tower-of-types--APPLY-RAISE"
                (list (type-tag x) tower-of-types)))
        ; if the x is at this current junction in the tower and there is
        ; nothing to raise to, return x
        ((eq? (type-tag x) (car types))
         (if (null? (cdr types))
             x
             ; find a procedure to raise to the next level in the tower
             (let ((raiser (get-coercion (type-tag x) (cadr types))))
               ; if a coercion procedure exists use it and return the
               ; raised x
               (if raiser
                   (raiser (contents x))
                ; otherwise just return x since we can't raise
                   x))))
         ; keep iterating until you hit the type of x to begin with
         (else (apply-raise x (cdr types)))))

; function to raise
(define (raise x)
    (apply-raise x tower-of-types))

; projecting is just raising in the opposite direction
(define (project x)
  (apply-raise x (reverse tower-of-types)))

; Function to find the highest type of a list of types
(define (find-highest-type l)
  ; helper function to filter types from a list
  (define (filter-type tower-type this-list)
    ; if there is nothing left in the list return empty
    (cond ((null? this-list) '())
          ; if the first type in the list is equal to the tower type to filter
          ; remove it from the list and continue filtering
          ((eq? (car this-list) tower-type) (filter-type tower-type (cdr this-list)))
          ; otherwise add it to the remaining list and filter the rest of the
          ; list
          (else (cons (car this-list) (filter-type tower-type (cdr this-list))))))
  ; helper function to find the highest type in a list
  (define (find-highest highest remaining-tower remaining-list)
    ; if nothing left in list return the last found (highest)
    (cond ((null? remaining-list) highest)
          ; if nothing left in the tower raise error
          ((null? remaining-tower)
           (error "Cannot find highest type from non-tower types -- FIND-HIGHEST-TYPE"
                  remaining-list))
          ; otherwise take current value in the tower as the highest,
          ; pass the rest of the tower as values to check and
          ; filter out anything of the current type from the rest of the list
          (else (find-highest (car remaining-tower)
                              (cdr remaining-tower)
                              (filter-type (car remaining-tower) remaining-list)))))
  (find-highest #f tower-of-types l))

; Function to drop down to the lowest possible type without loss of information
; to be able to drop down we should be able to project and raise without a 
; loss of information otherwise we cannot drop
(define (drop x)
  ; figure out what dropping the value would be
  (let ((dropped (project x)))
    ; if the tag is the same when we drop then just return the value
    (if (eq? (type-tag x) (type-tag dropped))
        x
        ; otherwise try to raise the dropped value
        (let ((raised (raise dropped)))
          ; if this is not equal to the original value
          (if (not (equ? x raised))
              ; return the original value
              x
              ; otherwise try dropping again
              (drop dropped))))))

; Function to raise to a particular type
; straightforward implementations of raising to a particular value and
; raising all terms to a value
(define (raise-to type value)
  ; if the value is already the right type, return it
  (cond ((eq? type (type-tag value)) value)
        ; otherwise check if type is in tower of types and try raising the value
        ((memq type tower-of-types) (raise-to type (raise value)))
        (else (error "Cannot raise to non-tower type -- RAISE-TO"
                     (list type tower-of-types)))))

; Function to raise all values to a particular type
(define (raise-all-to type values)
  (if (null? values)
      '()
      (cons (raise-to type (car values)) (raise-all-to type (cdr values)))))

; Functions to allow for global coercion as defined in each numeric package
; type
; defining get and put coercion as said was needed from the text
(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))
(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

;; Tag handling code
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

;; Operation table code (taken from stack overflow since its not defined yet)
(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))


;; APPLY GENERIC FUNCTION THAT WORKS ON ALL GENERIC FUNCTIONS
; Using the code from 
; http://jots-jottings.blogspot.com/2012/02/sicp-exercise-282-multi-argument.html
; for my new apply-generic procedure
(define (apply-generic op . args)
  ; new apply-generic this finds and applys an operation if it can
  ; otherwise it trys to coerce to the highest type. Finally at the end
  ; it trys to drop the result as far as it can
  (define (find-and-apply-op)
    ; find all the types of the arguments
    (let* ((type-tags (map type-tag args))
           ; get a procedure for these types
           (proc (get op type-tags)))
      ; if that procedure exists
      (if proc
          ; apply it to the arguments
          (apply proc (map contents args))
          ; otherwise if there is more than one argument
          (if (> (length args) 1)
              ; find the highest type; raise everything to it
              ; pull out all those types 
              ; and find a procedure for that
              (let* ((highest-type (find-highest-type type-tags))
                     (mapped-args (raise-all-to highest-type args))
                     (mapped-types (map type-tag mapped-args))
                     (mapped-proc (get op mapped-types)))
                ; if that procedure exists apply it, otherwise raise an
                ; error
                (if mapped-proc
                    (apply mapped-proc (map contents mapped-args))
                    (error
                      "No method for these types -- APPLY-GENERIC"
                      (list op type-tags))))))))
  ; once you have found and applied an argument
  (let ((result (find-and-apply-op)))
    ; check that the result is still in the tower. If so drop it as low as
    ; possible. Otherwise just return the result
    (if (in-tower? result)
        (drop result)
        result)))

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

(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
; negation generic
(define (neg x) (apply-generic 'neg x))

;; CODE FOR INTEGER NUMBERS
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  ; coerce integers to rational numbers
  (define (integer->rational x) (make-rational x 1))
  (put-coercion 'integer 'rational integer->rational)
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  ; make sure we are making an integer with an integer
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "non-integer value" x))))
  ;;; added equality
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  ;; added =zero?
  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))
  ;; add exponentiation only to number package
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; add new functions for implementing complex of any type
  (put 'square '(integer) (lambda (x) (tag (* x x))))
  (put 'square-root '(integer) 
       (lambda (x) 
         (let ((root (sqrt x)))
           (make-complex-from-real-imag (make-real (real-part root))
                                        (make-real (imag-part root))))))
  (put 'sine '(integer) (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer) (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer integer) (lambda (x y) (make-real (atan x y))))
  ; add negation function
  (put 'neg '(integer) (lambda (x) (tag (* -1 x))))
  'done)

;external constructor
(define (make-integer n)
  ((get 'make 'integer) n))


;; CODE FOR RATIONAL NUMBERS
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    ; add code to check that rational is made from integers
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g)))
        (error "non-integer numerator or denominator"
               (list n d))))
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
  (define (ratio x) (/ (numer x) (denom x)))
  (define (sqrt-rat x)
    (let ((root (sqrt (ratio x))))
      (make-complex-from-real-imag (make-real (real-part root))
                                   (make-real (imag-part root)))))

  (define (rational->real x) (make-real (/ (numer x) (denom x))))
  (define (rational->integer r) (make-integer (round (/ (numer r) (denom r)))))
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
  ;; add coercion
  (put-coercion 'rational 'real rational->real)
  (put-coercion 'rational 'integer rational->integer)
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

  ;; add new functions for implementing complex of any type
  (put 'square '(rational) (lambda (x) (tag (mul-rat x x))))
  (put 'square-root '(rational) (lambda (x) (sqrt-rat x)))
  (put 'sine '(rational) (lambda (x) (make-real (sin (ratio x)))))
  (put 'cosine '(rational) (lambda (x) (make-real (cos (ratio x)))))
  (put 'arctan '(rational rational) (lambda (x y) (make-real 
                                                    (atan (ratio x) 
                                                          (ratio y)))))

  ; add negation function
  (put 'neg '(rational) (lambda (x) 
                          (tag (make-rat (* -1 (numer x))
                                    (denom x)))))
  'done)

; external constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
; external selector
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

;; CODE FOR REAL NUMBERS
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (real->complex r) (make-complex-from-real-imag (tag r) (tag 0)))
  (define (real->rational r) (make-rational (inexact->exact (numerator r))
                                            (inexact->exact (denominator r))))
  (put-coercion 'real 'complex real->complex)
  (put-coercion 'real 'rational real->rational)
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "non-real value" x))))
  ;;; added equality
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  ;; added =zero?
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  ;; add exponentiation only to number package
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; add new functions for implementing complex of any type
  (put 'square '(real) (lambda (x) (tag (* x x))))
  (put 'square-root '(real) 
       (lambda (x) 
         (let ((root (sqrt x)))
           (make-complex-from-real-imag (tag (real-part root))
                                        (tag (imag-part root))))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'arctan '(real real) (lambda (x y) (tag (atan x y))))
  ; add negation function
  (put 'neg '(real) (lambda (x) (tag (* -1 x))))
  'done)

;external constructor
(define (make-real n)
  ((get 'make 'real) n))


;; CODE FOR COMPLEX NUMBERS
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    ; error checking to make sure making complex out of non-complex numbers
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        ; drop the arguments down as low as possible to simplify when making
        ; complex number
        (cons (drop x) (drop y))
        (error "non-real real or imaginary value -- MAKE-FROM-REAL-IMAG" (list x y))))
  (define (magnitude z)
    (square-root (add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (mul r (cosine a)) 
              (mul r (sine a)))
        (error "non-real magnitude or angle -- MAKE-FROM-MAG-ANG" (list r a))))
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
  (define (make-from-mag-ang r a) 
    (if (and (is-lower? r 'complex) (is-lower? a 'complex))
        (cons (drop r) (drop a))
        (error "non-real magnitude or angle -- MAKE-FROM-MAG-ANG" (list r a))))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (if (and (is-lower? x 'complex) (is-lower? y 'complex))
        (cons (square-root (add (square x) (square y)))
              (arctan y x))
        (error "non-real real or imaginary value -- MAKE-FROM-REAL-IMAG" (list x y))))
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
(define (complex-real-part z) (apply-generic 'real-part z))
(define (complex-imag-part z) (apply-generic 'imag-part z))
(define (complex-magnitude z) (apply-generic 'magnitude z))
(define (complex-angle z) (apply-generic 'angle z))
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
    (make-from-real-imag (add (complex-real-part z1) (complex-real-part z2))
                         (add (complex-imag-part z1) (complex-imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (complex-real-part z1) (complex-real-part z2))
                         (sub (complex-imag-part z1) (complex-imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (complex-magnitude z1) (complex-magnitude z2))
                       (mul (complex-angle z1) (complex-angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (complex-magnitude z1) (complex-magnitude z2))
                       (sub (complex-angle z1) (complex-angle z2))))
  ;;; add equality function (does not consider numerical error)
  (define (equ?-complex z1 z2)
    (and (equ? (complex-real-part z1) (complex-real-part z2))
         (equ? (complex-imag-part z1) (complex-imag-part z2))))
  ;;; add =zero? function
  (define (=zero?-complex z)
    (and (=zero? (complex-real-part z))
         (=zero? (complex-imag-part z))))
  (define (complex->real z) (raise-to 'real (complex-real-part z)))
  (define (complex->polynomial z) (make-zero-order-polynomial-from-coeff (drop (tag z))))
  ;; interface to the rest of the system
  (put-coercion 'complex 'real complex->real)
  (put-coercion 'complex 'polynomial complex->polynomial)
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
  (put 'real-part '(complex) complex-real-part)
  (put 'imag-part '(complex) complex-imag-part)
  (put 'magnitude '(complex) complex-magnitude)
  (put 'angle '(complex) complex-angle)

  ; add negation function
  (put 'neg '(complex) (lambda (x) 
                          (tag (make-from-real-imag (neg (complex-real-part x))
                                               (neg (complex-imag-part x))))))
  'done)

; accessing the constructors from outside complex
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; install all previous packages on which the polynomial rely on
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Concept of Zero
(define zero (make-integer 0))

;; CODE FOR POLYNOMIAL FUNCTIONS
;;
;; TERM CODE FIRST

; this is code shared between to two term representations similar to the complex
; code for rectangular and polar coordinates
(define (install-term-package)
  ;; internal procedures
  ; simple creation of terms
  (define (make-term order coeff) (list order coeff))
  ; get the order of a term
  (define (order-term term) (car term))
  ; get the coeff of a term
  (define (coeff-term term) (cadr term))
  ; check if two terms are equal by first chekcing their order, then
  ; the value of their coefficients
  (define (equ-term? t1 t2)
    (and (= (order-term t1) (order-term t2))
         (equ? (coeff-term t1) (coeff-term t2))))
  ;; interface to rest of system
  (define (tag t) (attach-tag 'term t))
  (put 'order '(term) order-term)
  (put 'coeff '(term) coeff-term)
  (put 'equ? '(term term) equ-term?)
  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  'done)

(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

;; how to choose which rep to use? Dense or sparse?
(define (store-as-sparse? highest-order zero-terms)
  ; here we say that if its higher than order 10 and more than 10 percent
  ; of the terms are zero then use sparse. Otherwise if there are more
  ; than x/5 terms as zero then use sparse
  (if (>= highest-order 10)
      (> (/ zero-terms highest-order) 0.1)
      (> zero-terms (/ highest-order 5))))

;;; Sparse term-lists

(define (install-sparse-terms-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  ;; Make sure a term list always has at least one term
  (define (ensure-valid-term-list terms)
    ; if term list is empty than return a zero order term of zero
    (if (empty-termlist? terms)
        (list (make-term 0 zero))
        terms))
  ;; creation, how can we insert a term into a sparse term list?
  (define (insert-term term terms)
    ; if the term list is empty then we can just add it to the empty list
    (if (empty-termlist? terms)
        (adjoin-term term (the-empty-termlist))
        ; otherwise we check the first term, its order and our new terms order
        (let* ((head (first-term terms))
               (head-order (order head))
               (term-order (order term)))
          ; if the new terms order is greater than the head, then we can just
          ; add the term to the front and return it
          (cond ((> term-order head-order) (adjoin-term term terms))
                ; if it is equal to the head, then we need to add the coefficients
                ; and create a new term list with the added term at the head
                ((= term-order head-order)
                 (adjoin-term (make-term term-order (add (coeff term) (coeff head)))
                              (rest-terms terms)))
                ; otherwise we move on and check the next term
                (else (adjoin-term head (insert-term term (rest-terms terms))))))))
  ; what if we want to build term by term from scratch?
  (define (build-terms terms result)
    ; if the terms are empty just return the result
    (if (null? terms)
        result
        ; otherwise iterate through and build up the list term by term. Note
        ; that insert term allows us to have the terms in any order as it 
        ; will get inserted into the correct order as it goes
        (build-terms (cdr terms) (insert-term (car terms) result))))
  ; here we can use build terms to build a term list from any set of terms
  ; and have it in the correct order
  (define (make-from-terms terms)
    (build-terms terms (the-empty-termlist)))
  ; what if we just have a set of coefficients in order?
  (define (convert-to-term-list coeffs)
    ; if that is empty then return the empty term list
    (if (null? coeffs)
        (the-empty-termlist)
        ;otherwise add terms one by one in order and add the order
        ;by the length of what is left
        (adjoin-term (make-term (- (length coeffs) 1) (car coeffs))
                     (convert-to-term-list (cdr coeffs)))))
  ; here we can use convert-to-term-list to make any set of coefficients
  ; into a term list
  (define (make-from-coeffs coeffs)
    (convert-to-term-list coeffs))

  ;; add-poly procedures- this is the same as it was for sparse terms
  (define (add-terms L1 L2)
    ; if one is empty return the other
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            ; get the first term from each list
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              ; if the order of 1 is greater than order of 2
              (cond ((> (order t1) (order t2))
                     ; join term 1 to the rest of the added terms from
                     ; the rest of list1 and all of list 2
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ; other way around do the opposite
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      ; otherwise add the two terms of the same order together
                      ; and join that to the addition of the rest of the
                      ; terms for each list
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  ;; mul-poly procedures
  (define (mul-terms L1 L2)
    ; if l1 is empty then everything will be empty
    (if (empty-termlist? L1)
        (the-empty-termlist)
        ; otherwise add the list of t1 multiplied by all the terms in l2 with
        ; the multiplication of the rest of the terms with all the other terms
        ; in L2
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  ; helper procedure to multiply a term through an second entire term list
  (define (mul-term-by-all-terms t1 L)
    ; if the term list is empty then you can just return the empty term list
    (if (empty-termlist? L)
        (the-empty-termlist)
        ; otherwise get the first term in the list
        (let ((t2 (first-term L)))
          ; add the order of this term with the term of interest
          ; multiply the values
          ; make a new term out of this
          ; and add it to the multiplication of t1 with the rest of the
          ; terms
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))

  ; helper function for performing division of polynomials
  (define (div-terms L1 L2) 
    ; if L1 is empty then return empty lists for both answer and remainder
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist)) 
      ; get the first terms of each
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        ; if the order of t2 is greater than t1 than return L1 as a remainder
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            ; otherwise divide coefficient 1 by coefficient 2
            (let ((new-c (div (coeff t1) (coeff t2)))
                  ; subtract get division of orders
                  (new-o (- (order t1) (order t2)))) 
              ; then do long division by recursively multiplying the new 
              ; coefficient by the divisor, subtracting it from the dividend
              ; and dividing the dividend by that result
              (let ((rest-of-result
                      (div-terms
                        (add-terms L1 
                                   (negate-terms
                                     (mul-term-by-all-terms (make-term new-o new-c) 
                                                             L2)))
                        L2)))
                ; return a list of the final result and remainder
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

  ;; zero test
  ;; are all the terms zero?
  (define (=zero-all-terms? L)
    ; if its empty then yes
    (cond ((empty-termlist? L) #t)
          ; if the coeff of the first term is not zero than no
          ((not (=zero? (coeff (first-term L)))) #f)
          ; otherwise check all the other terms
          (else (=zero-all-terms? (rest-terms L)))))

  ;; negation
  ;; flip the sign of every term. If empty terms then return empty
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        ; otherwise get the first term from the list
        (let ((term (first-term L)))
          ; add it as a new term with a negated coefficient
          (adjoin-term (make-term (order term)
                                  (neg (coeff term)))
                       ; continue with the rest of the terms
                       (negate-terms (rest-terms L))))))
  ;; equality
  ;; are the two term lists the same?
  (define (equ-terms? L1 L2)
    ; if list1 is empty return true if 2 is empty false otherwise
    (cond ((empty-termlist? L1) (empty-termlist? L2))
          ; if list 2 is then empty return false
          ((empty-termlist? L2) #f)
          ; if the first term of each list is equal and the rest of the
          ; terms are equal return true
          (else (and (equ? (first-term L1) (first-term L2))
                     (equ-terms? (rest-terms L1) (rest-terms L2))))))

  ;; constant
  ;; Need to be able to coerce a polynomial down a level. Define a procedure
  ;; to take only the zero order coefficient
  (define (get-sparse-constant L)
    ; if empty term list return zero
    (cond ((empty-termlist? L) zero)
          ; otherwise if first term is 0 order, return the coefficient of the
          ; first term
          ((= (order (first-term L)) 0) (coeff (first-term L)))
          ; otherwise, work your way down the terms to the zero term
          (else (get-sparse-constant (rest-terms L)))))

  ;; coercion
  ;; we also need to figure out how many terms are zero to decide if we need
  ;; to coerce down to dense
  (define (calculate-zero-terms first rest)
    ; if this is the last term return the order of the term (since everything
    ; below it will by definition also be zero since the term list is always
    ; sorted)
    (if (empty-termlist? rest)
        (order first)
        ; otherwise get the next term
        (let ((next (first-term rest)))
          ;subtract the order of the first term from the order of the next
          ;term and 1. Then add the amount of zero order terms that come
          ;from the rest of the list
          (+ (- (order first) (order next) 1)
             (calculate-zero-terms next (rest-terms rest))))))
  ; Decide if we want to keep the list as a sparse list
  (define (keep-as-sparse? L)
    ; if empty then we want to switch to dense so return false
    (if (empty-termlist? L)
        #f
        ; otherwise get the order of the first term and the number of
        ; zero terms
        (let ((highest-order (order (first-term L)))
              (zero-terms (calculate-zero-terms (first-term L) (rest-terms L))))
          ; use the store-as-sparse predicate to determine if it should be
          ; sparse or not
          (store-as-sparse? highest-order zero-terms))))
  ; coerce sparse-terms to dense terms using the above procedures
  (define (sparse-terms->dense-terms L)
    ; if keep as sparse just tag it it. Otherwise pull the procedure from
    ; dense terms to make the term list a dense list
    (if (keep-as-sparse? L)
        (tag L)
        ((get 'make-from-terms 'dense-terms) L)))

  ;; inteface to rest of system
  (define (tag t) (attach-tag 'sparse-terms (ensure-valid-term-list t)))
  (put 'add '(sparse-terms sparse-terms)
       (lambda (t1 t2) (tag (add-terms t1 t2))))
  (put 'mul '(sparse-terms sparse-terms)
       (lambda (t1 t2) (tag (mul-terms t1 t2))))
  (put 'div '(sparse-terms sparse-terms)
       ; since this is a list of remainder and result we have to manually
       ; split it coerce it to dense-terms if needed
       (lambda (t1 t2) 
         (let ((result (div-terms t1 t2)))
           (list (sparse-terms->dense-terms (car result))
                 (sparse-terms->dense-terms (cadr result))))))
  (put 'equ? '(sparse-terms sparse-terms) equ-terms?)
  (put '=zero? '(sparse-terms) =zero-all-terms?)
  (put 'neg '(sparse-terms)
       (lambda (t) (tag (negate-terms t))))
  (put 'get-constant '(sparse-terms) get-sparse-constant)
  (put 'make-from-terms 'sparse-terms
       (lambda (terms) (tag (make-from-terms terms))))
  (put 'make-from-coeffs 'sparse-terms
       (lambda (coeffs) (tag (make-from-coeffs coeffs))))
  (put-coercion 'sparse-terms 'dense-terms sparse-terms->dense-terms)
  'done)

(define (install-dense-terms-package)
  ;; internal procedures
  ; taken from http://jots-jottings.blogspot.com/2012/04/sicp-exercise-289-representing-dense.html
  ; comments added for my clarity
  ; Thinking ahead to the next problem, wanted to make changes that will work
  ; with both sparse and dense representations. Since, for dense polynomials
  ; adjoin-term will need to know the term order to be able to determine
  ; where to add the term we will make it a part of adjoin-term in general
  (define (adjoin-term term-order term-coeff term-list)
    ; if the term is zero then we will just return the term-list as is like
    ; before
    (cond ((=zero? term-coeff) term-list)
          ; if the term-order is one greater than the current term list than
          ; we can just add the term as a new term to the list
          ((= term-order (+ 1 (term-list-order term-list)))
           (cons term-coeff term-list))
          ; however if the term order is greater than 1 past the current list
          ; than we need to add zero terms in the dense representation
          ; which we will do now until we get to the the term we are adding
          ((> term-order (term-list-order term-list))
           (adjoin-term term-order
                        term-coeff
                        (cons zero term-list)))
          ; anything else will be an error
          (else (error "Cannot adjoin term of lower order than term list -- ADJOIN-TERM"
                       (list term-order term-coeff term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ; for dense representations the order is based off the length of the term-list
  ; and therefore needs the full term-list passed for easy determination. There
  ; is no easy way around this for dense reps but for sparse the function will
  ; need to be able to take in the full term-list now
  (define (term-list-order term-list) 
    (- (length term-list) 1))

  (define (ensure-valid-term-list terms)
    (if (empty-termlist? terms)
        (list zero)
        terms))

  ;; creation.
  ;; For dense representations leading zeros are not helpful, instead we
  ;; will strip any leading zeros to get down to the smallest size polynomial
  (define (strip-leading-zeros coeffs)
    ; if empty term list return an empty termlist
    (cond ((empty-termlist? coeffs) (the-empty-termlist))
          ; otherwise if first term is not equal to zero return all the
          ; coefficents
          ((not (=zero? (first-term coeffs))) coeffs)
          ; otherwise make a new list from the coefficients without the zero
          ; coefficient
          (else (make-from-coeffs (rest-term coeffs)))))

  (define (make-from-coeffs coeffs) coeffs)

  ; We also need to determine how to insert a term in the dense
  ; representation
  (define (insert-term term terms)
    ; if the terms are empty than make a new termlist with the new term
    (if (empty-termlist? terms)
        (adjoin-term (order term) (coeff term) (the-empty-termlist))
        ; otherwise get the order of the term list and the order of the
        ; term to insert
        (let ((head-order (term-list-order terms))
              (term-order (order term)))
          ; if the term has a higher order than the list add it to the
          ; beginning of the list
          (cond ((> term-order head-order)
                 (adjoin-term term-order (coeff term) terms))
                ; if they are equal then make a new term of the same order and
                ; add the values of the coefficients
                ((= term-order head-order)
                 (adjoin-term term-order
                              (add (coeff term) (car terms))
                              (rest-terms terms)))
                ; otherwise add the first term to the new term inserted in the
                ; rest of the terms
                (else (adjoin-term head-order
                                   (car terms)
                                   (insert-term term (rest-terms terms))))))))
  ; make a procedure to build a list out of terms
  (define (build-terms terms result)
    (if (null? terms)
        result
        (build-terms (cdr terms) (insert-term (car terms) result))))

  (define (make-from-terms terms)
    (build-terms terms (the-empty-termlist)))

  ;; procedures used by add-poly
  ;; same as for sparse we just have to get the order from the list instead
  ;; of a single term
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let* ((t1 (first-term L1))
                   (t2 (first-term L2))
                   (o1 (term-list-order L1))
                   (o2 (term-list-order L2)))
              (cond ((> o1 o2)
                     (adjoin-term
                       o1 t1 (add-terms (rest-terms L1) L2)))
                    ((< o1 o2)
                     (adjoin-term
                       o2 t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        o1
                        (add t1 t2)
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  ;; procedures used by mul-poly
  ;; once again same as sparse just have to use the list for the order
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (term-list-order L1) (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms o1 c1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let* ((t2 (first-term L))
               (new-order (+ o1 (term-list-order L))))
          (adjoin-term
            new-order
            (mul c1 t2)
            (mul-term-by-all-terms o1 c1 (rest-terms L))))))

  ; helper function for performing division of polynomials
  (define (div-terms L1 L2) 
    ; if L1 is empty then return empty lists for both answer and remainder
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist)) 
      ; get the first terms of each
      (let ((t1 (first-term L1))
            (t2 (first-term L2))
            (o1 (term-list-order L1))
            (o2 (term-list-order L2)))
        ; if the order of t2 is greater than t1 than return L1 as a remainder
        (if (> o2 o1)
            (list (the-empty-termlist) L1)
            ; otherwise divide coefficient 1 by coefficient 2
            (let ((new-c (div t1 t2))
                  ; subtract get division of orders
                  (new-o (- o1 o2))) 
              ; then do long division by recursively multiplying the new 
              ; coefficient by the divisor, subtracting it from the dividend
              ; and dividing the dividend by that result
              (let ((rest-of-result
                      (div-terms
                        (add-terms L1 
                                   (negate-terms
                                     (mul-term-by-all-terms new-o new-c
                                                             L2)))
                        L2)))
                ; return a list of the final result and remainder
                (list (adjoin-term new-o new-c
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

  ;; zero test
  (define (=zero-all-terms? L)
    (cond ((empty-termlist? L) #t)
          ((not (=zero? (first-term L))) #f)
          (else (=zero-all-terms? (rest-terms L)))))

  ;; Negation
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((term (first-term L)))
          (adjoin-term (term-list-order L)
                       (neg term)
                       (negate-terms (rest-terms L))))))
  
  ;; Equality
  (define (equ-terms? L1 L2)
    (cond ((empty-termlist? L1) (empty-termlist? L2))
          ((empty-termlist? L2) #f)
          (else (and (equ? (first-term L1) (first-term L2))
                     (equ-terms? (rest-terms L1) (rest-terms L2))))))

  ;; Constant
  (define (get-dense-constant L)
    (cond ((empty-termlist? L) zero)
          ((= (term-list-order L) 0) (first-term L))
          (else (get-dense-constant (rest-terms L)))))

  ;; Coercion
  (define (dense-terms->sparse-terms L)
    ((get 'make-from-coeffs 'sparse-terms) L))

  ; counting empty terms is easier for dense terms, just need to go through
  ; the list and add up the terms that have a 0 coefficient
  (define (count-zero-terms L)
    ; if empty then 0 of them do
    (if (empty-termlist? L)
        0
        ; otherwise if first term is 0 add 1 to counting zero for the rest
        ; of the terms
        (+ (if (=zero? (first-term L)) 1 0)
           (count-zero-terms (rest-terms L)))))

  (define (to-best-representation L)
    (if (store-as-sparse? (term-list-order L) (count-zero-terms L))
        (dense-terms->sparse-terms L)
        (tag L)))

  ;; interface to rest of the system
  (define (tag t) (attach-tag 'dense-terms (ensure-valid-term-list t)))
  (put 'add '(dense-terms dense-terms)
       (lambda (t1 t2) (to-best-representation (add-terms t1 t2))))
  (put 'mul '(dense-terms dense-terms)
       (lambda (t1 t2) (to-best-representation (mul-terms t1 t2))))
  (put 'div '(dense-terms dense-terms)
       ; since this is a list of remainder and result we have to manually
       ; split it coerce it to dense-terms if needed
       (lambda (t1 t2) 
         (let ((result (div-terms t1 t2)))
           (list (to-best-representation (car result))
                 (to-best-representation (cadr result))))))
  (put 'equ? '(dense-terms dense-terms) equ-terms?)
  (put '=zero? '(dense-terms) =zero-all-terms?)
  (put 'neg '(dense-terms)
       (lambda (t) (to-best-representation (negate-terms t))))
  (put 'get-constant '(dense-terms) get-dense-constant)
  (put 'make-from-terms 'dense-terms
       (lambda (terms) (tag (make-from-terms terms))))
  (put 'make-from-coeffs 'dense-terms
       (lambda (coeffs) (tag (make-from-coeffs coeffs))))
  (put-coercion 'dense-terms 'sparse-terms dense-terms->sparse-terms)
  'done)

(define (get-constant term-list)
  (apply-generic 'get-constant term-list))

;; Adding new polynomial package!
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  ; allow making polynomials with just a term list and variable
  (define (make-poly variable term-list)
    (cons variable term-list))
  ; be able to make directly from coefficients
  (define (make-from-coeffs variable coeffs)
    (make-poly variable
               ((get 'make-from-coeffs 'dense-terms) coeffs)))
  ; be able to make directly from a list of terms
  (define (make-from-terms variable terms)
    (make-poly variable
               ((get 'make-from-terms 'sparse-terms) terms)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; variable tests and selection
  (define (variable? x) (symbol? x))
  ; allow for zero term polynomial with unbound variable
  (define (same-variable? v1 v2)
    (and (variable? v1) 
         (variable? v2) 
         (or (eq? v1 v2)
             (eq? v1 'unbound)
             (eq? v2 'unbound))))

  ; make a function to choose a variable if one polynomial is just a constant
  (define (select-variable p1 p2)
    (let ((v1 (variable p1)))
      (if (eq? v1 'unbound)
          (variable p2)
          v1)))

  ;; procedures used by add-poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (select-variable p1 p2)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  ;; procedures use by mul-poly
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (select-variable p1 p2)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; procedures use by div-poly
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((variable (select-variable p1 p2))
              (result (div (term-list p1) (term-list p2))))
          (list (make-poly variable (car result))
                (make-poly variable (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  ;;Subtraction
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ;; zero test
  (define (=zero-poly? p)
    (=zero? (term-list p)))

  ;; Negation
  (define (negate-poly p)
    (make-poly (variable p)
               (neg (term-list p))))

  ;; Equality
  (define (equ-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (equ? (term-list p1) (term-list p2))))
  
  ;; coercion
  (define (polynomial->complex p)
    (let ((constant (get-constant (term-list p))))
      (if (is-lower? constant 'complex)
          (raise-to 'complex constant)
          constant)))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p)) 
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (let ((result (div-poly p1 p2)))
           (list (drop (tag (car result)))
                 (drop (tag (cadr result)))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'neg '(polynomial) (lambda (x)
                            (tag (negate-poly x))))
  (put 'make-from-terms 'polynomial
       (lambda (variable terms) (tag (make-from-terms variable terms))))
  (put 'make-from-coeffs 'polynomial
       (lambda (variable coeffs) (tag (make-from-coeffs variable coeffs))))
  (put-coercion 'polynomial 'complex polynomial->complex)
  'done)

(define (make-polynomial-from-coeffs variable coeffs)
  ((get 'make-from-coeffs 'polynomial) variable coeffs))
(define (make-polynomial-from-terms variable terms)
  ((get 'make-from-terms 'polynomial) variable terms))
(define (make-zero-order-polynomial-from-coeff coeff)
  ((get 'make-from-coeffs 'polynomial) 'unbound (list coeff)))


; Testing
(install-term-package)
(install-sparse-terms-package)
(install-dense-terms-package)
(install-polynomial-package)

(drop (make-integer 5))
(drop (make-complex-from-real-imag (make-real 42.5) (make-real 0.5)))
(drop (make-complex-from-real-imag (make-rational 3 4) (make-integer 0)))
(drop (make-real 2.5))

(add (make-real 4.25) (make-rational 5 2))
(sub (make-complex-from-real-imag (make-real 5.0) (make-real 2.0)) 
     (make-complex-from-real-imag (make-real 2.0) (make-real 2.0)))


(is-lower? (make-integer 4) 'complex)
(is-lower? (make-rational 3 4) 'real)
(is-lower? (make-rational 3 4) 'rational)
(is-lower? (make-rational 3 4) 'integer)
(make-complex-from-real-imag (make-integer 4) (make-rational 2 4))
(drop (raise (make-complex-from-real-imag (make-integer 4) (make-rational 2 4))))

(square-root (make-integer 16))
(square (make-rational 3 5))
(cosine (make-integer 0))
(cos 0)
(sine (make-integer 3))
(/ 5084384125703515.0 36028797018963968.0)
(sin 3)
(arctan (make-integer 3) (make-integer 4))
(/ 5796142707547873.0 9007199254740992.0)
(atan 3 4)

(square-root (make-integer -1))
(make-complex-from-real-imag (make-real 3.0) (make-rational 1 2))
(add (make-complex-from-real-imag (make-integer 4) (make-rational 2 4))
       (make-integer 4))
(sub (make-complex-from-real-imag (make-real 1.5) (make-integer 3))
       (make-complex-from-real-imag (make-rational 1 2) (make-integer 3)))

;tests
(define dense
        (make-polynomial-from-coeffs 'x
                                     (list (make-integer 4)
                                           (make-integer 3)
                                           (make-integer 2)
                                           (make-integer 1)
                                           zero)))

(define dense-with-many-zeros
        (make-polynomial-from-coeffs 'x
                                     (list (make-integer 42)
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           (make-integer -1))))

(define sparse
        (make-polynomial-from-terms 'x 
                                    (list (make-term 5 (make-integer 5))
                                          (make-term 3 (make-integer 3))
                                          (make-term 1 (make-integer 1)))))

(define another-sparse
        (make-polynomial-from-terms 'x
                                    (list (make-term 5 (make-integer 5))
                                          (make-term 3 (make-integer 3))
                                          (make-term 1 (make-integer 1))
                                          (make-term 0 (make-integer 3)))))

(define very-sparse
        (make-polynomial-from-terms 'x
                                    (list (make-term 50 (make-integer 150))
                                          (make-term 10 (make-integer 11))
                                          (make-term 0 (make-integer 1)))))

(define polypoly
        (make-polynomial-from-coeffs
         'x
         (list (make-polynomial-from-coeffs 'y
                                            (list (make-integer 2)
                                                  (make-integer 1))))))
dense
dense-with-many-zeros
sparse
another-sparse
very-sparse
polypoly
(add polypoly dense)
(add polypoly polypoly)
(add (add polypoly polypoly) (make-integer 3))
(add dense dense-with-many-zeros)
(add dense-with-many-zeros dense-with-many-zeros)
(add sparse sparse)
(add sparse another-sparse)
(add very-sparse sparse)
(mul sparse dense)
(add dense sparse)
(sub sparse dense)
(neg very-sparse)
(sub (add dense (make-integer 1)) dense)


;; NEW TESTS
; test case from the book
(define p1
  (make-polynomial-from-terms 'x
                              (list (make-term 5 (make-integer 1))
                                    (make-term 0 (make-integer -1)))))
(define p2
  (make-polynomial-from-terms 'x
                              (list (make-term 2 (make-integer 1))
                                    (make-term 0 (make-integer -1)))))
p1
p2
(div p1 p2)
(div p1 p1)
; test cases from 
; http://jots-jottings.blogspot.com/2012/06/sicp-exercise-291-dividing-polynomials.html
(define sparse-numerator-1
  (make-polynomial-from-terms 'x
                              (list (make-term 5 (make-integer 1))
                                    (make-term 0 (make-integer -1)))))

(define sparse-denominator-1
  (make-polynomial-from-terms 'x
                              (list (make-term 2 (make-integer 1))
                                    (make-term 0 (make-integer -1)))))

(define sparse-numerator-2
  (make-polynomial-from-terms 'x
                              (list (make-term 2 (make-integer 2))
                                    (make-term 0 (make-integer 2)))))

(define sparse-denominator-2
  (make-polynomial-from-terms 'x
                              (list (make-term 2 (make-integer 1))
                                    (make-term 0 (make-integer 1)))))

(define sparse-numerator-3
  (make-polynomial-from-terms 'x
                              (list (make-term 4 (make-integer 3))
                                    (make-term 3 (make-integer 7))
                                    (make-term 0 (make-integer 6)))))

(define sparse-denominator-3
  (make-polynomial-from-terms 'x
                              (list (make-term 4 (make-real 0.5))
                                    (make-term 3 (make-integer 1))
                                    (make-term 0 (make-integer 3)))))

(define dense-numerator-1
  (make-polynomial-from-coeffs 'x
                               (list (make-integer 1)
                                     zero
                                     zero
                                     zero
                                     zero
                                     (make-integer -1))))

(define dense-denominator-1
  (make-polynomial-from-coeffs 'x
                               (list (make-integer 1)
                                     zero
                                     (make-integer -1))))

(define dense-numerator-2
  (make-polynomial-from-coeffs 'x
                               (list (make-integer 2)
                                     zero
                                     (make-integer 2))))

(define dense-denominator-2
  (make-polynomial-from-coeffs 'x
                               (list (make-integer 1)
                                     zero
                                     (make-integer 1))))

(define dense-numerator-3
  (make-polynomial-from-coeffs 'x
                               (list (make-integer 3)
                                     (make-integer 7)
                                     zero
                                     zero
                                     (make-integer 6))))

(define dense-denominator-3
  (make-polynomial-from-coeffs 'x
                               (list (make-real 0.5)
                                     (make-integer 1)
                                     zero
                                     zero
                                     (make-integer 3))))
(div sparse-numerator-1 sparse-denominator-1)
(div sparse-numerator-2 sparse-denominator-2)
(div sparse-numerator-3 sparse-denominator-3)
(div dense-numerator-1 dense-denominator-1)
(div dense-numerator-2 dense-denominator-2)
(div dense-numerator-3 dense-denominator-3)
(div dense-numerator-1 sparse-denominator-1)
(div sparse-numerator-1 dense-denominator-1)
