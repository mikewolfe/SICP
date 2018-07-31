; Exercise 2.93. Modify the rational-arithmetic package to use generic
; operations, but change make-rat so that it does not attempt to reduce
; fractions to lowest terms. Test your system by calling make-rational on two
; polynomials to produce a rational function:
;
; (define p1 (make-polynomial 'x '((2 1) (0 1))))
; (define p2 (make-polynomial 'x '((3 1) (0 1))))
; (define rf (make-rational p2 p1))


;; COPY COMPLETE SYSTEM FROM 2.92 INCLUDING COMMENTS AND TESTS
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
(define tower-of-types '(integer rational real complex polynomial))

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
  ; still want to make sure that rational variables are only functions
  ; or integers. Still following along from:
  ; http://jots-jottings.blogspot.com/2013/02/sicp-exercise-293-rational-functions.html
  ; First define a predicate for testing for this
  (define (valid-component? c)
    (memq (type-tag c) '(integer polynomial)))

  (define (make-rat n d)
    ; add code to check that rational is made from integers
    (cond ((integer? n) (make-rat (make-integer n) d))
          ((integer? d) (make-rat n (make-integer d)))
          ((and (valid-component? n) (valid-component? d)) (cons n d))
          (else (error
                  "numerator and denominator must both be integer or polynomial types"
                  (list n d)))))
;  (define (make-rat n d)
;    ; add code to check that rational is made from integers
;    (if (and (integer? n) (integer? d))
;        (let ((g (gcd n d)))
;            (cons (/ n g) (/ d g)))
;        (error "non-integer numerator or denominator"
;               (list n d))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ; dealing with things that used to require real numbers
  (define (all-rational-numbers? rs)
    (cond ((null? rs) true)
          ((and (eq? 'integer (type-tag (numer (car rs))))
                (eq? 'integer (type-tag (denom (car rs)))))
           (all-rational-numbers? (cdr rs)))
          (else false)))
  (define (apply-as-real-if-all-rational-numbers f rs)
    (if (all-rational-numbers? rs)
        (apply f (map raise (map tag rs)))
        (error (list "numerator and denominator must both be integer to apply " f))))

  (define (sqrt-rat x)
    (apply-as-real-if-all-rational-numbers square-root (list x)))
  (define (arctan-rat x y)
    (apply-as-real-if-all-rational-numbers arctan (list x y)))
  (define (cosine-rat x)
    (apply-as-real-if-all-rational-numbers cosine (list x)))
  (define (sine-rat x)
    (apply-as-real-if-all-rational-numbers sine (list x)))
    
  ;; have to break encapsulation to get coercion to work
  (define (to-primitive x)
    (let ((n (numer x))
          (d (denom x)))
      (if (and (eq? 'integer (type-tag n)) (eq? 'integer (type-tag d)))
          (/ (contents n) (contents d))
          0)))
  (define (rational->real r) (make-real (to-primitive r)))
  (define (rational->integer r) (make-integer (round (to-primitive r))))
  ;;; added for equ (does not consider simplification, but since gcd is called
  ;;; when the number is made, we don't have to)
  (define (equ?-rat x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  ;;; added =zero? function
  (define (=zero?-rat x)
    (=zero? (numer x)))
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
  (put 'sine '(rational) (lambda (x) (sine-rat x)))
  (put 'cosine '(rational) (lambda (x) (cosine-rat x)))
  (put 'arctan '(rational rational) (lambda (x y) (arctan-rat x y)))

  ; add negation function
  (put 'neg '(rational) (lambda (x) 
                          (tag (make-rat (sub zero (numer x))
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

;;; Sparse term-lists

(define (install-sparse-terms-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((or (empty-termlist? term-list)
             (> (order term) (order (first-term term-list))))
         (cons term term-list))
        (else (error
                "Cannot adjoin term of lower order than term list -- ADJOIN-TERM"
                (list term term-list)))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  ;; creation, how can we insert a term into a sparse term list?
  (define (insert-term term terms)
    ; if the term list is empty then we can just add it to the empty list
    (if (empty-termlist? terms)
        (adjoin-term term terms)
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

  ; coerce sparse-terms to dense terms using the above procedures
  (define (sparse-terms->dense-terms L)
    ((get 'make-from-terms 'dense-terms) L))

  ;; inteface to rest of system
  (define (tag t) (attach-tag 'sparse-terms t))
  ; only needed procedures
  (put 'adjoin-term 'sparse-terms
       (lambda (t t1) (tag (adjoin-term t t1))))
  (put 'the-empty-termlist 'sparse-terms
       (lambda () (tag (the-empty-termlist))))
  (put 'first-term '(sparse-terms)
       (lambda (t1) (first-term t1)))
  (put 'rest-terms '(sparse-terms)
       (lambda (t1) (tag (rest-terms t1))))
  (put 'empty-termlist? '(sparse-terms)
       (lambda (t1) (empty-termlist? t1)))
  (put 'make-from-terms 'sparse-terms
       (lambda (terms) (tag (make-from-terms terms))))
  (put 'make-from-coeffs 'sparse-terms
       (lambda (coeffs) (tag (make-from-coeffs coeffs))))
  (put-coercion 'sparse-terms 'dense-terms sparse-terms->dense-terms)
  'done)

(define (install-dense-terms-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (let ((term-order (order term))
          (term-coeff (coeff term)))
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
           (adjoin-term term (cons zero term-list)))
          ; anything else will be an error
          (else (error "Cannot adjoin term of lower order than term list -- ADJOIN-TERM"
                       (list term term-list))))))
  (define (the-empty-termlist) '())

  (define (first-term term-list) 
    (if (empty-termlist? term-list)
        (make-term 0 zero)
        (let ((head (car term-list)))
          (if (=zero? head)
              (first-term (cdr term-list))
              (make-term (term-list-order term-list) (car term-list))))))

  (define (rest-terms term-list) 
    (let ((tail (cdr term-list)))
      (cond ((empty-termlist? tail) tail)
            ((=zero? (car tail)) (rest-terms tail))
            (else tail))))

  (define (empty-termlist? term-list) (null? term-list))
  ; for dense representations the order is based off the length of the term-list
  ; and therefore needs the full term-list passed for easy determination. There
  ; is no easy way around this for dense reps but for sparse the function will
  ; need to be able to take in the full term-list now
  (define (term-list-order term-list) 
    (- (length term-list) 1))

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
        (adjoin-term term terms)
        ; otherwise get the order of the term list and the order of the
        ; term to insert
        (let* ((head (first-term terms))
               (head-order (order head))
               (term-order (order term)))
          ; if the term has a higher order than the list add it to the
          ; beginning of the list
          (cond ((> term-order head-order)
                 (adjoin-term term terms))
                ; if they are equal then make a new term of the same order and
                ; add the values of the coefficients
                ((= term-order head-order)
                 (adjoin-term (make-term term-order
                              (add (coeff term) (coeff head)))
                              (rest-terms terms)))
                ; otherwise add the first term to the new term inserted in the
                ; rest of the terms
                (else (adjoin-term head
                                   (insert-term term (rest-terms terms))))))))
  ; make a procedure to build a list out of terms
  (define (build-terms terms result)
    (if (null? terms)
        result
        (build-terms (cdr terms) (insert-term (car terms) result))))

  (define (make-from-terms terms)
    (build-terms terms (the-empty-termlist)))

  ;; Coercion
  (define (dense-terms->sparse-terms L)
    ((get 'make-from-coeffs 'sparse-terms) L))

  ;; interface to rest of the system
  (define (tag t) (attach-tag 'dense-terms t))

  ; only procedures needed now
  (put 'adjoin-term 'dense-terms
       (lambda (t t1) (tag (adjoin-term t t1))))
  (put 'the-empty-termlist 'dense-terms
       (tag (the-empty-termlist)))
  (put 'first-term '(dense-terms)
       (lambda (t1) (first-term t1)))
  (put 'rest-terms '(dense-terms)
       (lambda (t1) (tag (rest-terms t1))))
  (put 'empty-termlist? '(dense-terms)
       (lambda (t1) (empty-termlist? t1)))
  (put 'make-from-terms 'dense-terms
       (lambda (terms) (tag (make-from-terms terms))))
  (put 'make-from-coeffs 'dense-terms
       (lambda (coeffs) (tag (make-from-coeffs coeffs))))
  (put-coercion 'dense-terms 'sparse-terms dense-terms->sparse-terms)
  'done)

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

  ; Create a function to deal with intermediate expanded polynomial
  ; representations
  (define (select-variable-from-polys p1 p2)
    (select-variable (variable p1) (variable p2)))

  ; make a function to choose a variable if one polynomial is just a constant
  (define (select-variable v1 v2)
    (if (eq? v1 'unbound)
        v2
        v1))

  ;; Term-list manipulation
  (define (first-term L)
    (apply-generic 'first-term L))

  (define (rest-terms L)
    (apply-generic 'rest-terms L))

  (define (empty-termlist? L)
    (apply-generic 'empty-termlist? L))

  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))

  (define (the-empty-termlist)
    ((get 'the-empty-termlist 'sparse-terms)))

  ;; procedures used by add-poly
  (define (add-poly p1 p2)
    (make-poly (select-variable-from-polys p1 p2)
                (add-terms (term-list p1)
                           (term-list p2))))

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


  ;; procedures use by mul-poly
  (define (mul-poly p1 p2)
    (make-poly (select-variable-from-polys p1 p2)
                (mul-terms (term-list p1)
                           (term-list p2))))

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

  ;; procedures use by div-poly
  (define (div-poly p1 p2)
    (let ((variable (select-variable-from-polys p1 p2))
          (result (div-terms (term-list p1) (term-list p2))))
      (list (make-poly variable (car result))
            (make-poly variable (cadr result)))))

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

  ;;Subtraction
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ;; zero test
  (define (=zero-poly? p)
    (=zero-all-terms? (term-list p)))

  ;; are all the terms zero?
  (define (=zero-all-terms? L)
    ; if its empty then yes
    (cond ((empty-termlist? L) #t)
          ; if the coeff of the first term is not zero than no
          ((not (=zero? (coeff (first-term L)))) #f)
          ; otherwise check all the other terms
          (else (=zero-all-terms? (rest-terms L)))))

  ;; Negation
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))
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
  ;; Equality
  (define (equ-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (equ-terms? (term-list p1) (term-list p2))))

  ;; are the two term lists the same?
  (define (equ-terms? L1 L2)
    ; if list1 is empty return true if 2 is empty false otherwise
    (cond ((=zero-all-terms? L1) (=zero-all-terms? L2))
          ; if list 2 is then empty return false
          ((empty-termlist? L2) #f)
          ; if the first term of each list is equal and the rest of the
          ; terms are equal return true
          (else (and (equ? (first-term L1) (first-term L2))
                     (equ-terms? (rest-terms L1) (rest-terms L2))))))

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

  ;; how to choose which rep to use? Dense or sparse?
  (define (select-representation highest-order zero-terms)
    ; here we say that if its higher than order 10 and more than 10 percent
    ; of the terms are zero then use sparse. Otherwise if there are more
    ; than x/5 terms as zero then use sparse
    (if (or (and (>= highest-order 10) (> (/ zero-terms highest-order) 0.1))
            (and (< highest-order 10) (> zero-terms (/ highest-order 5))))
        'sparse-terms
        'dense-terms))

  (define (to-best-representation L)
    (if (empty-termlist? L)
        L
        (let* ((first (first-term L))
               (current (type-tag L))
               (desired (select-representation (order first)
                                              (calculate-zero-terms first
                                                                    (rest-terms L)))))
          (if (eq? desired current)
              L
              (let ((raiser (get-coercion current desired)))
                (if raiser
                    (raiser (contents L))
                    (error "Missing coercion -- TO-BEST-REPRESENTATION"
                           (list current desired))))))))

  ;; constant
  ;; Need to be able to coerce a polynomial down a level. Define a procedure
  ;; to take only the zero order coefficient
  (define (get-constant L)
    ; if empty term list return zero
    (cond ((empty-termlist? L) zero)
          ; otherwise if first term is 0 order, return the coefficient of the
          ; first term
          ((= (order (first-term L)) 0) (coeff (first-term L)))
          ; otherwise, work your way down the terms to the zero term
          (else (get-constant (rest-terms L)))))
 
  ; New function to impose ordering and select a principal variable
  (define (select-principal-variable v1 v2)
    ; if either variable is unbound choose the other
    ; otherwise convert each symbol to a string and
    ; choose the highest string alphabetically
    (cond ((eq? v1 'unbound) v2)
          ((eq? v2 'unbound) v1)
          (else (let ((s1 (symbol->string v1))
                      (s2 (symbol->string v2)))
                  (if (string<=? s1 s2)
                      v1
                      v2)))))

  ; New function to express a polynomial in a different variable
  (define (express-in principal-variable p)
    (cond ((eq? principal-variable (variable p)) p)
          ((eq? 'unbound (variable p)) (make-poly principal-variable (term-list p)))
          (else (make-from-coeffs principal-variable (list (tag p))))))

  ; EXPANSION of polynomials- code taken from here 
  ; http://jots-jottings.blogspot.com/2012/09/sicp-exercise-292-dealing-with.html
  ; and comments added. This is based on the idea of representing expanded
  ; polynomials as a list of lists. Where each list contains a list of the
  ; variables and their order as the car and the coefficient as the cdr
  (define (expand-poly p)
    (expand-terms (variable p) (term-list p)))
    
  ; procedure to expand everything recursively
  (define (expand-terms var t1)
    ; since we are storing everything in a list we need to return the empty
    ; list if the term lists are empty
    (if (empty-termlist? t1)
        '()
        ; otherwise we append the expanded first term to the expanded
        ; rest of the terms
        (append (expand-term var (first-term t1))
                (expand-terms var (rest-terms t1)))))

  ; expand a single term based on the principle variable and actual term
  (define (expand-term var term)
    ; pull out the coefficient and order. Expand out the term if it is a
    ; polynomial, otherwise make a list of empty variables in the front
    ; and coefficient in the end
    (let* ((termcoeff (coeff term))
           (termorder (order term))
           (expanded (if (eq? (type-tag termcoeff) 'polynomial)
                         (expand-poly (contents termcoeff))
                         (list (cons '() termcoeff)))))
      ; if the term order is zero return the expanded term. Otherwise
      ; we need to expand based on the indeterminate
      (if (= termorder 0)
          expanded
          (expand-by-indeterminate var termorder expanded))))

  (define (expand-by-indeterminate var order expansion)
    ; procedure takes in the indeterminate variable, the order of that
    ; variable and the expanded term
    ; If this expansion is the empty list than return the empty list
    (if (null? expansion)
        '()
        ; get the beginning of the expansion which is the list of variables
        (let ((head (car expansion)))
          (cons (cons (accumulate-indeterminate var order (car head)) (cdr head))
                (expand-by-indeterminate var order (cdr expansion))))))

  ; procedure to add together the indeterminates of the same variable
  (define (accumulate-indeterminate var termorder il)
    ; il is the indeterminate list. If its empty than return a list of
    ; the variable and termorder only
    (if (null? il)
        (cons (cons var termorder) '())
        (let* ((head (car il))
               (head-var (car head)))
          ; if variables are the same then accumulate them together by adding
          ; the order of the terms
          (cond ((same-variable? var head-var)
                 (cons (cons (select-variable var head-var)
                             (+ termorder (cdr head)))
                       (cdr il)))
          ; if you encounter a variable that is higher in precedence put it
          ; add the head of the list and return
                ((string<=? (symbol->string var) (symbol->string head-var))
                 (cons (cons var termorder) il))
          ; otherwise keep accumulating terms recursively
          (else (cons head
                      (accumulate-indeterminate var termorder (cdr il))))))))

  ;; REARRANGING EXPANSIONS
  ; take in an expansion and rearrange to be in order
  (define (rearrange-expansion expansion)
    ; if its empty return the empty list
    (if (null? expansion)
        '()
        ; otherwise add the top of the expansion to the rearranged rest
        ; of the expansion in the right place
        (add-to-expansion-in-order (car expansion)
                                   (rearrange-expansion (cdr expansion)))))
    
  ; find the right place to add a part of the expansion to the rest of the
  ; expansion
  (define (add-to-expansion-in-order component expansion)
    ; if its empty just add the component to the front of the expansion
    (if (null? expansion)
        (cons component expansion)
        ; get the variable from the component and compare it to the first
        ; variable of the expansion
        (let* ((var (car component))
               (compare (compare-expanded-vars var (caar expansion))))
          ; if the variable is to the left of the first variable in the expansion
          ; add it appropriately
          (cond ((< compare 0)
                 (cons component expansion))
                ; if it is to the right of the expansion walk it down the expansion
                ; and put it in the right spot recusively
                ((> compare 0)
                 (cons (car expansion)
                       (add-to-expansion-in-order component (cdr expansion))))
                ; if it is equal then add the two components together, if they
                ; become zero then drop it and take the next part of the 
                ; expansion. Otherwise add this combined part of the expansion
                ; to the front of the list
                (else
                  (let ((combined (add (cdr component) (cdar expansion))))
                    (if (=zero? combined)
                        (cdr expansion)
                        (cons (cons var combined) (cdr expansion)))))))))
  ; compare to variables to see if they are the same
  (define (compare-expanded-vars vars1 vars2)
    ; first check for nulls and handle appropriately
    (cond ((null? vars1) (if (null? vars2) 0 1))
          ((null? vars2) -1)
          ; next see if the first variable is the same; if so, check the difference
          ; in their orders, if its zero. keep comparing, otherwise return the
          ; difference in the orders
          ((same-variable? (caar vars1) (caar vars2))
           (let ((order-diff (- (cdar vars2) (cdar vars1))))
             (if (= order-diff 0)
                 (compare-expanded-vars (cdr vars1) (cdr vars2))
                 order-diff)))
          ; if not the same variable, then select a principal variable. If the
          ; first var is the principal return -1, otherwise return 1 for
          ; the second variable
          (else (let* ((var1 (caar vars1))
                       (var2 (caar vars2))
                       (principal (select-principal-variable var1 var2)))
                  (if (same-variable? principal var1)
                      -1
                      1)))))

  ;; COLLAPSE EXPANSIONS
  (define (collapse-expansion expanded)
    ; if the expanded term list is empty then make a zero unbound poly
    (cond ((null? expanded) (make-from-coeffs 'unbound (list zero)))
          ; if the expanded term list doesn't have a bound variable than
          ; make an unbound poly with the coefficient from the zero term
          ((null? (caar expanded)) (make-from-coeffs 'unbound (list (cdar expanded))))
          ; otherwise get the first term in the list, the principal variable
          ; the starting order of that principal variable and collapse the
          ; whole list with that information. Make a polynomial out of that
          (else (let* ((first (car expanded))
                       (principal (caaar first))
                       (start-order (cdaar first))
                       (collapsed-tl (to-collapsed-term-list expanded
                                                             principal
                                                             start-order
                                                             '())))
                  (make-poly principal collapsed-tl)))))

  (define (to-collapsed-term-list expanded principal current-order current-group)
    ; ok this function is a little crazy, lots of cases to deal with. First
    ; are the base cases. If the expansion is empty as well as the current
    ; group then we return an empty term list
    (cond ((and (null? expanded) (null? current-group)) (the-empty-termlist))
          ; however if just the expansion is empty but there are terms in the
          ; current group than we make a term with the current-order and
          ; collapse the current group. We add this term to the empty
          ; term list
          ((null? expanded)
           (adjoin-term (make-term current-order
                                   (collapse-sub-expansion current-group))
                        (the-empty-termlist)))
          ; Ok base cases done. Now if the current order is not 0 and
          ; the variables of the first term in the expansion are null or
          ; not equal to the principal variable then we are at the zero order
          ; term for the principal variable. We can go ahead and make a term
          ; with the current order and the collapsed group and add it to a
          ; collapsed empty expansion of zero order and collapse the rest of
          ; the expansion as well
          ((and (not (= current-order 0))
                (or (null? (caar expanded))
                    (not (eq? principal (caaaar expanded)))))
           (adjoin-term (make-term current-order
                                   (collapse-sub-expansion current-group))
                        (to-collapsed-term-list '() principal 0 expanded)))
          ; However what if it is a zero order term or its not equal to the
          ; principal variable. Then we combine the current-group and the
          ; rest of the expansion and collapse it with the current-order
          ((or (null? (caar expanded))
               (not (eq? principal (caaaar expanded))))
           (to-collapsed-term-list '()
                                   principal
                                   current-order
                                   (append current-group expanded)))
          ; What if the current order is not zero and the next term has 
          ; a different order? We then create a new term with the current order
          ; collapse everything in the current group but then we start a new
          ; group where we recursively collapse the rest of expansion but
          ; update the current order to the highest in the expansion
          ((and (not (= current-order 0))
                (not (= current-order (cdaaar expanded))))
           (adjoin-term (make-term current-order
                                   (collapse-sub-expansion current-group))
                        (to-collapsed-term-list expanded
                                                principal
                                                (cdaaar expanded)
                                                '())))
          ; The last case is to add the expanded term to the group and keep
          ; processing
          (else
            (to-collapsed-term-list (cdr expanded)
                                    principal
                                    current-order
                                    (append current-group
                                            (list (cons (cdaar expanded)
                                                        (cdar expanded))))))))
  ; procedure similar to collapse expansion but for making terms rather
  ; than internal things. So we replace some the conditions with return
  ; values that make more sense
  (define (collapse-sub-expansion expanded)
    ; return zero here instead of an unbound polynomial
    (cond ((null? expanded) zero)
          ; return the coefficient here instead of an unbound polynomial
          ((null? (caar expanded)) (cdar expanded))
          (else (let* ((first (car expanded))
                       (principal (caaar first))
                       (start-order (cdaar first))
                       (collapsed-tl (to-collapsed-term-list expanded
                                                             principal
                                                             start-order
                                                             '())))
                  ;properly tag our polynomial to go into a term list
                  (tag (make-poly principal collapsed-tl))))))
  ; procedure to put all three actions together: expansion, rearrangement
  ; and collapse
  (define (to-canonical-poly p)
    (collapse-expansion (rearrange-expansion (expand-poly p))))


  ; new procedure to coerce polynomials to the principle variable and
  ; then call the operation on them
  (define (coerce-and-call p1 p2 op)
    (let* ((canonical-p1 (to-canonical-poly p1))
           (canonical-p2 (to-canonical-poly p2))
           (principal (select-principal-variable (variable canonical-p1) 
                                                 (variable canonical-p2)))
           (new-p1 (express-in principal canonical-p1))
           (new-p2 (express-in principal canonical-p2)))
      (op new-p1 new-p2)))
 
  ;; coercion
  (define (polynomial->complex p)
    (let ((constant (get-constant (term-list p))))
      (if (is-lower? constant 'complex)
          (raise-to 'complex constant)
          constant)))

  ;; interface to rest of the system
  (define (tag p) 
    (attach-tag 'polynomial 
                (make-poly (variable p)
                           (to-best-representation (term-list p)))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 add-poly))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 sub-poly))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (coerce-and-call p1 p2 mul-poly))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (let ((result (coerce-and-call p1 p2 div-poly)))
           (list (drop (tag (car result)))
                 (drop (tag (cadr result)))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'neg '(polynomial) (lambda (x)
                            (tag (negate-poly x))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
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

; new tests for polynomials of different variables
(define poly-1
    (make-polynomial-from-coeffs
     'x
     (list (make-polynomial-from-coeffs
            'y
            (list (make-integer 5) (make-integer 2) (make-integer -1)))
           (make-polynomial-from-coeffs
            'y
            (list (make-integer 2) (make-integer 1) (make-integer 2)))
           (make-integer -3))))
(define poly-2
    (make-polynomial-from-coeffs
     'y
     (list (make-polynomial-from-coeffs
            'x
            (list (make-integer 5) (make-integer 2) zero))
           (make-polynomial-from-coeffs
            'x
            (list (make-integer 2) (make-integer 1) zero))
           (make-polynomial-from-coeffs
            'x
            (list (make-integer -1) (make-integer 2) (make-integer -5))))))
(sub poly-1 poly-2)

; new tests for reducing polynomials properly

(define p1
  (make-polynomial-from-coeffs
   'x
   (list (make-integer 5)
         (make-polynomial-from-coeffs
          'x
          (list (make-integer 10)
                (make-integer 6)
                (make-integer 4)))
         (make-integer 3))))

(define p2
  (make-polynomial-from-coeffs
   'x
   (list (make-polynomial-from-coeffs
          'y
          (list (make-integer 10)
                (make-polynomial-from-coeffs
                 'x
                 (list (make-integer 10)
                       (make-integer 6)
                       (make-integer 4)))
                (make-integer 4)))
         (make-polynomial-from-coeffs
          'x
          (list (make-integer 10)
                (make-integer 6)
                (make-integer 4)))
         (make-integer 3))))

(define p3
  (make-polynomial-from-coeffs
   'y
   (list (make-polynomial-from-coeffs
          'x
          (list (make-integer 1)
                (make-integer 5)
                (make-integer -3)))
         (make-polynomial-from-coeffs
          'x
          (list (make-integer 2)
                (make-integer 3)
                (make-integer 1)))
         (make-integer -5))))

(define p4
  (make-polynomial-from-coeffs
   'x
   (list (make-polynomial-from-coeffs
          'y
          (list (make-integer 5)
                (make-integer 2)
                (make-integer -1)))
         (make-polynomial-from-coeffs
          'y
          (list (make-integer 2)
                (make-integer 1)
                (make-integer 2)))
         (make-integer -3))))

(define p5
  (make-polynomial-from-coeffs
   'y
   (list (make-polynomial-from-coeffs
          'x
          (list (make-integer 5)
                (make-integer 2)
                zero))
         (make-polynomial-from-coeffs
          'x
          (list (make-integer 2)
                (make-integer 1)
                zero))
         (make-polynomial-from-coeffs
          'x
          (list (make-integer -1)
                (make-integer 2)
                (make-integer -5))))))

(define p6 (make-polynomial-from-coeffs 'x (list (make-integer 42))))
p1
p2
p3
p4
p5
p6

(add p1 p2)
(add p4 p5)
(sub p4 p5)
(add p6 p3)
(add p6 p6)

; new tests for 2.93
(define p1 (make-polynomial-from-coeffs 'x (list (make-integer 1)
                                                 zero
                                                 (make-integer 1))))
p1
(define p2 (make-polynomial-from-coeffs 'x (list (make-integer 1)
                                                 zero
                                                 zero
                                                 (make-integer 1))))
p2
(define rf (make-rational p2 p1))
rf
(add rf rf)
