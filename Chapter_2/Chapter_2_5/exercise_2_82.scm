; Exercise 2.82. Show how to generalize apply-generic to handle coercion in the
; general case of multiple arguments. One strategy is to attempt to coerce all
; the arguments to the type of the first argument, then to the type of the
; second argument, and so on. Give an example of a situation where this strategy
; (and likewise the two-argument version given above) is not sufficiently
; general. (Hint: Consider the case where there are some suitable mixed-type
; operations present in the table that will not be tried.)

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

; helper functions for apply-generic with coercion

;given a list of type-tags and a type to convert to, trys to create a list of
;procedures that will convert each type tag to the type
(define (conversion-proc-list types type)
  (define (iter type-list this-type proc-list)
    (if (null? type-list)
      (reverse proc-list)
      ; search for a procedure to convert this type into the end type
      (let ((this-proc (get-coercion (car type-list) this-type)))
        ; if its found add it to the master list
        (cond (this-proc 
                (iter (cdr type-list) this-type (cons this-proc proc-list)))
              ; if the type is already correct add the identity function to
              ; the procedure list
              ((eq? (car type-list) this-type)
               (iter (cdr type-list) this-type (cons (lambda (x) x) proc-list)))
              ; otherwise return a null list to indicate failure
              (else '())))))
  (iter types type '()))

; given a set of type tags, uses conversion-proc-list to find a list of 
; procedures that will convert all the type tags to the same type
(define (find-common-type type-tags)
  (define (iter type-tags type-list)
    ; if we get to the end of the list, then we couldnt find an agreeable type
    ; for everything to get converted to and we return an empty list to 
    ; indicate failure
    (if (null? type-list)
        '()
        ; try to see if we convert everything to this type in the list
        (let ((proc-list (conversion-proc-list type-tags (car type-list))))
          (if (not (null? proc-list))
              proc-list
              ; if not try the next type in the list
              (iter type-tags (cdr type-list))))))
  (iter type-tags type-tags))

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

;testing all-eq?
(all-eq? '(1 1 1 2 1 1 1))

; apply-generic code WITH COERCION
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ; if a procedure couldnt be found and all the tags were the same
          ; then coercion won't help us
          (if (all-eq? type-tags)
            (error "No method for these types" (list op type-tags))
          ; otherwise we try to coerce all the arguments to the same type
            (let ((proc-list (find-common-type type-tags)))
              (if (not (null? proc-list))
                ; this is a little trick. Apply applies a function to a list
                ; of arguments. Since op is also one of those arguments in
                ; apply-generic, we have to add it to our list of converted
                ; arguments
                (apply apply-generic (cons op
                                           (map (lambda(x y) (x y))
                                                proc-list
                                                args)))
                (error "No method for these types" (list op type-tags)))))))))

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
  ;; added numer and denom selectors
  (put 'numer '(rational) (lambda (r) (numer r)))
  (put 'denom '(rational) (lambda (r) (denom r)))
  'done)


; external constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
; external selector
(define (numer r)
  (apply-generic 'numer r))
(define (denom r)
  (apply-generic 'denom r))

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

;testing apply-generic helper functions
(get-coercion 'scheme-number 'complex)
(conversion-proc-list '(scheme-number scheme-number scheme-number) 'complex)
(conversion-proc-list '(scheme-number 'rational 'scheme-number) 'complex)
(define x (list 1 (make-complex-from-mag-ang 1 4) 3 4 5))
(find-common-type (map type-tag x))
(map (lambda (x y) (x y))
     (find-common-type (map type-tag x))
     x)
;testing final apply-generic
(add 1 3)
(add (make-complex-from-real-imag 1 5) 1)
;(exp (make-complex-from-mag-ang 1 60) (make-complex-from-mag-ang 1 60))
;; add some new coercions
(put-coercion 'rational 'scheme-number (lambda (r) (/ (numer r) (denom r))))
(put-coercion 'scheme-number 'rational (lambda (x) (make-rational x 1)))

;; Now an example of where this coercion scheme will fail
; does fine when the coercible type that has the method is first
(exp 10 (make-rational 1 5))
; but fails when it is second
(exp (make-rational 1 5) 10)
