; Exercise 2.87 Install =zero? for polynomials in the generic arithmetic
; package. This will allow adjoin-term to work for polynomials with coefficients
; that are themselves polynomials.


; Copy over the generic arithmetic package from before

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

; Raise will be defined in each package (though it will be dependent on the other
; packages, so this may cause an issue)

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
       (lambda (x y) (tag (- x y))))
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
  (put 'project '(integer) (lambda (x) (tag x)))
  ;; add new functions for implementing complex of any type
  (put 'square '(integer) (lambda (x) (tag (* x x))))
  (put 'square-root '(integer) (lambda (x) (make-real (sqrt x))))
  (put 'sine '(integer) (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer) (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer integer) (lambda (x y) (make-real (atan x y))))
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
  ;; add raise function (have to add tags since complex now only deals with
  ;; tagged data)
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag (tag x) (tag 0.0))))
  ;; add level function
  (put 'level '(real) (lambda (x) 3))
  ;; add projection function
  (put 'project '(real) (lambda (x) (make-rational (round x) 1)))
  ;; add new functions for implementing complex of any type
  (put 'square '(real) (lambda (x) (tag (* x x))))
  (put 'square-root '(real) (lambda (x) (tag (sqrt x))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'arctan '(real real) (lambda (x y) (tag (atan x y))))
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

  ;; add new functions for implementing complex of any type
  (define (ratio x) (/ (numer x) (denom x)))
  (put 'square '(rational) (lambda (x) (tag (mul-rat x x))))
  (put 'square-root '(rational) (lambda (x) (make-real (sqrt (ratio x)))))
  (put 'sine '(rational) (lambda (x) (make-real (sin (ratio x)))))
  (put 'cosine '(rational) (lambda (x) (make-real (cos (ratio x)))))
  (put 'arctan '(rational rational) (lambda (x y) (make-real 
                                                    (atan (ratio x) 
                                                          (ratio y)))))
  'done)

; external constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
; external selector
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

; pull the entire complex number package:

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
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
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))
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
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (mul (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;;; add equality function (does not consider numerical error)
  (define (equ?-complex z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
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
  ; have to convert anything in the real-part to a real number now since it
  ; only deals with tagged data
  (put 'project '(complex) (lambda (x) (make-real (apply contents
                                         (raise-to 3 (list (real-part x)))))))

  (put 'raise '(complex) (lambda (x) (make-polynomial 'x (list 0 (tag x)))))
  'done)

; accessing the constructors from outside complex
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;; Adding new polynomial package!
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                     (adjoin-term
                       (make-term (order t1)
                                  (add (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mult-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t2) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  ; check to see that every term is zero
  (define (=zero?-termlist L)
    (cond ((empty-termlist? L)
          #t)
          ((and (=zero? (coeff (first-term L)))
               (=zero?-termlist (rest-terms L)))
          #t)
          (else #f)))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p)) 
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'term
       (lambda (order coeff) (make-term order coeff)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) (lambda (x) (=zero?-termlist (term-list x))))
  'done)


(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order terms)
  ((get 'make 'term) order terms))

; generic operators
; Removing the drops since I don't have a good way to drop/project a polynomial
; this means, terms won't be simplified, but we can live with that.
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
;; add generic level
(define (level x) (apply-generic 'level x))
;; add generic projection
(define (project x) (apply-generic 'project x))

(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))

; Testing
(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

;tests
(define poly1 (make-polynomial 'x (list (make-term 3 (make-integer 4))
                          (make-term 2 (make-integer 2))
                          (make-term 1 (make-integer 1))
                          (make-term 0 (make-integer 5)))))

(define poly2 (make-polynomial 'x (list (make-term 3 (make-integer 7))
                          (make-term 2 (make-integer 2))
                          (make-term 1 (make-integer 2))
                          (make-term 0 (make-integer 1)))))

(define poly3 (make-polynomial 'x (list (make-term 3 (make-integer 0))
                          (make-term 2 (make-integer 0))
                          (make-term 1 (make-integer 0))
                          (make-term 0 (make-integer 0)))))

(define poly4 (make-polynomial 'x (list (make-term 0 (make-complex-from-real-imag 0 0)))))
(add poly1 poly2)

(=zero? poly1)
(=zero? poly2)
(=zero? poly3)
(=zero? poly4)



