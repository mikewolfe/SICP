; Exercise 2.56. Show how to extend the basic differentiator to handle more
; kinds of expressions. For instance, implement the differentiation rule:
;
; d(u^n)/dx = nu^(n-1)(du/dx)
;
; by adding a new clause to the deriv program and defining appropriate
; procedures exponentiation?, base, exponent, and make-exponentiation. (You may
; use the symbol ** to denote exponentiation.) Build in the rules that anything
; raised to the power 0 is 1 and anything raised to the power 1 is the thing
; itself.

; First we need all the code associated with the differentiation system so far:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
     (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; test that the old deriv works:
; should be 1
(deriv '(+ x 3) 'x)
; should be y
(deriv '(* x y) 'x)
; should be (+ (* x y) (* y (+ x 3)))
(deriv '(* (* x y) (+ x 3)) 'x)

; Now we need to add exponentiation to the derivative procedure and the
; associated constructors and selectors. First we will modify deriv

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation (base exp)
                                  (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b1 e1)
  (cond ((=number? e1 0) 1)
        ((=number? e1 1) b1)
        ((and (number? b1) (number? e1)) (exp b1 e1))
        (else (list '** b1 e1))))

; First lets check to make sure we haven't broken anything

; test that the old deriv works:
; should be 1
(deriv '(+ x 3) 'x)
; should be y
(deriv '(* x y) 'x)
; should be (+ (* x y) (* y (+ x 3)))
(deriv '(* (* x y) (+ x 3)) 'x)

; Now lets check the exponent differentiation
; should be some variant of (* 4 x)
(deriv '(* 2 (** x 2)) 'x)
; should be the equivalent of 3x^2 + 4x 
(deriv '(+ (* 3 (** x 3)) 
           (* 2 (** x 2))) 'x)
