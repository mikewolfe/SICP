; Exercise 2.57. Extend the differentiation program to handle sums and products of
; arbitrary numbers of (two or more) terms. Then the last example above could be
; expressed as
;
; (deriv ’(* x y (+ x 3)) ’x)
; Try to do this by changing only the representation for sums and products,
; without changing the deriv procedure at all. For example, the addend of a sum
; would be the first term, and the augend would be the sum of the rest of the
; terms.

; First copy over all the code needed for the deriv program from the last
; exercise:

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

; Now we need to modify the sum and product procedures to take multiple
; arguments. The only thing we need to change is the definitions of the 
; addened augend multiplicand and multiplier

; Right now we are returning the 3rd item in the list without checking for
; more items. Instead, we need to return 3rd item as a list of the rest
; of the items with cddr. However we also need a check on whether or not
; there are items after the 3rd item. To do that we can check if the cdr of
; cddr is null. If so there are no more items in the list and we can just
; return car of the cddr. Otherwise we want the whole cddr.

(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s)))) 

(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p)))) 

; Now lets test the new procedure
;should be 3
(deriv '(+ (* 3 x) 2 3 4 5 6 7) 'x)

; should be deriv(xy(x+3)) = x^2y + xy3 = 2xy + 3y
(deriv '(* x y (+ x 3)) 'x)

; should be deriv(x^3 + 3x^2 + 2x + 1) = 3x^2 + 6x + 2
(deriv '(+ (** x 3) (* 3 (** x 2)) (* 2 x) 1) 'x)

