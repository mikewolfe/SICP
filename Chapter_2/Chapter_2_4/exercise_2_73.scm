; Exercise 2.73. Section 2.3.2 described a program that performs symbolic
; differentiation:
;
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;           (make-product (multiplier exp)
;                         (deriv (multiplicand exp) var))
;           (make-product (deriv (multiplier exp) var)
;                         (multiplicand exp))))
;<more rules can be added here>
;(else (error "unknown expression type -- DERIV" exp))))
;
;We can regard this program as performing a dispatch on the type of the
;expression to be differentiated. In this situation the ‘‘type tag’’ of the
;datum is the algebraic operator symbol (such as +) and the operation being
;performed is deriv. We can transform this program into data-directed style by
;rewriting the basic derivative procedure as

;(define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         (else ((get ’deriv (operator exp)) (operands exp)
;var))))
;(define (operator exp) (car exp))
;(define (operands exp) (cdr exp))

;a. Explain what was done above. Why can’t we assimilate the predicates number?
;and same-variable? into the data-directed dispatch?

; We can't assimilate them into the data-directed approach because they do not
; have a tag associated with them. We could add a tag for number or variable,
; but then we would have to change a lot of our system. Its easier to just
; deal with those two conditions in the dispatcher.

;b. Write the procedures for derivatives of sums and products, and the auxiliary
;code required to install them in the table used by the program above.

; functions needed for adding tags
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

(define (sum-deriv exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (product-deriv exp var)
  (make-sum 
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                  (multiplicand exp))))
;; add these to the dispatch system
(put 'deriv '+ sum-deriv)
(put 'deriv '* product-deriv)

; helper functions needed for deriv
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))

; deriv dispatcher
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get ’deriv (operator exp)) (operands exp)
var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; Note I cannot run this code because I do not have the put and get procedures
; defined yet. I might come back to this later to test it.


;c. Choose any additional differentiation rule that you like, such as the one
;for exponents (exercise 2.56), and install it in this data-directed system.

; Here we can just directly take the code specific to exponents and add to
; the list

(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b1 e1)
  (cond ((=number? e1 0) 1)
        ((=number? e1 1) b1)
        ((and (number? b1) (number? e1)) (exp b1 e1))
        (else (list '** b1 e1))))
(define (deriv-exp exp var)
  (make-product
    (make-product
      (exponent exp)
      (make-exponentiation (base exp)
                           (make-sum (exponent exp) -1)))
    (deriv (base exp) var)))

(put 'deriv '** deriv-exp)

;d. In this simple algebraic manipulator the type of an expression is the
;algebraic operator that binds it together. Suppose, however, we indexed the
;procedures in the opposite way, so that the dispatch line in deriv looked like
;((get (operator exp) ’deriv) (operands exp) var)
;What corresponding changes to the derivative system are required?

; All we would need to do is flip the order of the put calls:
(put '+ 'deriv sum-deriv)
(put '* 'deriv product-deriv)
(put '** 'deriv deriv-exp)
