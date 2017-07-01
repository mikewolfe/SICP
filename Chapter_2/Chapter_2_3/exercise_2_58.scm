; Exercise 2.58. Suppose we want to modify the differentiation program so that
; it works with ordinary mathematical notation, in which + and * are infix
; rather than prefix operators. Since the differentiation program is defined in
; terms of abstract data, we can modify it to work with different
; representations of expressions solely by changing the predicates, selectors,
; and constructors that define the representation of the algebraic expressions
; on which the differentiator is to operate.

; a. Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
; task, assume that + and * always take two arguments and that expressions are
; fully parenthesized.

; First pull all the old code that doesn't change. We will only need to 
; modify the predicates, constructors and selectors
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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


; Now we can modify each of the selectors, predicates, and constructors. All
; we need to do is move the location of the symbol (+, *, **) to the second
; location instead of the first in the list
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 `+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
     (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (make-exponentiation b1 e1)
  (cond ((=number? e1 0) 1)
        ((=number? e1 1) b1)
        ((and (number? b1) (number? e1)) (exp b1 e1))
        (else (list b1 '^ e1))))

; tests
; should be 4
(deriv '(x + (3 * (x + (y + 2)))) 'x)
; should be deriv(x^3 + 3x^2 + 2x + 1) = 3x^2 + 6x + 2
(deriv '( (x ^ 3) + ((3 * (x ^ 2)) + ((2 * x) + 1))) 'x)

; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such
; that our derivative program still works?

; This simplest solution would be to allow for multiple terms for each operator:

; Prevent lists of a single term from occuring
(define (simplify x)
  (cond ((not (pair? x)) x)
        ((null? (cdr x)) (car x))
        (else x)))

; Then we just need to modify the augend exponent and multiplicand values
(define (augend s) (simplify (cddr s)))
(define (exponent e) (simplify (cddr e)))
(define (multiplicand p) (simplify (caddr p)))

; This works when operation order is not an issue: should be 4
(deriv '(x + 3 * (x + y + 2)) 'x)
; should be 3
(deriv '(x + 3 * (x + y + 2)) 'y)

; However this fails when operation order does matter:
; this should be 3 + 5 = 8
(deriv '(x * 3 + 5 * (x + y + 2)) 'x)

; It is more complicated to do the order of operations. There are many ways to
; accomplish this. But this is a way I found that makes the most sense to me.
; What we are trying to do is force the deriv procedure to split the arguments
; in such a way that the operations with higher precedence are evaluated first.
; To do that, you define the predicates in such a way that you define a term by
; its lowest precedence operator:

(define (find-lowest expr)
  (cond ((memq '+ expr) '+)
        ((memq '* expr) '*)
        ((memq '^ expr) '^)
        (error "unknown expression type -- FIND-LOWEST" expr))) 

(define (sum? x)
  (and (pair? x)
       (eq? (find-lowest x) '+)))

(define (product? x)
  (and (pair? x)
       (eq? (find-lowest x) '*)))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (find-lowest x) '^)))

; Now, we need to be able to find the addend and augend of a sum for expressions
; like this:
; (3 * 5 * 6 + 2 * 4 * 7 ^ 8) which means we need to aggregate everything before
; the '+ for the addend and everything after the '+ for the augend. (Same for
; multiplication) Luckily memq gives us everything after the '+ we just need
; something that will gives us everything before the '+. This was pulled from
; the sicp community, the comments are mine.
(define (list-before inlist x)
  (define (iter result sublist)
    ; if at the end of the list, return the result
    (cond ((null? sublist) result)
          ; if you hit the character you are looking for return the result
          ((eq? (car sublist) x) result)
          ; otherwise keep adding each element to the result until you hit the
          ; end of the list or the character you are looking for
          (else (iter (append result (list (car sublist)))
                      (cdr sublist)))))
  (iter '() inlist))

(define (addend s)
  (simplify (list-before s '+)))

(define (augend s)
  (simplify (cdr (memq '+ s))))

(define (multiplier p)
  (simplify (list-before p '*)))

(define (multiplicand p)
  (simplify (cdr (memq '* p))))

; This should now do precedence correctly. Lets run it through the same
; tests:
; should be 4
(deriv '(x + 3 * (x + y + 2)) 'x)
; should be 3
(deriv '(x + 3 * (x + y + 2)) 'y)

; this should be 3 + 5 = 8
(deriv '(x * 3 + 5 * (x + y + 2)) 'x)

