What we did last lecture was translate the rules into the language of the 
computer. Why should we have to translate the rules?

Each rule has a lhs and a rhs and. The lhs is compared with the expression
you are trying to take the derivative of. Right hand side is the replacement for
that expression.

All rules are something like this:

```
          rule
pattern ---------> skeleton
  |                    |
  |                    |
  | match              | instantiation
  |                    |
  V                    V
  Expression -----> expression
  source             target
```

Build a language and the means of interpreting that language, where the language
allows us to directly express these rules. Instead of bringin rules to the
computer so that the rules are in the computers language, we are going to bring
the computer to the level of us so that the computer can understand the rules.

Trying to make a solution to a class of problems, rather than a particular one.
Lets look at a representation:

```scheme
(define deriv-rules
  '(
          lhs                                      rhs
    (  (dd (?c c) (? v))                           0)
    (  (dd (?v v) (? v))                           1)
    (  (dd (?v u) (? v))                           0)
    
    ( (dd (+ (? x1) (? x2)) (? v))
      (+ (dd (: x1) (: v))
         (dd (: x2) (: v)))                         )

    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2)))              )

    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n 1))))
         (dd (: x) (: v)))                          )
    ))
```
? are called pattern variables for matching in the language we are inventing.
: stand for substitution objects. Skeleton evaluations

Syntax for the rule language. Pattern match:
foo - matches exactly itself
(f a b) - matches any list whose first element is f, whose second element is
          a and whose third element is b.
(? x) - matches anything, call it x
(?c x) - matches only constants, call it x
(?v x) - matches only variables, call it x

Skeletons
foo - instantiates to itself
(f a b) - instantiates to a list of 3 elements
          results of instantiating each of f, a, and b
(: x) - instantiate to the value of x as in the matched pattern

```scheme
(define dsimp
   (simplifier deriv-rules))
(dsimp '(dd (+ x y) x))
(+ 1 0)
```

With this system, we can also define algebraic rules:
```scheme
(define algebra-rules '(
 ( ((? op) (?c e1) (?c e2))
   (: (op e1 e2))                                )

 ( ((? op) (? e1) (?c e2))
   ((: op) (: e2) (: e1))                        )

 ( (+ 0 (? e))                              (: e))
 ( (* 1 (? e))                              (: e))
 ( (* 0 (? e))                                  0)
 and so on
```

How does this all work? First think of the rules as a card deck. Each rule
has a pattern and a skeleton. Have both a matcher and instantiator.
```
                                        rules go into both match and
                                        instantiator
      dictionary
match --------> instantiator
     <--------
      expression
```
Think of it as an organic process, stick the process in the mess, and take it
out after a while. Keep doing until it doesn't change.

Now we have to tell you how it works. First look at the matcher:

```
            pattern
               |
               V
expression           dictionary
----->      matcher  ---------> result of augmenting previous dictionary
                                with whats found with matching expression
               ^                against the pattern 
               |
            dictionary (mapping of pattern variables with the values that were
                        found by matching)
```

Too complicated to look at except in pieces, with a lot of indented 
strucure. Its a case analysis.

```scheme
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed) ; propogate failures
        ((atom? pat) ; if pattern is not atomic
         *** Atomic patterns)
        *** Pattern variable clauses
        ((atom? exp) 'failed) ; but the expression is, then it fails
        ; general case here
        (else
         (match (cdr pat)  ; use the new dict to match the cdrs
                (cdr exp)
                (match (car pat) ; match the cars with a dict
                       (car exp) ; creating a new dict
                       dict)))))
```

Suppose we had the product
```scheme
(+ (* (? x) (? y))(? y))
matched against
(+ (* 3 x)) x)
```
This looks like the following trees
```
                 __
              / \   \
             /   \   ?y
            +   / | \
               * ?x ?y

                 __
              / \   \
             /   \    x
            +   / | \
               *  3  x


```
Traverse the trees simultaneously.

Now lets look at the inside of the matcher, simplest case is to look at when we
have an atomic pattern, such as foo

```scheme
((atom? pat)
  (if (atom? exp) ; if the expression is not atomic
      (if (eq? pat exp) ; if they are the same thing, then return the same dict
          dict
          'failed)
      'failed)) ; but the pattern was atomic, then you get a failure
```
Three kinds of pattern variables next:
```scheme
((arbitrary-constant? pat)
 (if (constant? exp)
     (extend-dict pat exp dict)
     'failed))
((arbitrary-variable? pat)
 (if (variable? exp)
     (extend-dict pat exp dict)
     'failed))
((arbitrary-expression? pat)
 (extend-dict pat exp dict))
```

This completes the matcher.

Now what about the instantiator
```
              skeleton
                 |
dictionary       V
----------> instantiate ----> expression
```
This is much easier:
```scheme
(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s) ; colon expression
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s)) ; recursive tree walk in the base case
                      (loop (cdr s))))))
  (loop skeleton))
```

Recursive tree walk over the skeleton and lookup things to subsitute from the
dictionary. Last bit of detail, what happens inside evaluate?

```scheme
(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
              user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict))
               (cdr form)))))
```
Cover this more later on. Right now its magic.

Now we must understand the control structure. Basic idea. GIGO (garbage in
garbage out simplifier). Build everything from simple objects.

Once again is a complicated structure, several subprograms in it.

```scheme
(define (simplifier the-rules)
  (define (simplify-exp exp)
   ***)
  (define (simplify-parts exp) ; these first two togther are the recursive
                               ; traversal of an expression
   ***)
  (define (try-rules exp) ; complicated part
   ***)
  simplify-exp)
```
The output is a simplification procedure appropriate for using that set of rules

```scheme
(define (simplify-exp exp)
  (try-rules (if (compound? exp)
                 (simplify-parts exp)
                 exp)))

(define (simplify-parts exp)
  (if (null? exp)
      '()
      (cons (simplify-exp (car exp))
            (simplify-parts (cdr exp)))))
```
What if we wrote it all in one function
```
(define (simplify-exp exp)
  (try-rules
    (if (compound? exp)
        (map simplify-exp exp)
        exp)))
```
How to try rules?
```scheme
(define (try-rules exp)
  (define (scan rules)
    ***)
  (scan the-rules))
```

```scheme
(define (scan rules)
  (if (null? rules)
      exp
      (let ((dict
             (match (pattern (car rules))
                    exp
                    (empty-dictionary))))
        (if (eq? dict 'failed)
            (scan (cdr rules))
            (simplify-exp
              (instantiate
                (skeleton (car rules))
                dict))))))
```

Final little bit to make the program work:

```
(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))
(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))
```
