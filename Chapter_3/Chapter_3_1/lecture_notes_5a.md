Why should we add an assignment statement?
Ability to break a problem into pieces that you can't otherwise

Been writing functional programs, they encode mathematical truths
All of the processes can be evolved by substitution. However, with 
assignment we have to consider the concept of time

```
                              TIME

<before>                        |
(SET! <var> <value>) ------------------------
<after>                         |<VAR> has the value <VALUE>
                                |
                                |
```

```
(DEFINE COUNT 1)
(DEFINE (DEMO X)
  (SET! COUNT (+ 1 COUNT))
  (+ X COUNT))

=>(DEMO 3)
5
=>(DEMO 3)
6
```

Same expression leads to two different answers, therefore demo is not
a function. This kills the substitution model.

Let's look at the functional version of factorial as an iterative
process

```
(DEFINE (FACT N)
  (DEFINE (ITER M I)
    (COND ((> I N) M)
          (ELSE (ITER (* I M) (+ I 1)))))
  (ITER 1 1))
```
^ THIS IS A FUNCTIONAL VERSION
Let's do a similar program using the same algorithm but using
assignments

BELOW IS AN IMPERATIVE VERSION
```
(DEFINE (FACT N)
  (LET ((I 1) (M 1))
    (DEFINE (LOOP)
      (COND ((> 1 N) M)
            (ELSE
              (SET! M (* I M))
              (SET! I (+ I 1))
              (LOOP))))
  (LOOP)))
```
Here we aren't copying procedures anymore, but changing the value of
M and I. However, now we can have timing errors. Something that didn't
happen before

What's the difference between DEFINE and SET! and LET. Define is
intended to be used only as setting something up the first time. (In
my head, define and let are acting as constructors, two defines in
a row would define two different locations in memory accessible by the
same symbol where the second define overwrites the first and the first
is eventually garbage collected) Let
however let doesn't redefine something, so it is also defining
something for the first time.

```
(LET ((VAR1 E1) (VAR2 E2))
  E3)

is the same as

(( lambda (VAR1 VAR2)
  E3)
  E1
  E2)
```
DEFINE is syntactic sugar where a bunch of variables are set up from
LET statements.


Ok so now we have to rebuild the model of computation. Substitution
model no longer works. We now need to build the environment model
which means we need to define new terms.

```
We say that a variable, V, is "bound in an expression", E. If the
meaning of E is unchanged by the uniform replacement of a variable, W,
not occuring in E, for every occurrance of V in E.
```

```
BOUND VARIABLES
for every x there exists y such that P(x, y)
x and y are bound because the expression does not depend on the
particular letters x and, can replace x with w and its the same
statement

(lambda (y) ((lambda (x) (* x y)) 3))
^ two bound variables x and y
equivalent to:
(lambda (z) ((lambda (x) (* x z)) 3))

However we can have procedures with unbound variables
FREE VARIABLES
(lambda (x) (* x y))
In this case y is not bound, it is free.
(lambda (y) ((lambda (x) (* x y)) 3))
Here * is a free variable
```

```
We say that a variable, V, is "free in an expression", E, if the
meaning of E is changed by the uniform replacement of a variable, W,
not occuring in E. For every occurrence of V in E.
```

What we are coming to is this idea of SCOPE
```
If x is a bound variable in E then there is a lambda expression where
it is bound. We call the list of formal parameters of the lambda
expression the "bound variable list" and we say that the lambda
expression "binds" the variables "declared" in its bound variable
list. In addition, those parts of the expression where a variable has
a value defined by the lambda expression which binds it is called the
"scope" of the variable.
```

Now we have enough terminology for a new model of computation. Need
a model where names refer to places rather than values.

```
                             ______________
                             I
                             x = 3
                             y = 5
                             _____________
                          ^           ^
                          | C       D |
 _____________   ----------            ---- _____________
 II                                         III
 x = 7                                      m = 1
 z = 6                                      y = 2
 _____________                              _____________
     ^                                           ^
     |                                           |
     |                                           |
     A                                           B

A, B, C, D are environments
C and D are the same environment
I, II, III are frames
z and x are bound in II
```
An environment is a function or a table or something like that, which
is made out of frames, peices of environments that are chained
together.

In frame I x and y are bound, in frame II x is bound, y is bound and
z is bound BUT the value of x is the x of 7 that SHADOWS environment
I. In environment III m y and x are bound, but y shadows the y in
frame I. These correspond to the applications of procedures.

```
 A
----> /\/\
      \/\/
      | |
      C B
A is (a pointer to) a procedure object
B is (a pointer to) an environment
C is the code of the procedure
```

This is a composite object with a peice of code and an environment
structure

New rules for our new model
```
Rule 1: A procedure object is applied to a set of arguments by
constructing a frame. binding the formal parameters of the procedure
to the actual arguments of the call, and then evaluating the body of
the procedure in the context of the new environment constructed. The
new frame has as its enclosing environment the environment part fo the
procedure being applied.

Rule 2: A lambda-expression is evaluated relative to a given
environment as follows: a new procedure object is formed, combining
the text (code) of the lambda-expression with a pointer to the
environment of evaluation.
```

Procedures combine code and environment. Environment is kind of like
a linked-list of frames.

Lets look at some programs with assignment.

```
(DEFINE MAKE-COUNTER
  (lambda (N)
    (lambda ()
      (SET! N (1+ N))
      N)))
```

Lets investigate the environment of this procedure

```
GLOBAL: +: *: /: -: car: etc.
MAKE-COUNTER                             C1
------------------------------------------------
                 |                        |
procedure object -                   _____|_______
|                                   | N = 0      |
|                                   --------------
(lambda (n) (lambda () ...))         |
                                    procedure object
                                    |
                                    (lambda () )
```

```
(DEFINE C1 (MAKE-COUNTER 0))
(DEFINE C2 (MAKE-COUNTER 10)) - makes a new frame with N =10 which
makes a new procedure etc.
```

Now lets use the procedures
```
=> (C1)
1
=> (C2)
11
=> (C1)
2
=> (C2)
12
```

Two counters each with its own independent state. What is an object?
How do you know something is an object? How do you know the two
objects aren't the same? Grab one and change it and see if the other
one changes. By introducing assignment and objects you open yourself
up to some of the biggest problems in philosophy.

```
Actions and Identity

We say that an action, A. had an effect on an object X. (or
equivalently that X was changed by A) if some property, P. which was
true of X before A became false of X after A.

We say that two objects, X and Y, are the same if any action which has
an effect on X has the same effect on Y.
```

Lets compute pi using a monte carlo method using Cesaros predicate

```
(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))
; monte carlo as defined in the textbook
```

The rest of this is in the textbook.



