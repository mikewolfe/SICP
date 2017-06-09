; Exercise 2.29. A binary mobile consists of two branches, a left branch and a
; right branch. Each branch is a rod of a certain length, from which hangs 
; either a weight or another binary mobile. We can represent a binary mobile
; using compound data by constructing it from two branches (for example, using
; list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a
; structure, which may be either a number (representing a simple weight) or 
; another mobile:

(define (make-branch length structure)
  (list length structure))

; a. Write the corresponding selectors left-branch and right-branch, which 
; return the branches of a mobile, and branch length and branch-structure which
; return the components of a branch.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

; b. Using your selectors, define a procedure total-weight that returns the
; total weight of a mobile.

; I had some help from an SICP blog here, but we have two recursively defined
; procedures. The first determines the weight of a branch by recursing down 
; until the ultimate base case of a single weight is found. The second adds
; the weights from each branch to make the total weight of the mobile
(define (branch-weight branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; Testing
(define mobile1 (make-mobile (make-branch 5 10) (make-branch 5 10)))
(define mobile2 (make-mobile (make-branch 10 20) (make-branch 30 2)))
(define mobile3 (make-mobile (make-branch 3 mobile1) (make-branch 4 mobile2)))
; should be 20
(total-weight mobile1)
; should be 22
(total-weight mobile2)
; should be 42
(total-weight mobile3)

; c. A mobile is said to be balanced if the torque applied by its top-left
; branch is equal to that applied by its top-right branch (that is, if the 
; length of the left rod multiplied by the weight hanging from that rod is equal
; to the corresponding product for the right side) and if each of the submobiles
; hanging off its branches is balanced. Design a predicate that tests whether
; a binary mobile is balanced.

; Once again, getting some help from a blog, first we define the torque of a
; single branch
(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

; Next we define whether a single branch is balanced:
(define (branch-balanced? branch)
  (if (not (pair? (branch-structure branch)))
      #t
      (balanced? (branch-structure branch))))

; We also then define whether the entire mobile is balanced. For that to be
; true, the torque from each branch must be the same and the branches themselves
; must be balanced
(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

; Testing
; Should be true
(balanced? mobile1)
; Should be false
(balanced? mobile2)
(balanced? mobile3)
; Should be true
(balanced? (make-mobile (make-branch 5 mobile1) (make-branch 5 mobile1)))

; d. Suppose we change the representation of mobiles so that the constructors
; are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new
; representation?

; The only thing that would have to change is the right-branch and
; branch-structure procedures
(define (right-branch mobile)
  (cdr mobile))
(define (branch-structure mobile)
  (cdr mobile))

; Testing
(define mobile1 (make-mobile (make-branch 5 10) (make-branch 5 10)))
(define mobile2 (make-mobile (make-branch 10 20) (make-branch 30 2)))
(define mobile3 (make-mobile (make-branch 3 mobile1) (make-branch 4 mobile2)))
; should be 20
(total-weight mobile1)
; should be 22
(total-weight mobile2)
; should be 42
(total-weight mobile3)

; Testing
; Should be true
(balanced? mobile1)
; Should be false
(balanced? mobile2)
(balanced? mobile3)
; Should be true
(balanced? (make-mobile (make-branch 5 mobile1) (make-branch 5 mobile1)))
