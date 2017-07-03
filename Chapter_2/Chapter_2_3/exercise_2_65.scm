; Exercise 2.65. Use the results of exercises 2.63 and 2.64 to give O(n)
; implementations of union-set and intersection-set for sets implemented as
; (balanced) binary trees.
;
; Pull the code from the past two exercises:

; O(n) for converting a tree to an ordered list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; O(1) for constructors and selectors
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; O(n) for converting an ordered list to a tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ; Find elements for left branch of the tree
      ; divide tree in half
      (let ((left-size (quotient (- n 1) 2)))
        ; get a partial tree with half the list
        (let ((left-result (partial-tree elts left-size)))
          ; store the left tree and the elements that didn't go
          ; to the left, calculate the right size
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ; Get the center element of the tree
            (let ((this-entry (car non-left-elts))
                  ; make a tree for the right side with the elements
                  ; that didn't go into the left
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                ; anything that is left after making the right tree is
                ; a remaining element
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; We also going to take code from the text for finding elements in a tree
; and adding elements to a set when they are in a tree:

; O(n) for checking membership
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; O(n) for adding element to set
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; Additionally, the intersection and union algorithms don't need to change,
; only the adjoin-set algorithm:

; O(n) for finding union of sets for an ordered list
(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set-list (cdr set1)
                                       (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set-list (cdr set1)
                                       set2)))
                  ((< x2 x1)
                   (cons x2 (union-set-list set1
                                       (cdr set2)))))))))

; O(n) for finding intersection of sets for an ordered list
(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

; O(n) intersection and unions for TREES, since each step is O(n) and they
; are linearly combined, then the entire procedure is O(n)
(define (union-set tree1 tree2)
  (list->tree (union-set-list (tree->list tree1) (tree->list tree2))))
(define (intersection-set tree1 tree2)
  (list->tree (intersection-set-list (tree->list tree1) (tree->list tree2))))

; testing:
; should be tree with 1 2 3 4 5 8 9 in it
(union-set (list->tree '(1 2 3 4)) (list->tree '(1 5 8 9)))
; should be tree with 1  in it
(intersection-set (list->tree '(1 2 3 4)) (list->tree '(1 5 8 9)))

; should be tree with 1 5 8 9 in it
(union-set (list->tree '()) (list->tree '(1 5 8 9)))
; should be tree with  '()  in it
(intersection-set (list->tree '()) (list->tree '(1 5 8 9)))
; should be tree with 1 5 8 9 in it
(union-set (list->tree '(1 5 8 9)) (list->tree '()))
; should be tree with  '()  in it
(intersection-set (list->tree '(1 5 8 9)) (list->tree '()))

