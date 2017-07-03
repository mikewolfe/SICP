; Exercise 2.63. Each of the following two procedures converts a binary tree to
; a list.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
; a. Do the two procedures produce the same result for every tree? If not, how
; do the results differ? What lists do the two procedures produce for the trees
; in figure 2.16?
 
; b. Do the two procedures have the same order of growth in the number of steps
; required to convert a balanced tree with n elements to a list? If not, which
; one grows more slowly?

; First we need to pull all the procedures for making trees from the text:

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; Part a. do they give the same result?
; The first procedure starts from the top of the tree, walks all the way
; down to the leaves in order and builds a list out of the left leaves and
; the right leaves, it then appends the left leaves to the right leaves. If it
; had to call append on the entire tree every time it would be O(n^2) however
; each call to append is roughly half the size of the tree. This makes this
; procedure O(nlogn)
;
; The second procedure is iterative. It walks down the left and right side of
; the trees and builds the new list as its goes. This should be O(n). And they
; should all give the same list.

; make a test trees
(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '())
                                        (make-tree 5 '() '()))
                           (make-tree 9 '()
                                        (make-tree 11 '() '()))))

(define tree2 (make-tree 3 (make-tree 1 '() '())
                           (make-tree 7 (make-tree 5 '() '())
                                        (make-tree 9 '() 
                                                      (make-tree 11 '() '())))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '())
                           (make-tree 9 (make-tree 7 '() '())
                                        (make-tree 11 '() '()))))
; Test each function
;should all be (1 3 5 7 9 11)
(tree->list-1 tree1)
(tree->list-2 tree1)

(tree->list-1 tree2)
(tree->list-2 tree2)

(tree->list-1 tree3)
(tree->list-2 tree3)
