; Exercise 2.64. The following procedure list->tree converts an ordered list to
; a balanced binary tree. The helper procedure partial-tree takes as arguments
; an integer n and list of at least n elements and constructs a balanced tree
; containing the first n elements of the list. The result returned by
; partial-tree is a pair (formed with cons) whose car is the constructed tree
; and whose cdr is the list of elements not included in the tree.
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
; a. Write a short paragraph explaining as clearly as you can how partial-tree
; works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; Need constructors and selectors first
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
; This procedure works by splitting the list into a left and right halves and
; recursively building a tree with the left half and right half, finally 
; assembling the final binary tree.
;
; The tree for the list (1 3 5 7 9 11) would be
; 
;                    5        
;                  /   \      
;                 1      9    
;                  \    /  \  
;                   3  7    11
; Test
(list->tree '(1 3 5 7 9 11))

; b. What is the order of growth in the number of steps required by list->tree
; to convert a list of n elements? 
;
; Every step you are working with half of the list, this should be an
; O(logn) operation, however, you are also calling partial-tree twice for every
; half list. So this ends up being O(n)

