; Exercise 2.66. Implement the lookup procedure for the case where the set of
; records is structured as a binary tree, ordered by the numerical values of the
; keys.

; Here is the unordered representation for keys:
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; Since we are treating the keys as numerical values this will almost be
; identical to the element-of-set? function for binary tree representations of
; sets:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

; To test this, I need to implement a quick database entry structure
(define (make-entry key data)
  (cons key data))
(define (key entry)
  (car entry))
(define (data entry)
  (cdr entry))

; Lets also pull the list to tree function to make this a bit easier to
; create a database
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
(define database
  (list->tree (list (make-entry 1 'Test1)
                    (make-entry 2 'Test2)
                    (make-entry 3 'Test3)
                    (make-entry 4 'Test4)
                    (make-entry 5 'Test5))))
; tests
(lookup 3 database)
(lookup 5 database)
(lookup 1 database)
