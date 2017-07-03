; Exercise 2.67. Define an encoding tree and a sample message:
;(define sample-tree
;  (make-code-tree (make-leaf 'A 4)
;                  (make-code-tree
;                   (make-leaf 'B 2)
;                   (make-code-tree (make-leaf 'D 1)
;                                   (make-leaf 'C 1)))))
;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; Use the decode procedure to decode the message, and give the result.

; First pull all the code needed to represent a huffman tree:

; Leaf of a tree
; constructor
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
; predicate
(define (leaf? object)
  (eq? (car object) 'leaf))
; selectors
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; Huffman tree itself
; constructor
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
; selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
; Question code
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; Finally the decoder:
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
; helper function
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Since the tree is already given to us, we don't need the procedure to create
; the optimal huffman tree. All we have to do is decode the message

;           (0 | 1 1 0 | 0 | 1 0 | 1 0 | 1 1 1 |  0)
; should be (a |   d   | a |  b  | b   |   c   |  a)
(decode sample-message sample-tree)

