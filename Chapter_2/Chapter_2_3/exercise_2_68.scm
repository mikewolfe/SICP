; Exercise 2.68. The encode procedure takes as arguments a message and a tree
; and produces the list of bits that gives the encoded message.
;(define (encode message tree)
;  (if (null? message)
;      '()
;      (append (encode-symbol (car message) tree)
;              (encode (cdr message) tree))))
; Encode-symbol is a procedure, which you must write, that returns the list of
; bits that encodes a given symbol according to a given tree. You should
; design encode-symbol so that it signals an error if the symbol is not in the
; tree at all. Test your procedure by encoding the result you obtained in
; exercise 2.67 with the sample tree and seeing whether it is the same as the
; original sample message.

; We need to pull all the code from the last exercise:

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


; Finally we need to define the procedure encode-symbol that will work together
; with encode to encode a message into bits.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
; Here is my code for encode-symbol (iterative)
(define (encode-symbol sym tree)
  (define (build-bits bits sym curr-branch)
    (cond 
          ; check to see if the current branch is a leaf
          ((leaf? curr-branch)
           (reverse bits))
          ; check if symbol is in left branch
          ((memq sym (symbols (left-branch curr-branch)))
           (build-bits (cons 0 bits) sym (left-branch curr-branch)))
          ; check if symbol is in right branch
          ((memq sym (symbols (right-branch curr-branch)))
           (build-bits (cons 1 bits) sym (right-branch curr-branch)))
          ; if none of these conditions occur than the symbol is not in the tree
          (else (error "symbol not in tree -- ENCODE-SYMBOL" sym))))
  (build-bits '() sym tree))

; Question code
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; testing;
;           (0 | 1 1 0 | 0 | 1 0 | 1 0 | 1 1 1 |  0)
; should be (a |   d   | a |  b  | b   |   c   |  a)
(encode '(a d a b b c a) sample-tree)
(decode (encode '(a d a b b c a) sample-tree) sample-tree)
(encode '(z t q) sample-tree) 
