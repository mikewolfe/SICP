; Exercise 2.71. Suppose we have a Huffman tree for an alphabet of n symbols,
; and that the relative frequencies of the symbols are 1, 2, 4, ..., 2 n-1
; . Sketch the tree for n=5; for n=10. In such a tree (for general n) how may
; bits are required to encode the most frequent symbol? the least frequent
; symbol?

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
(adjoin-set (make-leaf (car pair)   ; symbol
                       (cadr pair)) ; frequency
            (make-leaf-set (cdr pairs))))))

; Now I must define successive-merge
(define (successive-merge leaf-list)
    ; if the leaf-list is fully merged, then just return the full tree
    (if (null? (cdr leaf-list))
           (car leaf-list)
          ; otherwise make a code tree out of the two lowest terms and add
          ; that to the leaf-list
          (successive-merge (adjoin-set (make-code-tree (car leaf-list)
                                                       (cadr leaf-list))
                                       (cddr leaf-list)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; A n=5 tree
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))
; (((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31)

;                                 (a b c d e)
;                                   /     \ 
;                                  /       \
;                            (a b c d)      e
;                                /  \
;                               /    \
;                           (a b c)   d
;                            /   \
;                         (a b)   c
;                          / \
;                         a   b

; A n=10 tree
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)
                         (F 32) (G 64) (H 128) (I 256) (J 512)))

;                                 (a b c d e f g h i j)
;                                        /    \
;                                       /      \
;                           (a b c d e f g h i) j
;                                      |   \
;                           (a b c d e f g) h
;                                      |  \
;                            (a b c d e f) g
;                                      | \
;                                      |  f
;                                      |
;                                 (a b c d e)
;                                   /     \ 
;                                  /       \
;                            (a b c d)      e
;                                /  \
;                               /    \
;                           (a b c)   d
;                            /   \
;                         (a b)   c
;                          / \
;                         a   b

; You need one bit to encode the most frequent and n-1 bits for the least
; frequent
