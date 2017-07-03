; Exercise 2.69. The following procedure takes as its argument a list of
; symbol-frequency pairs (where no symbol appears in more than one pair) and
; generates a Huffman encoding tree according to the Huffman algorithm.
;(define (generate-huffman-tree pairs)
;  (successive-merge (make-leaf-set pairs)))
;Make-leaf-set is the procedure given above that transforms the list of pairs
;into an ordered set of leaves. Successive-merge is the procedure you must
;write, using make-code-tree to successively merge the smallest-weight elements
;of the set until there is only one element left, which is the desired Huffman
;tree. (This procedure is slightly tricky, but not really complicated. If you
;find yourself designing a complex procedure, then you are almost certainly
;doing something wrong. You can take significant advantage of the fact that we
;are using an ordered set representation.)

; Lets pull all the procedures from the text:

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

; Testing
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
;should be the same
sample-tree
