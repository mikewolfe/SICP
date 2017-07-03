; Exercise 2.70. The following eight-symbol alphabet with associated relative
; frequencies was designed to efficiently encode the lyrics of 1950s rock songs.
; (Note that the ‘‘symbols’’ of an ‘‘alphabet’’ need not be individual letters.)
; A    2    NA  16 
; BOOM 1    SHA 3 
; GET  2    YIP 9 
; JOB  2    WAH 1
;Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman
;tree, and use encode (exercise 2.68) to encode the following message:
;Get a job
;Sha na na na na na na na na 
;Get a job
;Sha na na na na na na na na
;Wah yip yip yip yip yip yip yip yip yip
;Sha boom
;How many bits are required for the encoding? What is the smallest number of
;bits that would be needed to encode this song if we used a fixed-length code
;for the eight-symbol alphabet?

; Grab a bunch of code from the past two exercises

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

; Here is the creation of the encoding tree:
(define song-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3)
                                           (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define message '(Get a job sha na na na na na na na na Get a job 
          Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip
          Sha boom))
(encode message song-tree)
(length(encode message song-tree))
; It takes 84 bits to encode this song. The smallest number of bits if we 
; encoded the song with a fixed-length code is the same as a huffman tree with
; equal weights:
(define song-tree (generate-huffman-tree '((A 1) (NA 1) (BOOM 1) (SHA 1)
                                           (GET 1) (YIP 1) (JOB 1) (WAH 1))))
(length (encode message song-tree))
; which ends up being 108 bits
