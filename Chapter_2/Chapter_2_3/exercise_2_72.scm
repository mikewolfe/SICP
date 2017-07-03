; Exercise 2.72. Consider the encoding procedure that you designed in exercise
; 2.68. What is the order of growth in the number of steps needed to encode
; a symbol? Be sure to include the number of steps needed to search the symbol
; list at each node encountered. To answer this question in general is
; difficult. Consider the special case where the relative frequencies of the
; n symbols are as described in exercise 2.71, and give the order of growth (as
; a function of n) of the number of steps needed to encode the most frequent and
; least frequent symbols in the alphabet.

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

; Its O(n^2) for the least frequent since you search both
; branches at every node. It is O(1) for the most frequent
