; Exercise 3.26: To search a table as implemented above, one needs to scan
; through the list of records. This is basically the unordered list 
; representation of Section 2.3.3. For large tables, it may be more effecient to
; structure the table in a different manner. Describe a table implementation 
; where the (key, value) records are organized using a binary tree, assuming
; the keys can be ordered in some way (e.g., numerically or alphabetically).
; (Compare Exercise 2.66 of Chapter 2)
;
; Copying over parts of Exercise 2.66 from the before below:
; We can basically just drop in a tree in place of the list representation

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (key-rec entry)
  (car entry))
(define (value-rec entry)
  (cdr entry))

; Here is the look up procedure from exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key-rec (entry set-of-records))) (entry set-of-records))
        ((< given-key (key-rec (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key-rec (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (key-rec x) (key-rec (entry set))) set)
        ((< (key-rec x) (key-rec (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (key-rec x) (key-rec (entry set)))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

; heres the new table definition using the tree functions. Note that this tree
; doesn't do any sort of branch balancing and requires numeric keys. 
; Changing the functions to use custom = < and > functions for alphanumeric
; keys would be need to make this work for any key.
(define (make-table)
  (let ((local-table '()))

    (define (lookup-tab keys)
      (let ((record (lookup keys local-table)))
        (if record
            (value-rec record)
            #f)))

    (define (insert! keys value)
      (let ((record (lookup keys local-table)))
          (if record
              (set-cdr! record value)
              (set! local-table (adjoin-set (cons keys value) local-table)))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-tab)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define test-table (make-table))
((test-table 'insert-proc!) 3 3)
((test-table 'insert-proc!) 2 2)
((test-table 'insert-proc!) 1 1)
((test-table 'insert-proc!) 4 4)
((test-table 'lookup-proc) 4)
((test-table 'lookup-proc) 3)
((test-table 'lookup-proc) 2)
((test-table 'lookup-proc) 1)
((test-table 'insert-proc!) 4 5)
((test-table 'lookup-proc) 4)

