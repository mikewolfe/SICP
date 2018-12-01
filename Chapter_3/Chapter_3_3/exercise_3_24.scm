; Exercise 3.24: In the table implementations above, the keys are tested for
; equality using equal? (called by assoc). This is not always the appropriate
; test. For instance, we might have a table with numeric keys in which we don't
; need an exact match to the number we're looking up, but only a number within
; some tolerance of it. Design a table constructor make-table that takes as an
; argument a same-key? procedure that will be used to test "equality" of keys.
; make-table should return a dispatch procedure that can be used to access
; appropriate lookup and insert! procedures for a local table.


; lets take all the procedures for implementing a table from the book:

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define test-table (make-table equal?))
((test-table 'insert-proc!) 'fruit 'apple 3)
((test-table 'insert-proc!) 'fruit 'pear 2)
((test-table 'insert-proc!) 'veggie 'lettuce 1)
((test-table 'insert-proc!) 'veggie 'artichoke 4)
((test-table 'lookup-proc) 'fruit 'apple)
((test-table 'lookup-proc) 'fruit 'pear)
((test-table 'lookup-proc) 'veggie 'lettuce)
((test-table 'lookup-proc) 'veggie 'artichoke)

; should fail since these are symbols not strings
(define test-table (make-table string=?))
((test-table 'insert-proc!) 'fruit 'apple 3)
((test-table 'insert-proc!) 'fruit 'pear 2)
((test-table 'insert-proc!) 'veggie 'lettuce 1)
((test-table 'insert-proc!) 'veggie 'artichoke 4)
((test-table 'lookup-proc) 'fruit 'apple)
((test-table 'lookup-proc) 'fruit 'pear)
((test-table 'lookup-proc) 'veggie 'lettuce)
((test-table 'lookup-proc) 'veggie 'artichoke)


