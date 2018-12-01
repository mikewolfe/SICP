; Exercise 3.25: Generalizing one- and two-dimensional tables, show how to 
; implement a table in which values are stored under an arbitrary number of
; keys and different values may be stored under different numbers of keys.
; The lookup and insert! procedures should take as input a list of keys used
; to access the table.


(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (assoc keys records)
      (cond ((null? records) false)
            ((same-key? keys (caar records)) (car records))
            (else (assoc keys (cdr records)))))

    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            #f)))

    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
          (if record
              (set-cdr! record value)
              (set-cdr! local-table (cons (cons keys value) 
                                          (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; This is the same as a single key table, we just use a list as the single
; key
(define test-table (make-table equal?))
((test-table 'insert-proc!) (list 'fruit 'apple) 3)
((test-table 'insert-proc!) (list 'fruit 'pear) 2)
((test-table 'insert-proc!) (list 'veggie 'lettuce) 1)
((test-table 'insert-proc!) (list 'veggie 'artichoke) 4)
((test-table 'lookup-proc) (list 'fruit 'apple))
((test-table 'lookup-proc) (list 'fruit 'pear))
((test-table 'lookup-proc) (list 'veggie 'lettuce))
((test-table 'lookup-proc) (list 'veggie 'artichoke))
((test-table 'insert-proc!) (list 'veggie 'artichoke 'kale) 10)
((test-table 'lookup-proc) (list 'veggie 'artichoke 'kale))



