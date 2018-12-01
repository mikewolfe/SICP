; Exercise 3.22: Instead of representing a queue as a pair of pointers,
; we can build a queue as a procedure with local state. The local state will
; consist of pointers to the beginning and the end of an ordinary list. Thus,
; make-queue procedure will have the form
;
;(define (make-queue)
;  (let ((front-ptr ...)
;        (rear-ptr ...))
;    <definitions of internal procedures>
;    (define (dispatch m) ...)
;    dispatch))
;
; Complete the definition of make-queue and provide implementations of queue
; operations using this representation.

(define (make-queue)

  (let ((front-ptr '())
        (rear-ptr '()))
    ; to set the front pointer, we just need to change where the first pointer
    ; points to
    (define (set-front-ptr! item)
      (set! front-ptr item))

    ; to set the rear pointer, we just need to change where the second pointer
    ; points to
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    ; To check to see if the queue is empty we only need to check if the front
    ; of the queue is empty
    (define (empty-queue?)
      (null? front-ptr))

    ; to get the front of the queue we will check it is empty and if not
    ; return the front pointer
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))

    ; To insert into the queue we first make a new pair with the item and the
    ; empty list. We then check to see if the queue is empty, if so we set the
    ; front and rear pointers to the new item and return the queue. If not
    ; then we set the cdr of the last part of the queue to the new pair and
    ; then set the rear pointer to the new pair. We then return the queue
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr))))
    ; To delete something from the front of the queue we first check to see if the
    ; queue is empty. If not we set the front pointer to the next part of the queue
    ; and then return the queue
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else (set-front-ptr! (cdr front-ptr))
                  front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else
              (error "Unknown request: MAKE-QUEUE" m))))
    dispatch))

;Testing
(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
((q1 'insert-queue!) 'c)
(q1 'delete-queue!)
(q1 'delete-queue!)
(q1 'delete-queue!)

