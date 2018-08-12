; Exercise 3.21: Ben Bitdiddle decides to test the queue implementation
; described above. He types in the procedures to the Lisp interpreter and 
; proceeds to try them out:

;(define q1 (make-queue))
;(insert-queue! q1 'a)
;((a) a)
;(insert-queue! q1 'b)
;((a b) b)
;(delete-queue! q1)
;((b) b)
;(delete-queue! q1)
;(() b)

; "It's all wrong!" he complains. "The interpreter's response shows that the
; last item is inserted into the queue twice. And when I delete both items, the
; second b is still there, so the queue isn't empty, even though it's supposed
; to be." Eva Lu Ator suggests that Ben has misunderstood what is happening.
; "It's not that the items are going into the queue twice," she explains.
; "It's just that the standard Lisp printer doesn't know how to make sense of
; the queue representation. If you want to see the queue printed correctly,
; you'll have to define your own print procedure for queues." Explain what Eva
; Lu is talking about. In particular, show why Ben's examples produce the
; printed results that they do. Define a procedure print-queue that takes a 
; queue as input and prints the sequence of items in the queue.

; front ptr is just the first part of the list
(define (front-ptr queue) (car queue))
; rear-ptr is the second part of the list
(define (rear-ptr queue) (cdr queue))
; to set the front pointer, we just need to change where the first pointer
; points to
(define (set-front-ptr! queue item)
  (set-car! queue item))
; to set the rear pointer, we just need to change where the second pointer
; points to
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
; To check to see if the queue is empty we only need to check if the front
; of the queue is empty
(define (empty-queue? queue)
  (null? (front-ptr queue)))
; To make a queue we will just make a list of null pointers
(define (make-queue) (cons '() '()))
; to get the front of the queue we will check it is empty and if not
; return the front pointer
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
; To insert into the queue we first make a new pair with the item and the
; empty list. We then check to see if the queue is empty, if so we set the
; front and rear pointers to the new item and return the queue. If not
; then we set the cdr of the last part of the queue to the new pair and
; then set the rear pointer to the new pair. We then return the queue
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))
; To delete something from the front of the queue we first check to see if the
; queue is empty. If not we set the front pointer to the next part of the queue
; and then return the queue
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;Answer
; Since queues are represented by a front pointer and a rear pointer, and the
; rear pointer is only needed to add things to the queue, the front pointer 
; represents everything that is actually in the queue. To delete things, you
; only need to move the front pointer, thus an empty queue can still have the
; rear pointer pointing to something which is what Ben was seeing.
; To properly print queues we only need to display the front pointer
(define (print-queue queue)
  (front-ptr queue))

(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
;((a) a)
(print-queue q1)
(insert-queue! q1 'b)
;((a b) b)
(print-queue q1)
(delete-queue! q1)
;((b) b)
(print-queue q1)
(delete-queue! q1)
;(() b)
(print-queue q1)
