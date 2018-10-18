; Exercise 3.23: A deque ("double-ended queue") is a sequence in which items can
; be inserted and deleted at either the front or the rear. Operations on deques
; are the constructor make-deque, the predicate empty-deque?, selectors
; front-deque and rear-deque, mutators front-insert-deque!, rear-insert-deque!,
; front-delete-deque!, and rear-delete-deque!. Show how to represent deques 
; using pairs, and give implementations of the operations. All operations should
; be accomplished in O(1) steps.

; Since the answer wants it represented in pairs and not as an object with
; local state I will follow the style of exercise 3.21

; front ptr is just the first part of the list
(define (front-ptr deque) 
  (car deque))
; rear-ptr is the second part of the list
(define (rear-ptr deque) 
  (cdr deque))
; to set the front pointer, we just need to change where the first pointer
; points to
(define (set-front-ptr! deque item)
  (set-car! deque item))
; to set the rear pointer, we just need to change where the second pointer
; points to
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
; To check to see if the deque is empty we need to check if both the front and
; the back of the deque are empty
(define (empty-deque? deque)
  (and (null? (front-ptr deque)) 
       (null? (rear-ptr deque))))
; To make a deque we will just make a list of null pointers
(define (make-deque) (cons '() '()))
; to get the front of the deque we will check it is empty and if not
; return the front pointer
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (caar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  ; need to make a new pair and have a pointer to both directions of the list.
  ; The car of this pair is another pair containing the item and a pointer to
  ; the item in front of it. The cdr of the pair is the pointer to the next
  ; item
  (let ((new-pair (cons
                    (cons item '())
                    '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque)
           )
          (else
            ; set the cdr of the new pair to the front pointer of the
            ; deque
            (set-cdr! new-pair (front-ptr deque))
            ; set the backwards pointer of the old front to the new pair
            (set-cdr! (car (front-ptr deque)) new-pair)
            ; set the new front pointer to the new pair
            (set-front-ptr! deque new-pair)
            (print-deque deque)
            )
          )
    )
  )

(define (rear-insert-deque! deque item)
  ; need to make a new pair and have a pointer to both directions of the list.
  ; The car of this pair is another pair containing the item and a pointer to
  ; the item in front of it. The cdr of the pair is the pointer to the next
  ; item
  (let ((new-pair (cons
                    (cons item '())
                    '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque)
           )
          (else
            ; set the cdr of the old rear ptr to be the new pair
            (set-cdr! (rear-ptr deque) new-pair)
            ; set the reverse pointer of the new pair to be the rear-ptr
            (set-cdr! (car new-pair) (rear-ptr deque))
            (set-rear-ptr! deque new-pair)
            (print-deque deque)
            )
          )
    )
  )
; To delete something from the front of the deque we first check to see if the
; deque is empty. If not we set the front pointer to the next part of the queue
; and then return the deque
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
              ; need to remove the back-facing pointer
              (set-cdr! (car (front-ptr deque)) '())
              (print-deque deque)
              )
        )
  )

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else (set-rear-ptr! deque (cdar (rear-ptr deque)))
              ; need to remove the pointer to the end
              (set-cdr! (rear-ptr deque) '())
              (print-deque deque)
              )
              
        )
  )

; helper printer function that allows visualization of the deque as a simple
; list. Just creates a list by car-ing down the deque and reversing it to get
; it back to the correct order
(define (print-deque deque)
  (define (print-helper p q)
    (cond ((null? q)
         p)
         (else 
           (print-helper (cons (caar q) p)
                        (cdr q))
           )
         )
    )
  (reverse (print-helper '() (front-ptr deque)))
  )

;Answer
; Since everything needs to be an O(1) step, we needed to represent a deque as a
; doubly linked list. However, the print statement here is the only step that is
; actually O(n) instead of O(1). I did put the print statement in many of the 
; function calls for ease of debugging, but you could remove that statement from
; each function to get them back to O(1).
(define q1 (make-deque))
(front-insert-deque! q1 'c)
;(c)
(front-insert-deque! q1 'b)
;(b c)
(front-insert-deque! q1 'a)
;(a b c)
(rear-insert-deque! q1 'd)
;(a b c d)
(rear-insert-deque! q1 'e)
;(a b c d e)
(rear-delete-deque! q1)
;(a b c d)
(front-delete-deque! q1)
;(b c d)
(front-delete-deque! q1)
;(c d)
(front-deque q1)
; c
(rear-deque q1)
; d
