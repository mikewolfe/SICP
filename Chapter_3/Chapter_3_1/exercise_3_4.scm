;Exercise 3.3: Modify the make-account procedure of Exercise 3.3 by adding
;another local state variable so that, if an account is accessed more than
;seven consecutive times with an incorrect password, it invokes the 
;procedure call-the-cops.

(define (make-account password balance)
  ; store the actual password
  (define thispw password)
  ; store the number of consecutive bad pw attempts
  (define badpwtimes 0)
  ; procedure to pull money out
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  ; procedure to put money in
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; procedure to return a message when a bad password is used
  (define (bad-password amount)
    (set! badpwtimes (+ badpwtimes 1))
    "Incorrect password")
  ; procedure to run when too many pw attempts have occured
  (define (call-the-cops amount)
    "Calling the cops!")
  ; dispatching for al the procedures
  (define (dispatch pw m)
    (if (eq? pw thispw)
        (begin (set! badpwtimes 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request: MAKE-ACCOUNT"
                                  m))))
        (if (>= badpwtimes 6) 
            call-the-cops
            bad-password)))
  dispatch)

(define acc (make-account 'secret-password 100))
((acc 'secret-password 'withdraw) 40)
; check to make sure only 7 bad attempts in a row calls the cops
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
