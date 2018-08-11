; Exercise 3.7: Consider the bank account objects created by make-account, with
; the password modification described in Exercise 3.3. Suppose that our banking
; system requires the ability to make joint accounts. Define a procedure
; make-join that accomplishes this. make-join should take three arguments.  The
; first is a password-protected account. The second argument must match the
; password with which the account was defined in order for the make-join
; operation to proceed. The third argument is a new password. make-join is to
; create an additional access to the original account using the new password.
; For example, if peter-acc is a bank account with password open-sesame, then
;
; (define paul-acc
;   (make-join peter-acc 'open-sesame 'rosebud))
;
; will allow one to make transactions on peter-acc using the name paul-acc and
; the password rosebud. You may wish to modify your solution to Exercise 3.3
; to accomodate this new feature.

; same procedure as 3.3 with minor updates
(define (make-account password balance)
  (define thispw password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; made a new get-balance procedure to check the current balance without
  ; depositing or withdrawing anything
  (define (get-balance)
    balance)
  ; made bad-password a no argument procedure
  (define (bad-password)
    "Incorrect password")
  (define (dispatch pw m)
    (if (eq? pw thispw)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'get-balance) (get-balance))
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        ; have bad-password be called rather than return the procedure
        (bad-password)))
  dispatch)

(define (make-joint acct pw newpw)
;first check to see if the correct password was entered. Note that this is
;hard-coded in to the current return for bad-password. IF the return value
;of bad-password procedure changes than this procedure will allow creation of
;an account that cannot be accessed
  (cond ((string? (acct pw 'get-balance)) "Incorrect password")
        (else
          ; If creation of the account is possible, then create a procedure
          ; that allows access to the original account. The issue here is that
          ; access is predicated on the original password used to set up the
          ; joint account not changing. If that password changes then the 
          ; joint account will use access. This could be either a bug or
          ; a feature. 
           (lambda (password message)
             (if (eq? password newpw)
                 (acct pw message)
                 "Incorrect password")))))

(define peter-acc (make-account 'open-sesame 100))
; make sure putting the wrong password in doesn't allow for joint account
; creation
(define bad-acc (make-joint peter-acc 'rosebud 'rosebud))
bad-acc
; Create the joint account
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; check that balances are the same in each account
(peter-acc 'open-sesame 'get-balance)
(paul-acc 'rosebud 'get-balance)
; withdraw from one account and check that the other one also changes
((peter-acc 'open-sesame 'withdraw) 10)
(paul-acc 'rosebud 'get-balance)
; withdraw from the other account and check the first
((paul-acc 'rosebud 'withdraw) 10)
(peter-acc 'open-sesame 'get-balance)
; Try depositing in both and making sure the updates are shared
((peter-acc 'open-sesame 'deposit) 20)
((paul-acc 'rosebud 'deposit) 20)
; Check that password for one account doesn't access the other account
(peter-acc 'rosebud 'get-balance)
(paul-acc 'open-sesame 'get-balance)
