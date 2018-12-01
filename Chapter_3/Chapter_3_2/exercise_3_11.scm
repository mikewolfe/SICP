; Exercise 3.11: In Section 3.2.3 we saw how environment model described the
; behavior of procedures with local state. Now e have seen how internal
; definitions work. A typical message-passing procedure contains both of these
; aspects. Consider the bank account procedure of Section 3.1.1:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
            (error "Unknown request: MAKE_ACCOUNT"
                   m))))
  disptach)

; Show the environment structure generated by tbe sequence of interactions
(define acc (make-account 50))
((acc 'deposit) 40)
90
((acc 'withdraw) 60)
30

; Where is the local state for acc kept? Suppose we define another account

(define acc2 (make-account 100))

; How are the local states for the two accounts kept distinct?
; Which parts of the environment structure are shared between acc and acc2?


;New Version
;              +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
; global ----> |       acc:---       ^                                       |            
;              ++++++++++++++|+++++++|++++++++++++++++++++++++++++++++++++++++            
;                            |       |                                                    
;                            |       |                                                    
;                            |       +++++++++++++++                                      
;                            |    E1-| balance: 50 |                                      
;                            |       | withdraw:   |----------------------------------    
;                            |       | deposit:    |     |       |                   |    
;                            |       | dispatch:   |     |       ++++++++            |    
;                            |       +++++++++++++++ -|  |    E2-|amt=40|      ++++++++   
;                            |       |     |          |  |       ++++++++   E3-|amt=60|   
;                       +++++++++++  |   +++++++++++  |  |           |         ++++++++   
;                       |    |    |---   |    |    |---  +++++++++++ |               |    
;                       +++++++++++      +++++++++++ |   |    |    |--   +++++++++++ |    
;                         |                |         |   +++++++++++  ---|    |    |--    
;                       param:m          param:m     |     |          |  +++++++++++      
;                       body:            body:       |   param:amt    |    |              
;                  (define (withdraw amt) ...)       |   body:        |  param:amt        
;                  (define (deposit amt) ...)        -----------------|  body:            
;                  (define (dispatch m) ...)                                              
;
; acc2 makes something similar to acc but with a new E4