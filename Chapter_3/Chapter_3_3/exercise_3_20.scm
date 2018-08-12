; Exercise 3.20: Draw environment diagrams to illustrate the evaluation of the
; sequence of expressions

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)

; using the procedural implementation of pairs given above. 
; (Compare Exercise 3.11.)

; Implement in pure procedures

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) 'set-x!)
          ((eq? m 'set-cdr!) 'set-y!)
          (else
            (error "Undefined operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)


;Environment model
;              +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++               
; global ----> |         x:---       ^               z:--     ^              |               
;              ++++++++++++++|+++++++|+++++++++++++++++++|++++|+++++++++++++++               
;                        ___ | _____ | __________________|   _| ___________________          
;                       |    |       |              ________| |                     |        
;                       |    |       +++++++++++++++     +++++++++++++++            |        
;                       |    |    E1-| x: 1 y: 2   |  E2-| x: x y: x   |            |        
;                       |    |       | set-x!:     |     | set-x!:     |            |        
;                       |    |       | set-y!:     |     | set-y!:     |            |        
;                       |    |       | dispatch:   |     | dispatch:   |            +++++++++
;                       |    |       +++++++++++++++     +++++++++++++++         E3-| m:'car|
;                       |    |       |          |         |     |                   +++++++++
;                       +++++++++++  |          |         |     |___________ +++++++++++  |  
;                       |    |    |------------------------                  |    |    |---  
;                       +++++++++++             |               +++++++++    +++++++++++     
;                         |                     |            E4-| v:17  |       |            
;                       param:x,y               +++++++++++     +++++++++   param: m         
;                       body:                   |    |    |-----|           body:            
;                  (define (set-x! v) ...)      +++++++++++                  ...             
;                  (define (set-y! v) ...)        |                                          
;                  (define (dispatch m) ...)    param: v                                     
;                                               body:                                        
;                                               (define (set-x! v)                           
;                                               (set! x v))                                  
;                                                                                            
; The last car is the same as E3 but coming from the x envrionment instead of the
; z environment
