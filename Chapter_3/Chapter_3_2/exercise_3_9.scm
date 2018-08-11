; Exercise 3.9: In Section 1.2.1 we used the substitution model to analyze
; two procedures for computing factorials, a recursive version

(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))
 
; and an iterative version

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Show the environment structures created by evaluating (factorial 6) using
; each version of the factorial procedure.
;
;
; Version 1
;              +++++++++++++++++++++++                           
; global ----> | factorial:---  ^    |                           
;              ++++++++++++++|++|+++++                           
;                            |  |                                
;                            |  |                                
;                       +++++++++++                              
;                       |    |    |                              
;                       +++++++++++                              
;                         |                                      
;                       parameters: n                            
;                       body:                                    
;                       (if (= n 1) 1 (* n (factorial (- n 1)))))
;                                                                
;Environment model;
;        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
;global->|                                                                                |     
;        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
;        |               |               |               |               |               |      
;     ++++++          ++++++          ++++++          ++++++          ++++++          ++++++    
; E1->|n:6 |      E2->|n:5 |      E3->|n:4 |      E4->|n:3 |      E5->|n:2 |      E6->|n:1 |    
;     ++++++          ++++++          ++++++          ++++++          ++++++          ++++++    
;(if (= 6 1) 1   (if (= 5 1) 1   (if (= 4 1) 1   (if (= 3 1) 1   (if (= 2 1) 1   (if (= 1 1) 1  
;(* 6 (factorial (* 5 (factorial (* 4 (factorial (* 3 (factorial (* 2 (factorial (* 1 (factorial
;(- 6 1)))))     (- 5 1)))))     (- 4 1)))))     (- 3 1)))))     (- 2 1)))))     (- 1 1)))))    
;
;
; Version 2;
;              ++++++++++++++++++++++++++++++++++++++++++++++++++++
; global ----> | factorial:---  ^        fact-iter:---  ^         |
;              ++++++++++++++|++|++++++++++++++++++++|++|++++++++++
;                            |  |                    |  |          
;                            |  |                    |  |          
;                       +++++++++++             +++++++++++        
;                       |    |    |             |    |    |        
;                       +++++++++++             +++++++++++        
;                         |                       |                
;                       parameters: n           parameters:        
;                       body:                   product            
;                       (fact-iter 1 1 n)       counter            
;                                               max-count          
;                                               body:              
;                                               (if (> counter     
;                                                    max-count)    
;                                                    product       
;                                                    (fact-iter    
;                                                    (* counter    
;                                                    product)      
;                                                    (+ counter 1) 
;                                                    max-count)))  
;                                                                  
;
;Environment model;
;        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;global->|                                                                                                                                                 |
;        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;        |            |                  |                 |                  |                  |                   |                  |                   
;     +++++++      +++++++++++++      +++++++++++++     +++++++++++++      +++++++++++++      +++++++++++++       +++++++++++++      +++++++++++++          
; E1->|n: 6 |  E2->|product:1  |  E3->|product:1  | E4->|product:2  |  E5->|product:6  |  E6->|product:24 |   E7->|product:120|  E8->|product:720|          
;     +++++++      |counter:1  |      |counter:2  |     |counter:3  |      |counter:4  |      |counter:5  |       |counter:6  |      |counter:7  |          
;  (fact-iter      |max-count:6|      |max-count:6|     |max-count:6|      |max-count:6|      |max-count:6|       |max-count:6|      |max-count:6|          
;   1 1 6)         +++++++++++++      +++++++++++++     +++++++++++++      +++++++++++++      +++++++++++++       +++++++++++++      +++++++++++++          
;              (if (> 1 6)        (if (> 2 6)       (if (> 3 6)        (if (> 4 6)        (if (> 5 6)         (if (> 6 6)        (if (> 7 6)                
;                  1                  1                 2                  6                  24                  120                720                    
;                  (fact-iter         (fact-iter        (fact-iter         (fact-iter         (fact-iter          (fact-iter         (fact-iter             
;                   (* 1 1)            (* 2 1)           (* 3 2)            (* 4 6)            (* 5 24)            (* 6 120)          (* 7 720)             
;                   (+ 1 1) 6)))       (+ 2 1) 6)))      (+ 3 1) 6)))       (+ 4 1) 6)))       (+ 5 1) 6)))        (+ 6 1) 6)))       (+ 7 1) 6)))          
