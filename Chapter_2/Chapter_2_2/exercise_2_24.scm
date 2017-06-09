; Exercise 2.24. Suppose we evaluate the expression (list 1 (list 2 (list 3 4)))
; Give the result printed by the interpreter, the corresponding box-and-pointer
; structure, and the interpretation of this as a tree (as in figure 2.6).

; The machine would print out:
;( 1 ( 2 (3 4)))
(list 1 (list 2 (list 3 4)))

; Box and pointer structure                         
;  ___ ___      ___ ___                             
; |   |   | -- |   | x |                            
;  --- ---      --- ---                             
;   |            |                                  
;   |            |                                  
;   1           ___ ___      ___ ___                
;              |   |   | -- |   | x |               
;               --- ---      --- ---                
;                |            |                     
;                |            |                     
;                2                                  
;                             ___ ___      ___ ___  
;                            |   |   | -- |   | x | 
;                             --- ---      --- ---  
;                              |            |       
;                              |            |       
;                              3            4       
; Tree structure                                    
;                                                   
;      (1 (2 (3 4)))                                
;           |                                       
;        /  |                                       
;       /   |                                       
;      1  (2 (3 4))                                 
;           |                                       
;         /  \                                      
;        2    (3 4)                                 
;               |                                   
;              / \                                  
;             3   4                                 
