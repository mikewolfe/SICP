; Exercise 2.76. As a large system with generic operations evolves, new types of
; data objects or new operations may be needed. For each of the three strategies
; -- generic operations with explicit dispatch, data-directed style, and
; message-passing-style -- describe the changes that must be made to a system in
; order to add new types or new operations. Which organization would be most
; appropriate for a system in which new types must often be added? Which would
; be most appropriate for a system in which new operations must often be added?
;
; explicit dispatch
; new type - change all generic operators that will use the new type
; new operation - write code for all the types that will use the new operation
; Things above this layer of abstraction will be impacted until code is changed
;
; data-directed
; new type -  don't need to change the operators at all but need to write
;             code specific for this type for each operator to be used
; new operation - only have to write code for each type that will use the new
;                 operator
; Things above this layer of abstraction will not be modified. Changes can be
; rolled out for noew types or operations without impacting existing code
;
; message-passing -
; new type - write code for all operations to be used with this type
; new operation - write code for each type that will use the new operation
; Just like data-directed, things above this layer of abstraction will not
; be modified.
;
; Both data-directed and message-passing are preferable to explicit dispatch,
; however, either can be used for systems where new types or new operations 
; are added. It seems that adding new types is easier under data-directed
; whereas adding new operations is easier under message passing.
