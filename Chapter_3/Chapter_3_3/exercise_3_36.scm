; Exercise 3.36 Suppose we evaluate the following sequence of expressions in
; the global environment:

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

; At some time during the evaluation of the set-value!, the following expression
; from the connectors local procedure is evaluated:

(for-each-except
  setter inform-about-value constraints)

; Draw an environment diagram showing the environment in which the above
; expression is evaluated.

; Rather than make the big environment diagram I am going to just put it in
; words here.
;
; (make-connector) --> makes a local environment where value, informant and
; constraints live
; (for-each-except is called in this local environment and passes the local
; informant, the global function inform-about-value, and the local constraints
; We then dive into the for-each environment with those values.
;
; So essentially its being evaluated within the local environment of the a
; connector


