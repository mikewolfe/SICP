;Exercise 2.55. Eva Lu Ator types to the interpreter the expression 
;(car ’’abracadabra) 
;To her surprise, the interpreter prints back quote. Explain.

; the ' symbol is actually syntatic sugar for the quote function. Here she
; is effectively saying (quote ' abracadabra) which evaluates to
; (quote (quote abracadabra)) this then evaluates to the list (quote abracadabra)
; which when we take the car, gives us back quote

(car ''abracadbra)
