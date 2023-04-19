#lang eopl


;   (begin
;     (set! cont (fact1-cont n cont))
;     (set! n (- n 1))
;     (set! pc fact/k))

; in fact/k, if n is not zero, then pc will always be fack/k,
; so no need to set it again.