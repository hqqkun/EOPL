#lang eopl

; (value-of exp1 ρ σ0) = (val1, σ1)
;-----------------------------------------
; (value-of (let-exp var exp1 body)) = 
;   (value-of body [var = l]ρ [l = val1]σ1)