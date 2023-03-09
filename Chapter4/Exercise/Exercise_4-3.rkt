#lang eopl

; (value-of rator ρ σ0) = (val1 σ1)
; (value-of rand  ρ σ1) = (val2 σ2)
; (expval->proc val1) = proc
; ---------------------------------------
; (value-of (call-exp rator rand) ρ σ0) 
;   = (apply-procedure proc val2 σ2)