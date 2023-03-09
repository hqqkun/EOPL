#lang eopl

; (value-of exp1 ρ σ0) = (val1 σ1)
; --------------------------------------------------
; (value-of (begin-exp exp1 null) ρ σ0) = (val1 σ1)

; (value-of (begin-exp exp1 (cons exp2 rest)) ρ σ0)
;   = (value-of (begin-exp exp2 rest) ρ σ1)
