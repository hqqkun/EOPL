#lang eopl

; (value-of exp1 ρ σ0) = (val1 σ1)
; -----------------------------------------
; (value-of (zero?-exp exp1) ρ σ0)
;  = 1. ((bool-val #t) σ1)    if (expval->num val1) == 0
;    2. ((bool-val #f) σ1)    if (expval->num val1) != 0
