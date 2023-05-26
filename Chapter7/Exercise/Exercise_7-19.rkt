#lang eopl

; p : var is unknown type
; r : var is unbound
; s : var can shown in the right hand side in a subst.

; We have :
;   p -> r ∨ s
;   ¬r -> ¬s
;  and I need to prove p -> r

; p, p -> r ∨ s |- r ∨ s
; --------------------------------
; if r then done.
; if s , then s, ¬r -> ¬s |- ¬¬r , which means r

;! so, in both case, we get r, which means p -> r