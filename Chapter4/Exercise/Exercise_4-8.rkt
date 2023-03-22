#lang eopl

; newref uses `append` to create a new list, which is a linear time.
; deref uses `list-ref`, which is a linear time.
; setref! makes a new list, which is a linear time.