#lang eopl

; 1   (int -> int)
; 2   ((t -> int) -> (t -> int))
; 3   (t -> t)
; 4   ((t1 -> t2) -> (t1 -> t2))
; 5   ((int -> t) -> t)
; 6   cannot
; 7   (bool -> int)
; 8   (bool -> (int -> int))
; 9   type error
; 10  type error
; 11  ((t -> int) -> ((int -> int) -> ((int -> int) -> (t -> int))))
; 12  too much
; x : int, p : (int -> bool), f : ((int -> bool) -> int)

; 13  cannot