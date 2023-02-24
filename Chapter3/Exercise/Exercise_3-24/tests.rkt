#lang mzscheme
  
(provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
(define test-list
  '(
    (odd 
      "let make-odd = proc(odd) proc(even) proc(n)
        if zero?(n) then 0 else (((even even) odd) -(n, 1))
      in let make-even = proc(even) proc(odd) proc(n) 
            if zero?(n) then 1 else (((odd odd) even) -(n, 1))
         in let odd? = proc(n) (((make-odd make-odd) make-even) n)
            in (odd? 5)" 1)
    (even 
      "let make-odd = proc(odd) proc(even) proc(n)
        if zero?(n) then 0 else (((even even) odd) -(n, 1))
      in let make-even = proc(even) proc(odd) proc(n) 
            if zero?(n) then 1 else (((odd odd) even) -(n, 1))
         in let even? = proc(n) (((make-even make-even) make-odd) n)
            in (even? 5)" 0)
   )
)