#lang mzscheme
  
(provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
(define test-list
  '(
    (fact 
    "let makemult = proc(Y) proc(x) proc(y)
      if zero?(x) then 0 else -((((Y Y) -(x, 1)) y), -(0,y))
     in 
      let mult = proc(x) proc(y) (((makemult makemult) x) y)
      in
        let makefact = proc(Y) proc(n) 
          if zero?(n) then 1 else
            if zero?(-(n,1)) then 1 else 
              ((mult ((Y Y) -(n, 1))) n)
        in 
          let fact = proc(n) ((makefact makefact) n)
          in (fact 5)" 120)
   )
)