#lang mzscheme

(provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; cons test
      (empty-list "emptylist" ())
      (cons-list "cons(1, cons(2, emptylist))" (1 2))
      (list-in-let "let x = 4
                      in cons(x,
                          cons(cons(-(x,1),
                                    emptylist),
                                emptylist))" (4 (3)))
      (next-list "let x = cons(1, cons(2, emptylist)) in 
                    cons(x, cons(x, emptylist))" ((1 2) (1 2)))
      (car-of-list "let x = cons(1, cons(2, emptylist)) in 
                    car(x)" 1)
      (cdr-of-list "let x = cons(1, cons(2, emptylist)) in
                   cdr(x)" (2))
      (null?-list  "let x = cons(1, cons(2, emptylist)) in null?(x)" #f)
      (null?-list-1 "let x = cons(1, cons(2, emptylist)) in null?(cdr(cdr(x)))" #t)

      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
      (simple-minus "minus(15)" -15)
      (nested-minus "minus(-(minus(5),9))" 14)

      ; numeric predicate
      (equal?-0 "equal?(1,1)" #t)
      (equal?-1 "equal?(1,2)" #f)
      (greater?-0 "greater?(1,2)" #f)
      (less?-0 "less?(1,2)" #t)
      
      ;; add, mul and div
      (add "+(1,2)" 3)
      (mul "*(4,5)" 20)
      (div "/(8,2)" 4)
      (div "/(8,3)" 2)
      (nested-add "+(+(1,2), +(3,4))" 10)
      (nested-add-mul "+(*(2,3), *(3,4))" 18)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2))
)