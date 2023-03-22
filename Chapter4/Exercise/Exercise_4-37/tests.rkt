#lang mzscheme

(provide test-list)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
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
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)
          
      (begin-test-1
        "begin 1; 2; 3 end"
        3)

      (assignment-test-1 "let x = 17
                          in begin set x = 27; x end"
        27)

      
      (simple-mutpair-left-1 "let p = newpair(22,33) in left(p)" 22)
      (simple-mutpair-right-1 "let p = newpair(22,33) in right(p)" 33)

      (simple-mutpair-setleft-1 "
let p = newpair(22,33) in begin setleft p = 77; left(p) end" 77)

      (simple-mutpair-setleft-2 "
let p = newpair(22,33) in begin setleft p = 77; right(p) end" 33)


      (simple-mutpair-setright-1 "
let p = newpair(22,33) in begin setright p = 77; right(p) end" 77)

      (simple-mutpair-setright-2 "
let p = newpair(22,33) in begin setright p = 77; left(p) end" 22)


      
      ;; new for call-by-reference

      (cbr-global-aliasing-1
        "let p = proc (z) set z = 44
         in let x = 33
         in begin (p x); x end"
        44)

      (cbr-direct-aliasing-1
        "let p = proc (x) proc (y)
                   begin
                    set x = 44;
                    y
                   end
         in let b = 33
         in ((p b) b)"
        33)


      (cbr-example-for-book "
let f = proc (x) set x = 44
in let g = proc (y) (f y)
in let z = 55
in begin
    (g z);
    z
  end"
        44)

      (cbr-swap-1
        "let swap = proc (x, y)
                      let temp = x
                      in begin 
                          set x = y;
                          set y = temp
                         end
         in let a = 33
         in let b = 44
         in begin
             (swap a b);
             -(a,b)
            end"
        11)
      )
)