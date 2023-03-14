#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite with (run-all).

(require "drscheme-init.rkt")
(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interpreter.rkt")           ; for value-of-program
(require "tests.rkt")            ; for test-list


;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define t0 "var x,y; {x = 3; y = 4; print +(x,y)}")
(define t1 "var x,y,z; 
              { x = 3;
                y = 4;
                z = 0;
                while not(zero?(x))
                {z = +(z,y); x = -(x,1)};
                print z}")
(define t2 " var x; 
              { x = 3; % Example 3
                print x;
                var x; {x = 4; print x};
                print x}")
(define t3 "var f,x; 
            { f = proc(x,y) *(x,y); % Example 4
              x = 3;
              print (f 4 x)}")
(define t4 "var x;
            { read x;
              print x}")

(define run-tests
  (lambda ()
    (eopl:printf ";;;;;;;;;;;;; test 0 ;;;;;;;;;;;;;;\n")
    (run t0)
    (eopl:printf ";;;;;;;;;;;;; test 1 ;;;;;;;;;;;;;;\n")
    (run t1)
    (eopl:printf ";;;;;;;;;;;;; test 2 ;;;;;;;;;;;;;;\n")
    (run t2)
    (eopl:printf ";;;;;;;;;;;;; test 3 ;;;;;;;;;;;;;;\n")
    (run t3)
    (eopl:printf ";;;;;;;;;;;;; test 4 ;;;;;;;;;;;;;;\n")
    (run t4))
)

(run-tests)