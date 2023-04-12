#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite with (run-all N), where N is the size of the
;; time slice.

;;; interface for book test ;;;
(provide test-all)
(define (test-all)
  (run-all 50))

(require "drscheme-init.rkt")
(require "data-structures.rkt")
(require "lang.rkt")                  ; for scan&parse
(require "interpreter.rkt")           ; for value-of-program
(require "tests.rkt")                 ; for test-list

;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))

(define run-all
  (lambda (timeslice)
    (run-tests!
     (lambda (string) (run timeslice string))
     equal-answer? test-list)))

(define run-one
  (lambda (timeslice test-name)
    (let ((the-test (assoc test-name test-list)))
      (cond
        ((assoc test-name test-list)
         => (lambda (test)
              (run timeslice (cadr test))))
        (else (eopl:error 'run-one "no such test: ~s" test-name))))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
      (else
       (eopl:error 'sloppy->expval
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))


(stop-after-first-error #t)

(define wait-all-threads
  "let x = 0 in
   let mut1 = mutex() in
   let mut2 = mutex() in
   let mut3 = mutex() in
   let mut =  mutex() in
   let proc1 =
    proc(id)
    proc(d)
      begin
        wait(mut);
        set x = -(x,-1);
        signal(mut);
        if =(id, 100) then signal(mut1) 
        else if =(id, 200) then signal(mut2)
        else signal(mut3)
      end
   in
    begin
      spawn((proc1 100));
      spawn((proc1 200));
      spawn((proc1 300));
      wait(mut1);
      wait(mut2);
      wait(mut3);
      wait(mut1);
      wait(mut2);
      wait(mut3);
      print(x)
    end"
)

; ! this will need to add =(exp1, exp2)
; (run 1 wait-all-threads)

; version 2
(define wait-all-threads-2
  "let x = 0 in
   let mut = mutex() in
   let incr_x = 
    proc(id)
      let mut-main = mutex() in
      begin
        wait(mut-main);
        spawn(proc(d) begin wait(mut); set x = -(x, -1); signal(mut); signal(mut-main) end);
        mut-main
      end
  in 
  let mut1 = (incr_x 100) in
  let mut2 = (incr_x 200) in
  let mut3 = (incr_x 300) in
  begin
    wait(mut1);
    wait(mut2);
    wait(mut3);
    print(x)
  end")

(run 4 wait-all-threads-2)