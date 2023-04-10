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
; (run-all 5)
; (run-one 1000 'safe-ctr)

(define safe-ctr
        "let mut1 = mutex() in
         let mut2 = mutex() in 
         let mut3 = mutex() in 
         let mut =  mutex() in
         let x = 0 in 
         let incr_x = 
          proc(id)
            proc(dummy)
              begin
                wait(mut);
                set x = -(x,-1);
                print(x);
                signal(mut);
                if =(id, 100) then signal(mut1) 
                else if =(id, 200) then signal(mut2)
                else signal(mut3)
              end
         in begin
              spawn((incr_x 100));
              spawn((incr_x 200));
              spawn((incr_x 300))
              wait(mut1);
              wait(mut2);
              wait(mut3);
              wait(mut1);
              wait(mut2);
              wait(mut3);
            end
          "
)

(run 1000 safe-ctr)
