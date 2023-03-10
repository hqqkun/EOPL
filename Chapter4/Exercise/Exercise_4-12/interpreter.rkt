#lang eopl

;; interpreter for the EXPLICIT-REFS language

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of instrument-let)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

(define instrument-let (make-parameter #f))


(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env) (empty-store)))))
)


(define value-of
  (lambda (exp env store)
    (cases expression exp

      (const-exp (num)
        (an-answer (num-val num) store))
      
      (var-exp (var)
        (an-answer
          (apply-env env var)
          store))

      (diff-exp (exp1 exp2)
        (let* 
          ( [res1 (value-of exp1 env store)]
            [num1 (expval->num (answer->val res1))]
            [store1 (answer->store res1)]
            [res2 (value-of exp2 env store1)]
            [num2 (expval->num (answer->val res2))]
            [store2 (answer->store res2)])
          (an-answer
            (num-val (- num1 num2))
            store2)))
  
      (zero?-exp (exp1)
        (let*
          ( [res1 (value-of exp1 env store)]
            [num1 (expval->num (answer->val res1))]
            [store1 (answer->store res1)])
          (if (zero? num1)
            (an-answer (bool-val #t) store1)
            (an-answer (bool-val #f) store1))))

      (if-exp (exp1 exp2 exp3)
        (cases answer (value-of exp1 env store)
          (an-answer (val new-store)
            (if (expval->bool val)
              (value-of exp2 env new-store)
              (value-of exp3 env new-store)))))

      (let-exp (var exp1 body)
        (let* 
          ( [res1 (value-of exp1 env store)]
            [val1 (answer->val res1)]
            [store1 (answer->store res1)])
          (value-of body (extend-env var val1 env) store1)))   
      
      (proc-exp (var body)
        (an-answer
          (proc-val (procedure var body env))
          store))

      (call-exp (rator rand)
        (let*
          ( [res1 (value-of rator env store)]
            [proc (expval->proc (answer->val res1))]
            [store1 (answer->store res1)]
            [res2 (value-of rand env store1)]
            [arg (answer->val res2)]
            [store2 (answer->store res2)])
          (apply-procedure proc arg store2)))

      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec* p-names b-vars p-bodies env) store))

      (begin-exp (exp1 exps)
        (letrec
          ( [value-of-begins
              (lambda (e1 es sto)
                (let* 
                  ( [res1 (value-of e1 env sto)]
                    [val1 (answer->val res1)]
                    [sto1 (answer->store res1)])
                  (if (null? es)
                    (an-answer val1 sto1)
                    (value-of-begins (car es) (cdr es) sto1))))])
          (value-of-begins exp1 exps store)))
      
      (newref-exp (exp1)
        (let* 
          ( [res1 (value-of exp1 env store)]
            [val1 (answer->val res1)]
            [store1 (answer->store res1)])
          (newref store1 val1)))

      (deref-exp (exp1)
        (let*
          ( [res1 (value-of exp1 env store)]
            [ref1 (expval->ref (answer->val res1))]
            [store1 (answer->store ref1)])
          
          (an-answer
            (deref store1 ref1)
            store1)))

      (setref-exp (exp1 exp2)
        (let* 
          ( [res1 (value-of exp1 env store)]
            [ref (expval->ref (answer->val res1))]
            [store1 (answer->store res1)]
            [res2 (value-of exp2 env store1)]
            [v2 (answer->val res2)]
            [store2 (answer->store res2)])
            
          (an-answer
            (num-val 23) (setref! store ref v2))))
      ))
)

;   apply-procedure : Proc * ExpVal -> ExpVal
;   uninstrumented version
(define apply-procedure
  (lambda (proc1 arg store)
    (cases proc proc1
      (procedure (bvar body saved-env)
        (value-of body (extend-env bvar arg saved-env) store))))
)
