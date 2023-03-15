#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")

(require dyoo-while-loop)

(provide value-of-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmt1)
        (initialize-store!)
        (result-of stmt1 (init-env))))
  ))


(define result-of
  (lambda (stmt env)
    (cases statement stmt

      (assign-stmt (var exp)
        (setref! 
          (apply-env env var)
          (value-of exp env)))  
      
      (print-stmt (exp1)
        (cases expval (value-of exp1 env)
          (num-val (num) 
            (eopl:printf "~s\n" num))
          (bool-val (bool)
            (eopl:printf "~s\n" bool))
          (ref-val (ref)
            (eopl:printf "loc : ~s\n" ref))
          (proc-val (proc)
            (eopl:printf "proc : ~s\n" proc))))

        (seqs-stmt (stmts)
          (map (lambda (stmt) (result-of stmt env)) stmts))
        
        (if-stmt (exp1 stmt1 stmt2)
          (if (expval->bool (value-of exp1 env))
            (result-of stmt1 env)
            (result-of stmt2 env)))

        (block-stmt (vars stmt1)
          (let 
            ( [new-env (extend-env-uninit* vars env)])
            (result-of stmt1 new-env)))
        
        (block-init-stmt (vars exps stmt1)
          (let* 
            ( [vals (map (lambda (exp) (value-of exp env)) exps)]
              [new-env (extend-env* vars vals env)])
            (result-of stmt1 new-env)))

        (while-stmt (exp1 stmt1)
          (while  
            (expval->bool (value-of exp1 env))
            (result-of stmt1 env)))
    )
  )
)

(define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))

        (not-exp (exp1)
          (let
            ([bool1 (expval->bool (value-of exp1 env))])
            (if bool1
              (bool-val #f)
              (bool-val #t))))

        (mul-exp (exp1 exp2)
          (let
            ( [num1 (expval->num (value-of exp1 env))]
              [num2 (expval->num (value-of exp2 env))])
            (num-val (* num1 num2))))

        (add-exp (exp1 exp2)
          (let
            ( [num1 (expval->num (value-of exp1 env))]
              [num2 (expval->num (value-of exp2 env))])
            (num-val (+ num1 num2))))
        
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env)) rands)))
            (apply-procedure proc args)))

        (letrec-exp (p-names list-b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names list-b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        ))
)

; uninstrumented version
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body
          (extend-env* vars vals saved-env)))))
)