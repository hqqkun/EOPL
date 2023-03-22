#lang eopl

;; interpreter for the IMPLICIT-REFS language

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "pairvals.rkt")

(provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ; pair
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (newpair-exp (exp1 exp2)
          (let
            ( [val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (mutpair-val (make-pair val1 val2))))

        (left-exp (exp1)
          (let* 
            ( [val1 (value-of exp1 env)]
              [p1 (expval->mutpair val1)])
            (left p1)))

        (right-exp (exp1)
          (let*
            ( [val1 (value-of exp1 env)]
              [p1 (expval->mutpair val1)])
            (right p1)))
        
        (setleft-exp (exp1 exp2)
          (let*
            ( [val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)]
              [p1 (expval->mutpair val1)])
            (begin
              (setleft p1 val2)
              (num-val 82))))
        
        (setright-exp (exp1 exp2)
          (let* 
            ( [val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)]
              [p1 (expval->mutpair val1)])
            (begin
              (setright p1 val2)
              (num-val 83))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))

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
                (args (map (lambda (rand) (value-of-oprand rand env)) rands)))
            (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

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

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

;   uninstrumented version
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (let*
          ( [new-refs (map (lambda (ref) (newref (deref ref))) vals)]
            [new-env (extend-env* vars new-refs saved-env)]
            [body-val (value-of body new-env)])
          (letrec
            ( [S  (lambda (old-refs new-refs)
                    (if (null? old-refs)
                    #t
                    (begin
                      (setref! (car old-refs) (deref (car new-refs)))
                      (S (cdr old-refs) (cdr new-refs)))))])
            (begin
              (S vals new-refs)
              body-val))))))
)
  
;   ;; instrumented version
;   (define apply-procedure
;       (lambda (proc1 val)
;         (cases proc proc1
;           (procedure (var body saved-env)
;             (let ((new-env (extend-env var val saved-env)))
;           (when (instrument-let)
;             (begin
;               (eopl:printf
;             "entering body of proc ~s with env =~%"
;             var)
;           (pretty-print (env->list new-env))
;                   (eopl:printf "store =~%")
;                   (pretty-print (store->readable (get-store-as-list)))
;           (eopl:printf "~%")))
;             (value-of body new-env)))))) 

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))



; value-of-oprand : Exp * Env -> Ref
(define value-of-oprand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else 
        (eopl:error 'value-of-oprand "~s is not var-exp" exp))))
)