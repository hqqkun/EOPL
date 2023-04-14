#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "store.rkt")
(require "schsem.rkt")

(provide value-of-program trace-interp)

(define trace-interp (make-parameter #f))
(define trace-thread-id (make-parameter #f))
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
;; value-of-program : Int * Program -> ExpVal
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
        (value-of/k
          exp1
          (init-env)
          (end-main-thread-cont)))))
)

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (when (trace-interp)
      (eopl:printf "value-of/k: ~s~%" exp))

    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (const-list-exp (nums)
        (apply-cont cont
          (list-val (map num-val nums))))
      (var-exp (var) 
        (apply-cont cont (deref (apply-env env var))))

      ;; Exercise 5-55
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (send-exp (exp1 exp2)
        (value-of/k exp1 env
          (send1-cont exp2 env cont)))
      (recv-exp ()
        (recv-message (get-current-thread-id)
          (a-thread
            (lambda () (apply-cont cont (num-val 123)))
            (get-current-thread-info))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (diff1-cont exp2 env cont)))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)))
      (proc-exp (var body)
        (apply-cont cont
          (proc-val
            (procedure var body env))))
      (call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont)))
      (kill-exp (exp1)
        (value-of/k exp1 env 
          (kill-cont cont)))

      ;! implemented like a macro!
      (let-exp (var exp1 body)
        (value-of/k
          (call-exp (proc-exp var body) exp1)
          env cont))

      (begin-exp (exp exps)
        (if (null? exps)
          (value-of/k exp env cont)
          (value-of/k
            (call-exp
              (proc-exp 
                (fresh-identifier 'dummy)
                (begin-exp (car exps) (cdr exps)))
              exp)
            env cont)))

      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec* p-names b-vars p-bodies env)
          cont))
      (set-exp (id exp)
        (value-of/k exp env
          (set-rhs-cont (apply-env env id) cont)))
      (spawn-exp (exp)
        (value-of/k exp env
          (spawn-cont cont)))
      (mutex-exp ()
        (apply-cont cont
          (mutex-val (new-mutex))))
      (wait-exp (exp)
        (value-of/k exp env
          (wait-cont cont)))
      (signal-exp (exp)
        (value-of/k exp env
          (signal-cont cont)))
      (unop-exp (unop1 exp)
        (value-of/k exp env
          (unop-arg-cont unop1 cont)))
      (yield-exp ()
        (place-on-ready-queue!
          (a-thread
            (lambda ()
              (apply-cont cont (num-val 99)))
              (get-current-thread-info)))
        (run-next-thread))         
      )
  )
)

(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
      (begin
        (place-on-ready-queue!
          (a-thread
            (lambda () (apply-cont cont val))
            (get-current-thread-info)))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation cont
          (end-main-thread-cont ()
            (set-final-answer! val)
            (run-next-thread))
          
          (end-subthread-cont ()
            (run-next-thread))

          ;; Exercise 5-55
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (send1-cont (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (send2-cont val saved-cont)))
          (send2-cont (val1 saved-cont)
            (let
              ( [id (expval->num val)])
              (send-to-thread id val1)
              (apply-cont saved-cont (num-val 124))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (kill-cont (saved-cont)
            (let* 
              ( [need-id (expval->num val)]
                [res (kill need-id)])
              (apply-cont saved-cont (bool-val res))))
          
          (diff1-cont (exp2 saved-env saved-cont)
            (value-of/k exp2 saved-env
              (diff2-cont val saved-cont)))
          (diff2-cont (val1 saved-cont)
            (let 
              ( [n1 (expval->num val1)]
                [n2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (- n1 n2)))))
          (if-test-cont (exp2 exp3 env cont)
            (if (expval->bool val)
              (value-of/k exp2 env cont)
              (value-of/k exp3 env cont)))
            (rator-cont (rand saved-env saved-cont)
              (value-of/k rand saved-env
                (rand-cont val saved-cont)))
            (rand-cont (val1 saved-cont)
              (let ((proc (expval->proc val1)))
                (apply-procedure/k proc val saved-cont)))
            (set-rhs-cont (loc cont)
              (begin
                (setref! loc val)
                (apply-cont cont (num-val 26))))
            ;!
            (spawn-cont (saved-cont)
              (let 
                ( [proc1 (expval->proc val)]
                  [id (get-next-thread-id)])
                (when (trace-thread-id)
                  (eopl:printf "spawn : child id = ~a~%" id))
                (place-on-ready-queue!
                  (a-thread
                    (lambda ()
                      (apply-procedure/k proc1
                        (num-val id) (end-subthread-cont)))
                    (a-thread-info id (get-current-thread-id))))
                (apply-cont saved-cont (num-val id))))
            
            (wait-cont (saved-cont)
              (let ([m (expval->mutex val)])
                (wait-for-mutex m
                  (a-thread
                    (lambda ()
                      (apply-cont saved-cont (num-val 52)))
                    (get-current-thread-info)))))
            (signal-cont (saved-cont)
              (signal-mutex
                (expval->mutex val)
                (a-thread
                  (lambda () (apply-cont saved-cont (num-val 53)))
                  (get-current-thread-info))))
            (unop-arg-cont (unop1 cont)
              (apply-unop unop1 val cont))
          ))
    ))
)

(define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var (newref arg) saved-env)
            cont))))
)

(define apply-unop
  (lambda (unop1 arg cont)
    (cases unop unop1
      (zero?-unop ()
        (apply-cont cont 
          (bool-val (zero? (expval->num arg)))))
      (car-unop ()
        (let ([lst (expval->list arg)])
          (apply-cont cont (car lst))))
      (cdr-unop ()
        (let ([lst (expval->list arg)])
          (apply-cont cont (list-val (cdr lst)))))
      (null?-unop ()
        (apply-cont cont
          (bool-val (null? (expval->list arg)))))
      (print-unop ()
        (begin
          (eopl:printf "~a~%" (expval->num arg))
          (apply-cont cont (num-val 1))))
    ))
)