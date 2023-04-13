#lang eopl

(require "drscheme-init.rkt")
(require "store.rkt")                    ; for store ops
(require "data-structures.rkt")          ; for lock, a-lock
(require "scheduler.rkt")                ; for os calls
(require "queues.rkt")

(provide (all-defined-out))

(define instrument-mutexes (make-parameter #f))

;; implements binary semaphores (mutexes).

(define new-mutex
  (lambda ()
    (a-mutex
      (newref #f)
      (newref '())))
)

; wait-for-mutex : Mutex * Thread -> FinalAnswer
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let 
          ( [closed (deref ref-to-closed?)]
            [queue (deref ref-to-wait-queue)])
          (cond
            (closed 
              (setref! ref-to-wait-queue
                 (enqueue queue th))
              (run-next-thread))
            (else
              (setref! ref-to-closed?  #t)
              (th)))))))
)

;; signal-mutex : Mutex * Thread -> FinalAnswer
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let
          ( [closed (deref ref-to-closed?)]
            [queue (deref ref-to-wait-queue)])
          (when closed
            (if (null? queue)
              (setref! ref-to-closed? #f)
              (dequeue queue
                (lambda (first-waiting-th other-waiting-ths)
                  (setref! ref-to-wait-queue
                     other-waiting-ths)
                  (place-on-ready-queue! first-waiting-th)))))
          (th)))))    
)
