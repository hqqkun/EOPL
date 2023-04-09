#lang eopl

(require "drscheme-init.rkt")
(require "queues.rkt")
(require "data-structures.rkt")       ; for continuation?
(require "lang.rkt")                  ; for expval?

(provide initialize-scheduler! set-final-answer!
  time-expired? decrement-timer! place-on-ready-queue!
  run-next-thread)


;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

;; components of the scheduler state:

(define the-ready-queue   'uninitialized)
(define the-final-answer  'uninitialized)

(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)

;; initialize-scheduler! : Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice))
)

; th = thread
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
      (enqueue the-ready-queue th)))
)

;; run-next-thread : () -> FinalAnswer
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
      the-final-answer
      (dequeue the-ready-queue
        (lambda (first-ready-thread other-ready-threads)
          (set! the-ready-queue other-ready-threads)
          (set! the-time-remaining the-max-time-slice)
          (first-ready-thread)))))
)

;; set-final-answer! : ExpVal -> Unspecified
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val))
)

;; time-expired? : () -> Bool
(define time-expired?
  (lambda ()
    (zero? the-time-remaining))
)

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining
      (- the-time-remaining 1)))
)
