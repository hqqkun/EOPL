#lang eopl

(require "store.rkt")    
(require "drscheme-init.rkt")
(require "queues.rkt")
(require "data-structures.rkt")       ; for continuation?
(require "lang.rkt")                  ; for expval?

(provide (all-defined-out))

;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

;; components of the scheduler state:

(define the-ready-queue   'uninitialized)
(define the-final-answer  'uninitialized)

(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)

; Exercise 5-53
(define next-thread-id 'uninitialized)
; Exercise 5-54
(define current-thread-info 'uninitialized)
(define the-mutex-queue 'uninitialized)

;; initialize-scheduler! : Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)
    (set! current-thread-info (a-thread-info 0 -1))
    (set! next-thread-id 1)
    (set! the-mutex-queue '())
    (set! message-wait-queue '())
    (set! messages '()))
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
          (apply-thread first-ready-thread)))))
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

(define get-next-thread-id
  (lambda ()
    (let ([id next-thread-id])
      (set! next-thread-id (+ next-thread-id 1))
      id))
)

(define get-current-thread-id
  (lambda ()
    (cases thread-info current-thread-info
      (a-thread-info (id parent-id)
        id)))
)

(define get-current-thread-info
  (lambda ()
    current-thread-info)
)

(define apply-thread
  (lambda (th)
    (cases thread th
      (a-thread (proc th-info)
        (set! current-thread-info th-info)
        (proc))))
)

(define get-id-from-thread
  (lambda (th)
    (cases thread th
      (a-thread (proc th-info)
        (cases thread-info th-info
          (a-thread-info (id parent-id)
            id)))))
)

(define kill-ready-queue
  (lambda (thread-id)
    (let ([find #f])
      (letrec
        ( [K  (lambda (lst)
                (cond
                  ((null? lst) '())
                  ((eq? thread-id (get-id-from-thread (car lst))) 
                    (begin (set! find #t) (cdr lst)))
                  (else (cons (car lst) (K (cdr lst))))))])
        (set! the-ready-queue (K the-ready-queue))
        find)))
)

(define kill-mutex-queue
  (lambda (thread-id)
    (let
      ( [find #f])
      (letrec
        ( [K  (lambda (lst)
                (if (null? lst)
                  '()
                  (cases mutex (car lst)
                      (a-mutex (ref-to-closed? ref-to-wait-queue)
                        (begin
                          (setref! ref-to-wait-queue (Kill-mq (deref ref-to-wait-queue)))
                          (if find
                            #t
                            (K (cdr lst))))))))]
          [Kill-mq  (lambda (lst)
                      (cond
                        ((null? lst) '())
                        ((eq? thread-id (get-id-from-thread (car lst))) 
                          (begin (set! find #t) (cdr lst)))
                        (else (cons (car lst) (Kill-mq (cdr lst))))))])
        (K the-mutex-queue)
        find)))
)


(define kill
  (lambda (thread-id)
    (or 
      (kill-ready-queue thread-id)
      (kill-mutex-queue thread-id)))
)



(define instrument-mutexes (make-parameter #f))

;; implements binary semaphores (mutexes).

(define new-mutex
  (lambda ()
    (let
      ( [mut 
          (a-mutex
            (newref #f)
            (newref '()))])
      (set! the-mutex-queue (cons mut the-mutex-queue))
      mut))
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
              (apply-thread th)))))))
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
          (apply-thread th)))))    
)



;; Exercise 5-55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define message-wait-queue 'uninitialized)
(define messages 'uninitialized)
(define send-to-thread
  (lambda (id val)
    (letrec
      ( [S  (lambda (lst)
              (cond
                ((null? lst) '())
                ((eq? id (get-id-from-thread (car lst)))
                  (place-on-ready-queue! (car lst)) (cdr lst))
                (else (cons (car lst) (S (cdr lst))))))])
      (set! message-wait-queue (S message-wait-queue))
      (set! messages (enqueue messages (cons id val)))))
)

;; find if messages contains message sending to it.
;; if no, block this thread by adding itself to `message-wait-queue`
(define recv-message
  (lambda (id th)
    (letrec
      ( [find #f]
        [R  (lambda (lst)
              (cond
                ((null? lst) 
                  (set! message-wait-queue (enqueue message-wait-queue
                    (cases thread th
                      (a-thread (_ th-info)
                        (a-thread 
                          (lambda () (recv-message id th))
                          th-info))))) '())
                ((eq? id (caar lst))
                  (eopl:printf "receive ~a~%" (expval->num (cdar lst)))
                  (set! find #t)
                  (cdr lst))
                (else (cons (car lst) (R (cdr lst))))))])
      (set! messages (R messages))
      (if find
        (apply-thread th)
        (run-next-thread))))
)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;