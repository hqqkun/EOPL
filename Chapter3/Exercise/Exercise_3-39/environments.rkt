#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.rkt

(require "data-structures.rkt")

(define init-nameless-env
    (lambda ()
      (extend-nameless-env 
       (num-val 1)			; was i
       (extend-nameless-env
        (num-val 5)			; was v
        (extend-nameless-env
         (num-val 10)			; was x
         (empty-nameless-env)))))
)

(define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'x
            (empty-senv)))))
)

(provide init-nameless-env init-senv)

