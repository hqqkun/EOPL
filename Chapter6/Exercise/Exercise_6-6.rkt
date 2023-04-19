#lang eopl

; 6

; g, f, j, h
; g, j, f, h
; g, j, h, f

; j, h, g, f
; j, g, h, f
; j, g, f, h

; for j, g, f, h
(lambda (x y cont)
  (j y
    (lambda (val1)
      (g x 
        (lambda (val2)
          (f val2
            (lambda (val3)
              (h val1
                (lambda (val4)
                  (cont (+ val3 val4))))))))))
)

; skip the rest.