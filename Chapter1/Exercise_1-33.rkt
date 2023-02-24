#lang eopl

(require "Exercise_1-31.rkt")

(define M
	(lambda (bintree red-depth)
		(if (leaf? bintree)
			(leaf red-depth)
			(let* (
					[sym (contents-of bintree)]
					[new_depth (if (eqv? sym 'red) (+ 1 red-depth) red-depth)])
				(interior-node 
					sym
					(M (lson bintree) new_depth)
					(M (rson bintree) new_depth)))))
)

(define mark-leaves-with-red-depth
	(lambda (bintree)
		(M bintree 0))
)

; test
(define tree (interior-node 'red
				(interior-node 'bar
					(leaf 26)
					(leaf 12))
				(interior-node 'red
					(leaf 11)
					(interior-node 'quux
						(leaf 117)
						(leaf 14)))))

(display (mark-leaves-with-red-depth tree))