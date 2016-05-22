#lang racket

(provide diff-num)

;; calcule le nombre dérivé de la fonction 'f' en 'x' de manière numérique
;; avec le taux d'accroissement entre x et x+(1/n)
(define (diff-num f x n)
  (* n (- (f (+ x (/ 1 n)))
	  (f x))))
