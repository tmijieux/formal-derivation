#!/usr/bin/env racket
#lang racket

(require "simplify.rkt")
(require "simplify-rule.rkt")
(require "flatten.rkt")

(simplify '(-- (-- u)))
(binarize '(- 3))
(binarize '(- 3 2))
(binarize '(-- 3 2))
(binarize '(-- 3))
(binarize '(-- 3 5 4 1))
(binarize '(- 3 5 4 1))
(binarize '(+ 5 4 1))
(binarize '(+ x))
(binarize '(+))
(simplify '(pow x 7 8))
(define A 
  '(+ (-- (- x 2 3)
	  (* 4 5 y)
	  (+ 8 9 0))
      (- (-- z)) 
      (* 2 3 0)))
(binarize A)
(simplify A)

(simplify '(* (/ 3 x) (pow x 4)))
(simplify '(-- (- u v)))
(simplify '(-- (+ 3 v)))
(simplify '(-- (/ (+ 3 4) u)))
(simplify '(-- (/ (* 3 v) v)))

(simplify '(/ u (* u v)))
(simplify '(/ u (* v u)))
(simplify '(arcos x))
