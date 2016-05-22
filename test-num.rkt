#!/usr/bin/env racket
#lang racket

(require "diff.rkt")
(require "diff-num.rkt")
(require "eval.rkt")
(require "diff-rule.rkt")

;; construit une lambda avec un unique parametre
(define (make-lambda symbol expr)
  (eval-syntax #`(lambda (#,symbol)
		   #,expr)))

;; comparaison entre la dérivée formelle et la dérivée numérique
(define (test-diff expr symbol env rand)
  (values (evaluate (diff- symbol expr) env)
	  (diff-num (make-lambda symbol expr)
		    rand 1000)))

(define EPS 0.01)

;; réalise un test sur l'expression '(/ (* 3 x) (pow x 4)) 
(define (test-div)
  (let ([rand (+ (random) (random 20))])
    (test-diff '(/ (* 3 x) (pow x 4)) 'x
	       `((x . ,rand)
		 (pow . ,expt)) rand)))

;; réalise un test sur l'expression '(+ 1 (log (sqr u)))
(define (test-log-sqr)
  (let ([rand (+ (random) (random 20))])
    (test-diff '(+ 1 (log (sqr u))) 'u
	       `((u . ,rand)
		 (pow . ,expt)) rand)))

;; réalise un test sur l'expression '(cos (* 10 u))
(define (test-cos10)
  (let ([rand (+ (random) (random 20))])
    (test-diff '(cos (* 10 u)) 'u
	       `((u . ,rand)
		 (pow . ,expt)
                 (-- . ,-)) rand)))

(define (test-cos0.2)
  (let ([rand (+ (random) (random 20))])
    (test-diff '(cos (* 0.2 u)) 'u
	       `((u . ,rand)
		 (pow . ,expt)
                 (-- . ,-)) rand)))

;; lance une séquence de test
(define (test-diff-n n generator)
  (let ((l (lambda (a b)
	     (let ((c (< (abs (/ (- a b) a)) EPS))) ; c = difference relative
	     (displayln `(,a ,b ,c))))))
    (if (> n 0)
	(begin
	  (call-with-values generator l)
	  (test-diff-n (sub1 n) generator))
	(displayln ""))))

(display "EPS:") (displayln EPS)

(test-diff-n 20 test-log-sqr)
(test-diff-n 20 test-div)
(test-diff-n 20 test-cos10)
(test-diff-n 20 test-cos0.2)
