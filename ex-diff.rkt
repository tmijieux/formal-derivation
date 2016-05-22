#!/usr/bin/env racket
#lang racket

;;; Test et exemple de dérivation.

(require "diff.rkt")
(require "diff-rule.rkt")

(display "f:u --> 1 + log(sqr(u))\n")
(display "df/du:\n")
(define u 'u)
(diff (+ 1 (log (sqr u))) u)
(diff (+ 1 (log (* 4 (* u u)))) u)

(defvar v 5)
(diff (* u v) u)
(diff (* u v) v)

(defvar v 3)
(diff (* u v) u)
(diff (* u v) v)

(defvar v 'v)
(diff (* u v) u)
(diff (* u v) v)

(diff (pow x w) x)

(defvar w 4)
(diff (pow x w) x)

(diff (/ (* 3 x)
	 (pow x 4)) x)

(diff-n (* u u) u 1)
(diff-n (* u u) u 2)
(diff-n (* u u) u 3)

(diff (log (+ (expt x 2) 4)) x)

(diff (pow x 3) x)
