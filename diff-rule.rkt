#lang racket

;;; liste des règles de dérivation

(require "diff.rkt")

(provide pow)
(define pow expt)

(defrule + (u v)
  (+ (d u) (d v)))

(defrule * (u v)
  (+ (* u (d v)) (* (d u) v)))

(defrule - (u v)
  (- (d u) (d v)))

(defrule -- (u)
  (-- (d u)))

(defrule sqrt (u)
  (/ (d u) (* 2 (sqrt u))))

(defrule sqr (u)
  (* 2 (* u (d u))))

(defrule log (u)
  (/ (d u) u))

(defrule / (u v)
  (/ (- (* (d u) v)
	(* u (d v)))
     (pow v 2)))

(defrule pow (u v)
  (* v (* (d u) (pow u (- v 1)))))

(defrule expt (u v)
  (* (log u) (* (d v) (expt u v))))

(defrule cos (u)
  (* (d u) (- (sin u))))

(defrule sin (u)
  (* (d u) (cos u)))
