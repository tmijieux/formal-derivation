#lang racket

(provide diff)
(provide diff-)
(provide diff-n)
(provide defrule)
(provide defvar)

(require "simplify.rkt")
(require "simplify-rule.rkt")
(require "table.rkt")

;; ajoute un symbole dans la table des symbole
;; pour ajouter une parametre formel 
;; on peut par exemple faire ceci:  (new-symbol-entry x 'x)
(define-syntax-rule (defvar varsymb value)
  (symbol-table (cons 'varsymb value)))

;; construit la lambda
(define (make-lambda-syntax args body)
  #`(lambda #,args
      #,body))

;; construit la liste syntaxique des arguments
;; de la lambda regle de dérivation
(define (diffrule-arglist varlist symb)
  #`(#,symb #,@varlist))

;; construit le corps syntaxique 
;; de la lambda regle de dérivation
(define (diffrule-body replacement symb)
  (letrec ([body-aux
	    (lambda (x)
	      (cond
	       [(list? x)
		(if (equal? 'd (car x))
		    #`,(diff- #,symb #,(cadr x))
		    (cons (car x)
			  (map body-aux (cdr x))))]
	       [(symbol? x) #`,#,x]
	       [else x]))])
    #``#,(cons (car replacement)
	       (map body-aux (cdr replacement)))))

;; renvoit la lambda qui constitue la regle de dérivation
(define (diffrule-lambda args replacement)
  (let ((symb (gensym)))
    (eval-syntax
     (make-lambda-syntax
      (diffrule-arglist args symb)
      (diffrule-body replacement symb)))))

(define-syntax-rule (defrule f args replacement)
  (diff-table (cons 'f (diffrule-lambda #'args 'replacement))))

;; remplace toute les occurences de old-value par new-value dans
;; 'list' puis retourne le résultat (appliqué recursivement sur les sous listes)
(define (replace-value list old-value new-value)
  (let ([replace-value-aux
	 (lambda (x)
	   (cond [(equal? x old-value) new-value]
		 [(pair? x) (replace-value x old-value new-value)]
		 [else x]))])
    (if (null? list)
	'()
	(map replace-value-aux list))))

;; recupere la regle de derivation du la fonction 'f'
;; dans la table des regles
(define (diff-rule f)
  (let ((p (assoc f (diff-table))))
    (if (pair? p)
	(cdr p)
	(raise `("no known rule for function" ,f)))))

(define atom? (or/c number? symbol? boolean? string? null?))

;; renvoit 'expr' avec chacun des symboles différents du symbole
;; par rapport auquel on dérive remplacé par sa valeur
;; dans la table des symboles.
(define (symbol-replace expr respected-symbol)
  (letrec ([symbol-replace-aux
	    (lambda (x)
	      (if (and (symbol? x)
		       (not (equal? x respected-symbol)))
		  (let ([tmp (assoc x (symbol-table))])
		    (if (pair? tmp)
			(cdr tmp)
			x))
		  x))])
    (map symbol-replace-aux expr)))

;; dérivé de 'expr' par rapport a 'respected-symbol'
(define (diff- respected-symbol expr)
  (match expr
	 [(? number?) 0]
	 [(? symbol?)
	  (if (equal? expr respected-symbol)
	      1
	      0)]
	 [(cons app args)
	  (let* ([args (symbol-replace args respected-symbol)]
		 [d-rule (diff-rule app)])
	    (simplify (apply d-rule respected-symbol args)))]))

;; interface pour diff
(define-syntax-rule (diff expr symbol)
  (diff- 'symbol (simplify 'expr)))

(define (diff-n- expr symbol order)
  (if (< order 1)
      expr
      (diff-n- (diff- symbol expr)
	       symbol
	       (sub1 order))))

(define-syntax-rule (diff-n expr symbol order)
  (diff-n- (simplify 'expr) 'symbol order))
