#lang racket

;;; module réalisant les simplifications;

(require "table.rkt")
(require "eval.rkt")
(require "flatten.rkt")

(provide defsimrule)
(provide defsimclass)
(provide simplify)

;; ajout d'une classe de simplification pour la fonction 'f'
(define-syntax-rule (defsimclass f)
  (simplify-table (cons 'f (new-table))))

;; construit l'objet syntaxique d'une lambda a partir
;; de l'objet syntaxique 'args de la liste des arguments
;; et de 'body le corps de la fonction.
(define (make-lambda-syntax args body)
  #`(lambda #,args
      #,body))

(define (lambda-arglist symb)
  #`(#,symb))

;; construit le corps de la lambda qui test si on peut
;; appliquer une simplification
(define (lambda-body-test mtch bool symb)
  #`(match #,symb
	   [`#,mtch #,bool]
	   [default #f]))

;; construit le corps de la lambda qui applique une simplification
(define (lambda-body-applic mtch res symb)
  #`(match #,symb
	   [`#,mtch `#,res]
	   [default #,symb]))

;; construit les deux lambda qui constitue une règle de simplification
(define (create-simplify-functions mtch bool res)
  (let* ((symb (gensym))
	 (argl (lambda-arglist symb)))
    (let ([test (make-lambda-syntax argl
				    (lambda-body-test mtch bool symb))]
          [applic (make-lambda-syntax argl
				      (lambda-body-applic mtch res symb))])
      (cons (eval-syntax test)
            (eval-syntax applic)))))

(define (create-simplify-rule cons-test-applic)
  (lambda (expr)
    (let ([test (car cons-test-applic)]
	  [applic (cdr cons-test-applic)])
      (if (test expr)
	  (applic expr)
	  #f))))

(define (add-new-rule symb-list entry)
  (if (null? symb-list)
      (void)
      (let* ([s-table (simplify-table)]
             [current-symb (car symb-list)]
             [symb-class-table (assoc current-symb s-table)])
	(if (pair? symb-class-table)
	    ((cdr symb-class-table) entry)
	    (error "could not find the simplify class of" current-symb))
	(add-new-rule (cdr symb-list) entry))))

(define (defsimrule-f symlist mtch bool res)
  (add-new-rule symlist
		(create-simplify-rule
		 (create-simplify-functions mtch
					    bool
					    res))))

(define-syntax-rule (defsimrule symlist mtch bool res)
  (defsimrule-f 'symlist #'mtch #'bool #'res))

(define (get-simplify-table symb)
  (let ([p (assoc symb (simplify-table))])
    (if (pair? p)
	((cdr p))
	(error "could not find the simplify class" symb))))

(define (simplify-class class expr)
  (let ([rule-table (get-simplify-table class)])
    (letrec ([simplify-class-aux
	      (lambda (rule-table expr-aux)
		(match rule-table
		       [(? null?) expr-aux]
		       [(cons x y)
			(let ([new-expr (x expr-aux)])
			  (if (equal? new-expr #f)
			      (simplify-class-aux y expr-aux)
			      (simplify new-expr)))]))])
      (simplify-class-aux rule-table expr))))

;; simplifie l'expression 'expr' bien formée
(define (simplify-aux expr)
  (match expr
	 [(? atom?) expr]
	 [(cons x y)
	  (let ([simp-expr (map simplify-aux expr)])
	    (simplify-class x simp-expr))]
	 [default expr]))

;; simplifie l'expression 'expr' en changeant la forme de 'expr'
(define (simplify expr)
  (simplify-aux (binarize expr)))
