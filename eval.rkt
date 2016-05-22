#lang racket

;;expr ::= number | id | (oper expr*)

(provide evaluate)

;; evalue l'expression 'expr' dans l'environement 'env' fourni sous la forme
;; d'une liste de couple (symbole . valeur)
(define (evaluate expr env)
  (let ((ns (make-base-namespace)))
    (map (lambda (x)
           (namespace-set-variable-value! (car x) (cdr x) #t ns))
         env)
    (eval expr ns)))


