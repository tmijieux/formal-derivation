#lang racket

(provide symbol-table)
(provide diff-table)
(provide simplify-table)
(provide new-table)

;; 'let over lambda'
;; construit un table d'association et une méthode pour y accèder et la modifier
(define-syntax-rule (new-table)
  (let ([table  '()])
    (lambda args
      (cond
       [(pair? args) (set! table (cons (car args) table))]
       [else table]))))

(define symbol-table
  (new-table))

(define diff-table
  (new-table))

(define simplify-table
  (new-table))
