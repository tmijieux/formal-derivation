#lang racket
(provide flatten-expr)
(provide atom?)
(provide binarize)

(define atom? (or/c number? symbol? boolean? string? null?))

(define (opeToFlatten? ope)
  (cond [(eq? ope '+) #t]
	[(eq? ope '-) #t]
	[(eq? ope '*) #t]
	[(eq? ope '/) #t]
	[else #f]))

(define (opeNAsso? ope)
  (cond [(eq? ope '-) #t]
	[(eq? ope '/) #t]
	[else #f]))

(define (flatten-expr-aux l symb)
  (match l 
	 [(? null?) '()]
	 [(? atom?) (cons l '())]
	 [(cons f expr)
	  (cond [(pair? f)
		 (append (flatten-expr-aux f symb)
			 (flatten-expr-aux expr symb))]
		[(number? f)
		 (if (opeNAsso? symb)
		     (cons f  (flatten-expr-aux expr 'void))
		     (cons f  (flatten-expr-aux expr symb)))]
		[(eq? symb f) (flatten-expr-aux expr symb)]
		[(opeToFlatten? f) (cons (flatten-expr l) '())]
		[(null? expr) (list f)]
		[else (cons f (flatten-expr-aux expr symb))])]))

(define (flatten-expr l)
  (match l
	 [(cons f1 expr)
	  (cons f1 (flatten-expr-aux expr f1))]))
;; > (flatten-expr '(+ (+ z a (* (* (/ 8 (* 5 (- 4 (- 99 (- 2 5))y) y) 2) 8)))))
;; '(+ 3 4 (* (/ 8 (* 5 (- 4 (- 99 (- 2 5)) y) y) 2) 8))


(define (associative? x)
  (match x
	 ['+ #t]
	 ['* #t]
	 [default #f]))


(define (2parameters expr)
  (letrec
      ([2parameters-aux
        (lambda (func param)
          (if (not (associative? func))
              (cons func (map 2parameters param))
              (match param
		     [(cons p1 '()) (2parameters (car param))] 
		     [(cons p1 p2)
		      `(,func ,(2parameters (car param))  
			      ,(2parameters-aux func (cdr param)))]
		     [default (cons func param)])))])
    (match expr
	   [(? atom?) expr]
	   [(cons func param) 
	    (2parameters-aux func param)])))



(define (binarize expr)
  (let ([expr (2parameters expr)])
    (match expr
      [(? atom?) expr]
      [default
        (let ([expr (map binarize expr)])
          (match expr
            [`(- ,param)
             `(-- ,param)]
            [`(/ ,param)
             `(/ 1 ,param)]
            [`(-- ,param ...)
             (binarize (append '(-)
                               param))]
            [`(- ,p1 ,param ...)
             `(- ,p1
                 ,(binarize (append '(+) param)))]
            [default expr]))])))




