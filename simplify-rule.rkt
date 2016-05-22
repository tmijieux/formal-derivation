#lang racket

(require "simplify.rkt")
(require "eval.rkt")

(defsimclass *)
(defsimclass +)
(defsimclass /)
(defsimclass -) ;; binaire
(defsimclass --) ;; unaire
(defsimclass cos)
(defsimclass sqr)
(defsimclass sqrt)
(defsimclass sin)
(defsimclass exp)
(defsimclass log)
(defsimclass expt)
(defsimclass pow)
(defsimclass abs)

;; s = variable
;; n = nombre

;; ---------------- ;;
;; + * - / expt
;; ----------------;;

;; Evalue l'expression '(func x y) si x et y sont des nombres
(defsimrule (+ * - / expt)
  (,f ,x ,y)
  (and (number? x)
       (number? y))
  ,(evaluate `(,f ,x ,y) '()))

;; ---------------- ;;
;; cos sin sqr sqrt exp log abs
;; ----------------;;

;; Evalue l'expression '(f x) si x est un nombre
(defsimrule (cos sin sqr sqrt exp log abs)
  (,f ,x)
  (number? x)
  ,(evaluate `(,f ,x) '()))

;; ---------------- ;;
;; + *
;; ----------------;;

;; (* 1 x) = x et (+ 0 x) = x
(defsimrule (+ *)
  (,f ,x ,y)
  (equal? (evaluate `(,f) '()) x)
  ,y)

;; (f n1 (f n2 s)) = (f (f n1 n2) s)
(defsimrule (+ *)
  (,f ,x (,f ,y ,z))
  (and (number? x) 
       (number? y))
  (,f (,f ,x ,y) ,z))

;;(f n1 (f n2 s)) = (f (f n1 n2) s)
(defsimrule (+ *)
  (,f ,x (,f ,y ,z))
  (and (number? x)
       (number? y))
  (,f (,f ,x ,y) ,z))

;;(f s1 (f n s2)) = (f n (f s1 s2))
(defsimrule (+ *)
  (,f ,x (,f ,y ,z))
  (and (not (number? x))
       (number? y))
  (,f ,y (,f ,x ,z)))

;;(f expr n/s) = (f n/s expr) 
(defsimrule (+ *)
  (,f ,x ,y) 
  (and (not (number? x))
       (or (number? y)
	   (symbol? y))
       (not (and (symbol? x)
		 (symbol? y))))
  (,f ,y ,x))

;; ---------------- ;;
;; +
;; ----------------;;

;; (+ n1 (- s n2)) = (+ (- n1 n2) s)
(defsimrule (+)
  (+ ,x (- ,y ,z))
  (and (number? x) 
       (number? z))
  (+ (- ,x ,z) ,y))

;; (+ n1 (- n2 s)) = (- (+ n1 n2) s)
(defsimrule (+)
  (+ ,x (- ,y ,z))
  (and (number? x) 
       (number? y))
  (- (+ ,x ,y) ,z))

;;(+ s1 (- n s2)) = (+ n (- s1 s2))
(defsimrule (+)
  (+ ,x (- ,y ,z))
  (number? y)
  (+ ,y (- ,x ,z)))

;;(+ s1 (- s2 n)) = (- (+ s1 s2) n)
(defsimrule (+)
  (+ ,x (- ,y ,z))
  (number? z)
  (- (+ ,x ,y) ,z))

;;(+ (+ n1 s) n2) = (+ (+ n1 n2) s)
(defsimrule (+)
  (+ (+ ,x ,y) ,z) 
  (and (number? x)
       (number? z))
  (+ (+ ,x ,z) ,y))

;;(+ (* n1 x) (* n2 x)) = (* (+ n1 n2) x)
(defsimrule (+)
  (+ (* ,x ,y) (* ,z ,t)) 
  (and (number? x)
       (number? z)
       (equal? y t))
  (* (+ ,x ,z) ,y))

;;(+ (- n1 s) n2) = (- (+ n1 n2) s)
(defsimrule (+)
  (+ (- ,x ,y) ,z) 
  (and (number? x)
       (number? z))
  (- (+ ,x ,z) ,y))

;; (+ (pow (cos x) 2) (pow (sin x) 2)) = 1
(defsimrule (+)
  (+ (pow (,f ,x) 2) (pow (,g ,x) 2))
  (or (and (equal? f 'cos)
	   (equal? g 'sin))
      (and (equal? f 'sin)
	   (equal? g 'cos)))
  1)

;; (+ x x) = (* 2 x)
(defsimrule (+)
  (+ ,x ,x)
  #t
  (* 2 ,x))

;; (+ (* 5 x) x) = (* 6 x)
(defsimrule (+)
  (+ (* ,n ,x) ,x)
  #t
  (* (+ ,n 1) ,x))

;; (+ x (* 5 x)) = (* 6 x)
(defsimrule (+)
  (+ ,x (* ,n ,x))
  #t
  (* (+ ,n 1) ,x))

;; ---------------- ;;
;; *
;; ----------------;;

;; (* s/n 0) = 0 et (* 0 s/n) = 0
(defsimrule (*)
  (* ,x ,y)
  (or (equal? 0 x) 
      (equal? 0 y))
  0)

;; (* x x) = (pow x 2)
(defsimrule (*)
  (* ,x ,x)
  #t 
  (pow ,x 2))

;; (* (cos x) (sin x)) = (/ (sin (* 2 x)) 2)
(defsimrule (*)
  (* (,c ,x) (,s ,x))
  (or (and (equal? c 'cos)
	   (equal? s 'sin))
      (and (equal? c 'sin)
	   (equal? s 'cos)))
  (/ (sin (* 2 ,x)) 2))

;; (* y (pow x n)) = (pow x (+ n 1)) when x = y
(defsimrule (*)
  (* ,x (pow ,x ,n))
  #t
  (pow ,x (+ ,n 1)))

;; (* (pow x n) x) = (pow x (+ n 1))
(defsimrule (*)
  (* (pow ,x ,n) ,x)
  #t
  (pow ,x (+ ,n 1)))

;; (* (pow x y) (pow x z)) = (pow x (+ y z))
(defsimrule (*)
  (* (pow ,x ,y) (pow ,x ,z))
  #t
  (pow ,x (+ ,y ,z)))

;; (* (pow x n) (* x y)) = (* y (pow x (+ 1 n)))
(defsimrule (*)
  (* (pow ,x ,n) (* ,x ,y))
  #t
  (* ,y (pow ,x (+ 1 ,n))))

;; (* (* x y) (pow x n)) = (* y (pow x (+ 1 n)))
(defsimrule (*)
  (* (* ,x ,y) (pow ,x ,n))
  #t
  (* ,y (pow ,x (+ 1 ,n))))

;; (* (* y x ) (pow x n)) = (* y (pow x (+ 1 n)))
(defsimrule (*)
  (* (* ,y ,x) (pow ,x ,n))
  #t
  (* ,y (pow ,x (+ 1 ,n))))

;; (* (pow x n) (* y x)) = (* y (pow x (+ 1 n)))
(defsimrule (*)
  (* (pow ,x ,n) (* ,y ,x))
  #t
  (* ,y (pow ,x (+ 1 ,n))))

;; (* (pow x n1) (* y (pow x n2))) = (* y (pow x (+ n1 n2)))
(defsimrule (*)
  (* (pow ,x ,n1) (* ,y (pow ,x ,n2)))
  #t
  (* ,y (pow ,x (+ ,n1 ,n2))))

;; (* (pow x n1) (* (pow x n2) y))) = (* y (pow x (+ n1 n2)))
(defsimrule (*)
  (* (pow ,x ,n1) (* (pow ,x ,n2) ,y))
  #t
  (* ,y (pow ,x (+ ,n1 ,n2))))

;; (* (* y (pow x n2)) (pow x n1)) = (* y (pow x (+ n1 n2)))
(defsimrule (*)
  (* (* ,y (pow ,x ,n2)) (pow ,x ,n1))
  #t
  (* ,y (pow ,x (+ ,n1 ,n2))))

;; (* (* (pow x n2) y) (pow x n1)) = (* y (pow x (+ n1 n2)))
(defsimrule (*)
  (* (* (pow ,x ,n2) ,y) (pow ,x ,n1))
  #t
  (* ,y (pow ,x (+ ,n1 ,n2))))

;; (* x (pow y -1)) = (/ x y)
(defsimrule (*)
  (* ,x (pow ,y -1))
  #t
  (/ ,x ,y))

;; (* n1 (/ n2 x)) = (/ (* n1 n2) x)
(defsimrule (*)
  (* ,n1 (/ ,n2 ,x))
  (and (number? n1)
       (number? n2)
       (not (number? x)))
  (/ (* ,n1 ,n2) ,x))

;; (* n1 (/ x n2)) = (* (/ n1 n2) x)
(defsimrule (*)
  (* ,n1 (/ ,x ,n2))
  (and (number? n1)
       (number? n2)
       (not (number? x)))
  (* (/ ,n1 ,n2) ,x))

;; ---------------- ;;
;; /
;; ----------------;;

;; (/ x 1) = x
(defsimrule (/)
  (/ ,x 1)
  #t
  ,x)

;; (/ n1 (* n2 s)) = (/ (/ n1 n2) s)
(defsimrule (/)
  (/ ,x (* ,y ,z))
  (and (number? x) 
       (number? y))
  (/ (/ ,x ,y) ,z))

;; (/ n1 (* s n2)) = (/ (/ n1 n2) s)
(defsimrule (/)
  (/ ,x (* ,y ,z))
  (and (number? x) 
       (number? z))
  (/ (/ ,x ,z)  ,y))

;; (/ n1 (/ s n2)) = (/ (* n1 n2) s)
(defsimrule (/)
  (/ ,x (/ ,y ,z))
  (and (number? x) 
       (number? z))
  (/ (* ,x ,z) ,y))

;; (/ n1 (/ n2 s)) = (* (/ n1 n2) s)
(defsimrule (/)
  (/ ,x (/ ,y ,z))
  (and (number? x) 
       (number? y))
  (* (/ ,x ,y)  ,z))

;; (/ (/ x y) z) = (/ x (* y z))
(defsimrule (/)
  (/ (/ ,x ,y) ,z)
  #t
  (/ ,x (* ,y ,z)))

;; (/ (* x y) (* x z)) = (/ y z)
(defsimrule (/)
  (/ (* ,x ,y) (* ,x ,z))
  #t
  (/ ,y ,z))

;; (/ (* x y) (* z x)) = (/ y z)
(defsimrule (/)
  (/ (* ,x ,y) (* ,z ,x))
  #t
  (/ ,y ,z))

;; (/ (* y x) (* z x)) = (/ y z)
(defsimrule (/)
  (/ (* ,y ,x) (* ,z ,x))
  #t
  (/ ,y ,z))

;; (/ (* y x) (* x z)) = (/ y z)
(defsimrule (/)
  (/ (* ,y ,x) (* ,x ,z))
  #t
  (/ ,y ,z))

;; (/ (* x y) x) = y
(defsimrule (/)
  (/ (* ,x ,y) ,x)
  #t
  ,y)

;; (/ (* x y) y) = x
(defsimrule (/)
  (/ (* ,x ,y) ,y)
  #t
  ,x)

;; (/ x (* x y)) = (/ 1 y)
(defsimrule (/)
  (/ ,x (* ,x ,y))
  #t
  (/ 1 ,y))

;; (/ x (* y x)) = (/ 1 y)
(defsimrule (/)
  (/ ,x (* ,y ,x))
  #t
  (/ 1 ,y))

;; (/ x (pow y n)) = (* x (pow y (- n)))
(defsimrule (/)
  (/ ,x (pow ,y ,n))
  (number? n)
  (* ,x (pow ,y ,(evaluate `(- ,n) '()))))

;; (/ (* n x) (* m y)) = (* (/ n m) (/ x y))
(defsimrule (/)
  (/ (* ,n ,x) (* ,m ,y))
  (and (number? n)
       (number? m))
  (* (/ ,n ,m) (/ ,x ,y)))




;; ---------------- ;;
;; -
;; ----------------;;

;; (- x 0) = x
(defsimrule (-)
  (- ,x 0)
  #t
  ,x)

;; (- 0 x) = (-- x)
(defsimrule (-)
  (- 0 ,x)
  #t
  (-- ,x))

;; (- x) = (-- x)
(defsimrule (-)
  (- ,x)
  #t
  (-- ,x))

;;(- x x) = 0
(defsimrule (-)
  (- ,x ,x)
  #t
  0)

;; (- n1 (- s n2)) = (- (+ n1 n2) s)
(defsimrule (-)
  (- ,x (- ,y ,z))
  (and (number? x) 
       (number? z)
       (not (number? y)))
  (- (+ ,x ,z) ,y))

;;(- n1 (- n2 s)) = (+ (- n1 n2) s) 
(defsimrule (-)
  (- ,x (- ,y ,z))
  (and (number? x) 
       (number? y)
       (not (number? z)))
  (+ (- ,x ,y) ,z))

;;(- s1 (+ n s2)) = (- (- s1 s2) n)
(defsimrule (-)
  (- ,x (+ ,y ,z)) 
  (and (number? y)
       (not (number? x))
       (not (number? z)))
  (- (- ,x ,z) ,y))

;;(- s1 (+ s2 n)) = (- (- s1 s2) n))
(defsimrule (-)
  (- ,x (+ ,y ,z)) 
  (and (number? z)
       (not (number? x))
       (not (number? y)))
  (- (- ,x ,y) ,z))

;;(- s1 (- n s2)) = (- (+ s1 s2) n)
(defsimrule (-)
  (- ,x (- ,y ,z)) 
  (and (number? y)
       (not (number? x))
       (not (number? z)))
  (- (+ ,x ,z) ,y))

;;(- s1 (- s2 n)) = (+ (- s1 s2) n))
(defsimrule (-)
  (- ,x (- ,y ,z)) 
  (and (number? z)
       (not (number? x))
       (not (number? y)))
  (+ (- ,x ,y) ,z))

;;(- (+ n1 s1) n2) = (+ (- n1 n2) s1))
(defsimrule (-)
  (- (+ ,x ,y) ,z) 
  (and (number? x)
       (number? z)
       (not (number? y)))
  (+ (- ,x ,z) ,y))

;;(- (+ s1 n1) n2) = (+ (- n1 n2) s1))
(defsimrule (-)
  (- (+ ,x ,y) ,z) 
  (and (number? y)
       (number? z)
       (not (number? x)))
  (+ (- ,y ,z) ,x))

;;(- (- n1 s) n2) = (- (- n1 n2) s)
(defsimrule (-)
  (- (- ,x ,y) ,z) 
  (and (number? x)
       (number? z)
       (not (number? y)))
  (- (- ,x ,z) ,y))

;;(- (- s n1) n2) = (- s (+ n1 n2))
(defsimrule (-)
  (- (- ,x ,y) ,z) 
  (and (number? y)
       (number? z)
       (not (number? x)))
  (- ,x (+ ,y ,z)))

;; (- (pow (cos x) 2) (pow (sin x) 2)) = (cos (* 2 x))
(defsimrule (-)
  (- (pow (cos ,x) 2) (pow (sin ,x) 2))
  #t
  (cos (* 2 ,x)))

;; (- (* n x) (* m x)) = (* (- n m) x) (factorisation)
(defsimrule (-)
  (- (* ,n ,x) (* ,m ,x))
  (and (number? n)
       (number? m))
  (* (- ,n ,m) ,x))



;; ---------------- ;;
;; log
;; ----------------;;

;; (log (exp x)) = x
(defsimrule (log)
  (log (exp ,x))
  #t
  ,x)
;; ---------------- ;;
;; exp
;; ----------------;;

;; (exp (log x)) = x
(defsimrule (exp)
  (exp (log ,x))
  #t
  ,x)

;; ---------------- ;;
;; pow
;; ----------------;;

;;Evalue l'expression '(pow x y) si x et y sont des nombres
(defsimrule (pow)
  (pow ,x ,y)
  (and (number? x)
       (number? y))
  ,(evaluate `(expt ,x ,y) '()))

;; (pow (abs x) n) = (pow x n)
(defsimrule (pow)
  (pow (abs ,x) ,n)
  (and (number? n) 
       (and (integer? n)
            (even? n)))
  (pow ,x ,n))

;; (pow n x) = (expt n x)
(defsimrule (pow)
  (pow ,x ,y) 
  (and (number? x)
       (not (number? y)))
  (expt ,x ,y))

;; (pow (pow x y) z) = (pow x (* y z)) (y is not even or x is negative)
(defsimrule (pow)
  (pow (pow ,x ,y) ,z)
  (or (not (and (integer? y)
		(even? y)) )
      (and (number? x)
	   (> x 0)))
  (pow ,x (* ,y ,z)))

;; (pow (pow x y) z) = (pow (abs x) (* y z)) (y is even and x is negative)
(defsimrule (pow)
  (pow (pow ,x ,y) ,z)
  (or (and (integer? y)
	   (even? y)))
  (pow (abs ,x) (* ,y ,z)))

;; (pow x 1) = x
(defsimrule (pow)
  (pow ,x 1)
  #t
  ,x)

;; ---------------- ;;
;; abs
;; ----------------;;

;; (abs (pow x n)) = (pow x n) (n is even)
(defsimrule (abs)
  (abs (pow ,x ,n))
  (and (number? n) 
       (even? n))
  (pow ,x ,n))

;; ---------------- ;;
;; expt
;; ----------------;;

;; (expt x n) = (pow x n)
(defsimrule (expt)
  (expt ,x ,y)
  (and (number? y)
       (not (number? x)))
  (pow ,x ,y))

;; ---------------- ;;
;; sqr
;; ----------------;;

;; (sqr x) = (pow x 2)
(defsimrule (sqr)
  (sqr ,x)
  #t
  (pow ,x 2))

;; ---------------- ;;
;; sqrt
;; ----------------;;

;; (srqt x) = (pow x (/ 1 2))
(defsimrule (sqrt)
  (sqrt ,x)
  #t
  (pow ,x ,(/ 1 2)))


;; ---------------- ;;
;; --
;; ----------------;;

;; (-- n) = (- n) quand n est un nombre
(defsimrule (--)
  (-- ,x)
  (number? x)
  ,(evaluate `(- ,x) '()))

;; (-- (-- x)) = x
(defsimrule (--)
  (-- (-- ,x))
  #t
  ,x)

;; (-- (* n x)) = (* (-- n) x)
(defsimrule (--)
  (-- (* ,n ,x))
  #t
  (* (-- ,n) ,x))

;; (-- (/ n x)) = (/ (-- n) x)
(defsimrule (--)
  (-- (/ ,n ,x))
  (number? n)
  (/ (-- ,n) ,x))

;; (-- (/ x n)) = (/ x (-- n))
(defsimrule (--)
  (-- (/ ,x ,n))
  (number? n)
  (/ ,x (-- ,n)))

;; (-- (+ n x)) = (- (-- n) x)
(defsimrule (--)
  (-- (+ ,n ,x))
  (number? n)
  (- (-- ,n) ,x))

;; (-- (- x y)) = (- y x) 
(defsimrule (--)
  (-- (- ,x ,y))
  #t
  (- ,y ,x))

;; (sin (+ n x)) = (sin (+ (modulo n (* 2 pi)) x))) (faire modulo pour les flottants)

;; ------------------------------------------------------------------

