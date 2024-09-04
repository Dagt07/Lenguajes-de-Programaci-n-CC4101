#lang play
(require "T1.rkt")
(require math/flonum)

(print-only-errors #t)

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----

(test (eval simple_cf) 1)
(test (eval my_cf) 49/4)
(test (eval my_cf_2) 200/49)
(test (eval my_cf_3) 649/200)

;; ----- Parte c) -----

(test (degree simple_cf) 0)
(test (degree my_cf) 1)
(test (degree my_cf_2) 2)
(test (degree my_cf_3) 3)

;; ----- Parte d) -----

;; acá no se testea como tal ya que se definio solamente fold-cfraction,
;; es decir, se debe testear las funciones que se hagan a partir de este fold

;; ----- Parte e) -----
;; Observación: recordar que las fold-function usan los mismos tests que la function original o que quieren abstraer

;; tests para eval2
(test (eval2 simple_cf) 1)
(test (eval2 my_cf) 49/4)
(test (eval2 my_cf_2) 200/49)
(test (eval2 my_cf_3) 649/200)

;; tests para degree2
(test (degree2 simple_cf) 0)
(test (degree2 my_cf) 1)
(test (degree2 my_cf_2) 2)
(test (degree2 my_cf_3) 3)

;; ----- Parte f) -----

(define simple_c0 (simple 6) )
(define mysterious_c1 (compound 6 (sqr 1) (simple 6)) )
(define mysterious_c2 (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))) )
(define mysterious_c3 (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (simple 6)))) )
(define mysterious_c4 (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (compound 6 (sqr 7) (simple 6))))) )

(test (mysterious-cf 0) simple_c0)
(test (mysterious-cf 1) mysterious_c1)
(test (mysterious-cf 2) mysterious_c2)
(test (mysterious-cf 3) mysterious_c3)
(test (mysterious-cf 4) mysterious_c4)
(test/exn (mysterious-cf -1) "Error: argumento negativo")

;; Se pregunto al cuerpo docente si se podía hacer una función auxiliar -> Si
;; Se pregunto si era necesario testear esa función auxiliar -> No

;; ----- Parte g) -----

;; Tests para from-to
(test (from-to 0 3) '(0 1 2))
(test (from-to 2 5) '(2 3 4))
(test (from-to 0 0) '())
(test (from-to 5 5) '())

;; Tests para mysterious-list
(define l1 (list (fl (- (eval (mysterious-cf 0)) 3)) ) )
(define l2 (list (fl (- (eval (mysterious-cf 0)) 3)) (fl (- (eval (mysterious-cf 1)) 3)) ) )
(define l3 (list (fl (- (eval (mysterious-cf 0)) 3)) (fl (- (eval (mysterious-cf 1)) 3)) (fl (- (eval (mysterious-cf 2)) 3)) ) )
(define l4 (list (fl (- (eval (mysterious-cf 0)) 3)) (fl (- (eval (mysterious-cf 1)) 3)) (fl (- (eval (mysterious-cf 2)) 3)) (fl (- (eval (mysterious-cf 3)) 3)) ) )

(test (mysterious-list 0) '())
(test (mysterious-list 1) l1)
(test (mysterious-list 2) l2)
(test (mysterious-list 3) l3)
(test (mysterious-list 4) l4)
;; ----- Parte h) -----