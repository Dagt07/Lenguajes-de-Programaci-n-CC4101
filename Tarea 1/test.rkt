#lang play
(require "T1.rkt")

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

;; tests para fold-eval
(test (fold-eval simple_cf) 1)
(test (fold-eval my_cf) 49/4)
(test (fold-eval my_cf_2) 200/49)
(test (fold-eval my_cf_3) 649/200)

;; tests para fold-degree
(test (fold-degree simple_cf) 0)
(test (fold-degree my_cf) 1)
(test (fold-degree my_cf_2) 2)
(test (fold-degree my_cf_3) 3)

;; ----- Parte f) -----


;; ----- Parte g) -----


;; ----- Parte h) -----