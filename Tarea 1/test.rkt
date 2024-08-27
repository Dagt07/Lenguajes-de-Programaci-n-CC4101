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


;; ----- Parte e) -----


;; ----- Parte f) -----


;; ----- Parte g) -----


;; ----- Parte h) -----