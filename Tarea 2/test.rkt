#lang play
(require "t2.rkt")

;;(print-only-errors #t)

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----

(test (parse-prop 'true) (tt) )
(test (parse-prop 'false) (ff) )
(test (parse-prop '{not true}) (p-not (tt)) )
(test (parse-prop '{not false}) (p-not (ff)) )


;; ----- Parte c) -----


;; ----- Parte d) -----


;; ----- Parte e) -----

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----


;; ----- Parte c) -----


;; ----- Parte d) -----


;; ----- Parte e) -----