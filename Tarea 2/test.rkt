#lang play
(require "t2.rkt")

;;(print-only-errors #t)

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----

;; basic operations
(test (parse-prop 'true) (tt) )
(test (parse-prop 'false) (ff) )
(test (parse-prop '{not true}) (p-not (tt)) )
(test (parse-prop '{not false}) (p-not (ff)) )
(test (parse-prop '{not {not true}}) (p-not (p-not (tt))) )


;; and & or
(test (parse-prop '{and true false}) (p-and (list (tt) (ff))) )
(test (parse-prop '{and true false true}) (p-and (list (tt) (ff) (tt))) )
(test (parse-prop '{and false {not true}}) (p-and (list (ff) (p-not (tt)))) )
(test (parse-prop '{and {and false true false} true false}) (p-and (list (p-and (list (ff) (tt) (ff))) (tt) (ff))) )

(test (parse-prop '{or true false}) (p-or (list (tt) (ff))) )
(test (parse-prop '{or true false true}) (p-or (list (tt) (ff) (tt))) )
(test (parse-prop '{or false {not false}}) (p-or (list (ff) (p-not (ff)))) )
(test (parse-prop '{or {or false true false} true false}) (p-or (list (p-or (list (ff) (tt) (ff))) (tt) (ff))) )

(test/exn (parse-prop '{and}) "and expects at least two operands")
(test/exn (parse-prop '{or}) "or expects at least two operands")
(test/exn (parse-prop '{and true}) "and expects at least two operands")
(test/exn (parse-prop '{or false}) "or expects at least two operands")
(test/exn (parse-prop '{and {and true} false}) "and expects at least two operands" )
(test/exn (parse-prop '{or {or false} true}) "or expects at least two operands" )
(test/exn (parse-prop '{and {or true} true true}) "or expects at least two operands" )
(test/exn (parse-prop '{or {and false} true false}) "and expects at least two operands" )

;; id & where
(test (parse-prop 'x) (p-id 'x))
;;(test (parse-prop '{false where {x true}}) (p-where (ff) 'x (tt)) )
;;(test (parse-prop '{x where {x true}}) (p-where (p-id 'x) 'x (tt)) )

;; ----- Parte c) -----

(test (from-Pvalue (ttV)) (tt) )
(test (from-Pvalue (ffV)) (ff) )

;; ----- Parte d) -----


;; ----- Parte e) -----

(test (p-eval (tt)) (ttV)) 
(test (p-eval (ff)) (ffV)) 
(test (p-eval (p-not (tt))) (ffV) )
(test (p-eval (p-not (ff))) (ttV) )

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----


;; ----- Parte c) -----


;; ----- Parte d) -----


;; ----- Parte e) -----