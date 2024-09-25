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
(test (parse-prop '{false where [x true]}) (p-where (ff) 'x (tt)) )
(test (parse-prop '{x where [x true]}) (p-where (p-id 'x) 'x (tt)) )
(test (parse-prop '{{z where [y true]} where [z {not false}]}) (p-where (p-where (p-id 'z) 'y (tt)) 'z (p-not (ff))) )

;; all together
(test (parse-prop '{and true true {x where [x true]}}) (p-and (list (tt) (tt) (p-where (p-id 'x) 'x (tt)))) )
(test (parse-prop '{or false {not false} {x where [x true]}}) 
        (p-or (list (ff) (p-not (ff)) (p-where (p-id 'x) 'x (tt)))) )
(test (parse-prop '{or {and false true false} true {x where [x true]}}) 
        (p-or (list (p-and (list (ff) (tt) (ff))) (tt) (p-where (p-id 'x) 'x (tt)))) )
(test (parse-prop '{{and x true {not {and false {y where [y true]}}}} where [x {or true false}]})
        (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (ff) (p-where (p-id 'y) 'y (tt))))))) 'x (p-or (list (tt) (ff)))))

;; ----- Parte c) -----

(test (from-Pvalue (ttV)) (tt) )
(test (from-Pvalue (ffV)) (ff) )

;; ----- Parte d) -----

(test (p-subst (p-id 'x) 'x (tt)) (tt) )
(test (p-subst (p-id 'x) 'x (p-not (tt))) (p-not (tt)) )
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x) )
(test (p-subst (p-id 'x) 'y (p-not (tt))) (p-id 'x) )
(test (p-subst (p-id 'x) 'x (p-and (list (tt) (ff)))) (p-and (list (tt) (ff))) )
(test (p-subst (p-id 'x) 'x (p-or (list (tt) (ff)))) (p-or (list (tt) (ff))) )

(test (p-subst (p-and (list (p-id 'x) (tt))) 'x (tt)) (p-and (list (tt) (tt))) )
(test (p-subst (p-or (list (p-id 'x) (tt))) 'x (tt)) (p-or (list (tt) (tt))) )

(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)) ) ;; inner x is not a free ocurrence of x, so its not replaced
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)) ) ;; free ocurrence of x, then its replaced
(test (p-subst (parse-prop '{z where [y true]}) 'z (ff)) (parse-prop '{false where [y true]}) )

;; ----- Parte e) -----

;; testing directly from a well constructed AST
(test (p-eval (tt)) (ttV)) 
(test (p-eval (ff)) (ffV)) 
(test (p-eval (p-not (tt))) (ffV) )
(test (p-eval (p-not (ff))) (ttV) )
(test (p-eval (p-not (p-not (tt)))) (ttV) )

;; testing from concrete syntax (using the parser)
(test (p-eval (parse-prop 'true)) (ttV) )
(test (p-eval (parse-prop 'false)) (ffV) )
(test (p-eval (parse-prop '{not true})) (ffV) )
(test (p-eval (parse-prop '{not false})) (ttV) )
(test (p-eval (parse-prop '{not {not true}})) (ttV) )

;; for this point on, we will use the parser to make writing tests easier and double checking the parser.

(test (p-eval (parse-prop '{x where [x true]}))  (ttV) )
(test (p-eval (parse-prop '{{z where [y true]} where [z false]})) (ffV) )
(test (p-eval (parse-prop '{{z where [y true]} where [z {not false}]})) (ttV) )

(test/exn (p-eval (parse-prop 'x)) "Open expression (free occurrence of p-id x)" )
(test/exn (p-eval (parse-prop '{x where [y true]})) "Open expression (free occurrence of p-id x)" )
(test/exn (p-eval (parse-prop '{{z where [y true]} where [z {not y}]}) ) "Open expression (free occurrence of p-id y)" ) ;; for '(not y) y is undefined 

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;; ----- Parte a) -----

;; deftype no lleva test (visto en clases)

;; ----- Parte b) -----

;; basic types
(test (parse '1) (real 1))
(test (parse '{1 i}) (imaginary 1))

;; + & -
(test (parse '{+ 1 2}) (add (real 1) (real 2)))
(test (parse '{- 2 1}) (sub (real 2) (real 1)))
(test (parse '{+ 1 {2 i}}) (add (real 1) (imaginary 2)))
(test (parse '{+ {2 i} 1}) (add (imaginary 2) (real 1)))
(test (parse '{- 1 {2 i}}) (sub (real 1) (imaginary 2)))
(test (parse '{- {2 i} 1}) (sub (imaginary 2) (real 1)))
(test (parse '{+ {- 3 4} {+ 5 9}}) (add (sub (real 3) (real 4)) (add (real 5) (real 9))))

;; if0
(test (parse '{if0 0 1 2}) (if0 (real 0) (real 1) (real 2)))
(test (parse '{if0 {+ 1 1} 1 {2 i}}) (if0 (add (real 1) (real 1)) (real 1) (imaginary 2)))
(test (parse '{if0 {if0 0 {3 i} 2} 1 {2 i}}) (if0 (if0 (real 0) (imaginary 3) (real 2)) (real 1) (imaginary 2)))

;; id & with
(test (parse 'x) (id 'x))


;; all together

;; ----- Parte c) -----


;; ----- Parte d) -----


;; ----- Parte e) -----