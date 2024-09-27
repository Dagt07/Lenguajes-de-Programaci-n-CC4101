#lang play
(require "t2.rkt")

(print-only-errors #t)

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

(test/exn (parse-prop '{and}) "and expects at least two operands" )
(test/exn (parse-prop '{or}) "or expects at least two operands" )
(test/exn (parse-prop '{and true}) "and expects at least two operands" )
(test/exn (parse-prop '{or false}) "or expects at least two operands" )
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
        (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (ff) (p-where (p-id 'y) 'y (tt))))))) 'x (p-or (list (tt) (ff)))) )

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
(test (p-eval (parse-prop '{or false false true})) (ttV))
(test (p-eval (parse-prop '{and true false true true})) (ffV))

;; for this point on, we will use the parser to make writing tests easier and double checking the parser.
(test (p-eval (parse-prop '{x where [x true]}))  (ttV) )
(test (p-eval (parse-prop '{and true {not {not true}} {x where [x true]}})) (ttV) )
(test (p-eval (parse-prop '{{z where [y true]} where [z false]})) (ffV) )
(test (p-eval (parse-prop '{{z where [y true]} where [z {not false}]})) (ttV) )
(test (p-eval (parse-prop '{{z where [y true]} where [z {not {or false true}}]})) (ffV) )

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
(test (parse '{+ 1 2}) (add (real 1) (real 2)) )
(test (parse '{- 2 1}) (sub (real 2) (real 1)) )
(test (parse '{+ 1 {2 i}}) (add (real 1) (imaginary 2)) )
(test (parse '{+ {2 i} 1}) (add (imaginary 2) (real 1)) )
(test (parse '{- 1 {2 i}}) (sub (real 1) (imaginary 2)) )
(test (parse '{- {2 i} 1}) (sub (imaginary 2) (real 1)) )
(test (parse '{+ {- 3 4} {+ 5 9}}) (add (sub (real 3) (real 4)) (add (real 5) (real 9))) )

;; if0
(test (parse '{if0 0 1 2}) (if0 (real 0) (real 1) (real 2)) )
(test (parse '{if0 {+ 1 1} 1 {2 i}}) (if0 (add (real 1) (real 1)) (real 1) (imaginary 2)) )
(test (parse '{if0 {if0 0 {3 i} 2} 1 {2 i}}) (if0 (if0 (real 0) (imaginary 3) (real 2)) (real 1) (imaginary 2)) )

;; id & with
(test (parse 'x) (id 'x) )
(test (parse '{with [{x 1} {y 1}] {+ x y}}) (with (list (cons 'x (real 1)) (cons 'y (real 1))) (add (id 'x) (id 'y))) )
(test (parse '{with [{x z} {y 3}] {+ x y}}) (with (list (cons 'x (id 'z)) (cons 'y (real 3))) (add (id 'x) (id 'y))) )
(test (parse '{with [{x 4} {y (2 i)} {z x1}] {+ x y}}) (with (list (cons 'x (real 4)) (cons 'y (imaginary 2)) (cons 'z (id 'x1))) (add (id 'x) (id 'y))) )

(test/exn (parse '{with [] 1}) "with expects at least one binding" )

;; all together
(test (parse '{with [{x {1 i}} {y {if0 {+ 0 {-1 i}} {0 i} {- x 1}}}] {+ x y}})
      (with (list (cons 'x (imaginary 1)) (cons 'y (if0 (add (real 0) (imaginary -1)) (imaginary 0) (sub (id 'x) (real 1))))) (add (id 'x) (id 'y))) )

;; ----- Parte c) -----

;; from-CValue
(test (from-CValue (compV 1 0)) (real 1) )
(test (from-CValue (compV 0 1)) (imaginary 1) )
(test (from-CValue (compV 0 -2)) (imaginary -2))
(test (from-CValue (compV 0 0)) (add (real 0) (imaginary 0)) )
(test (from-CValue (compV 1 2)) (add (real 1) (imaginary 2)) )
(test (from-CValue (compV -1 -4)) (add (real -1) (imaginary -4)) )

(test/exn (from-CValue 'x) "Invalid CValue" )
(test/exn (from-CValue '()) "Invalid CValue" )
(test/exn (from-CValue '(1 2 3 4 5)) "Invalid CValue" )

;; cmplx+
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6) )
(test (cmplx+ (compV 10 0) (compV 5 0)) (compV 15 0) )
(test (cmplx+ (compV 0 10) (compV 0 5)) (compV 0 15) )
(test (cmplx+ (compV 0 0) (compV 0 0)) (compV 0 0) )

;; cmplx-
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2) )
(test (cmplx- (compV 10 0) (compV 5 0)) (compV 5 0) )
(test (cmplx- (compV 0 10) (compV 0 5)) (compV 0 5) )
(test (cmplx- (compV 0 0) (compV 0 0)) (compV 0 0) )

;; cmplx0?
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 1 0)) #f)
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 1)) #f)
(test (cmplx0? (cmplx+ (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx+ (compV 4 -4) (compV -4 4))) #t)
(test (cmplx0? (cmplx+ (compV -4 -2) (compV -6 -3))) #f)
(test (cmplx0? (cmplx- (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx- (compV 2 3) (compV 2 3))) #t)
(test (cmplx0? (cmplx- (compV 3 2) (compV 4 1))) #f)

;; --- auxiliary functions ---
(test (cvalue-real (compV 1 2)) 1)
(test (cvalue-real (compV 0 3)) 0)
(test (cvalue-imaginary (compV 1 2)) 2)
(test (cvalue-imaginary (compV 3 0)) 0)

;; ----- Parte d) -----

;; no shadowing
(test (subst (parse '{with [{x 2} {y z}] {+ x z}}) 'z {real 1}) 
        (with ( list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))) )
(test (subst (parse '{+ x y}) 'x (real 10)) (add (real 10) (id 'y)) )
(test (subst (parse '{if0 x 1 0}) 'x (real 5)) (if0 (real 5) (real 1) (real 0)) )
(test (subst (parse '{+ {- x 3} y}) 'x (real 2)) (add (sub (real 2) (real 3)) (id 'y)))
;; shadowing
(test (subst (parse '{with [{x 2} {y x}] {+ x x}}) 'x (real 1)) 
        (with ( list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))) )
(test (subst (parse '{if0 x {with [{x 2}] {+ x y}} z}) 'x (real 4)) 
        (if0 (real 4) (with (list (cons 'x (real 2))) (add (id 'x) (id 'y))) (id 'z)) )
(test (subst (parse '{with [{x 2}] {+ x {with [{x 3}] {+ x y}}}}) 'x (real 5))
      (with (list (cons 'x (real 2))) (add (id 'x) (with (list (cons 'x (real 3))) (add (id 'x) (id 'y))))) )

;; note: subst-aux is define inside subst, then is tested indirectly by the tests of subst

;; ----- Parte e) -----

(test (interp (parse '1)) (compV 1 0) )
(test (interp (parse '{2 i})) (compV 0 2) )
(test (interp (parse '{+ 1 2})) (compV 3 0) )
(test (interp (parse '{+ 1 {2 i}})) (compV 1 2) )
(test (interp (parse '{- 1 {2 i}})) (compV 1 -2) )
(test (interp (parse '{+ {- 3 4} {+ 5 9}})) (compV 13 0) )

(test (interp (parse '{if0 1 2 3})) (compV 3 0) ) ;; cond is false, then return false branch (3)
(test (interp (parse '{if0 {+ 0 {0 i}} 1 0})) (compV 1 0) ) ;; cond is true, then return true branch (1)
(test (interp (parse '{if0 {+ 0 {1 i}} 1 {2 i}})) (compV 0 2) ) 
(test (interp (parse '{if0 {- {+ 2 {2 i}} {+ 2 {2 i}}} 1 {2 i}})) (compV 1 0) )

(test (interp (parse '{with {{x 3}} {with {{x {4 i}}} {+ x x}}})) (compV 0 8) )
(test (interp (parse '{if0 0 {with {{x {3 i}}} {+ x 2}} x})) (compV 2 3) )
(test/exn (interp (parse 'x)) "Open expression (free occurrence of id x)" )
