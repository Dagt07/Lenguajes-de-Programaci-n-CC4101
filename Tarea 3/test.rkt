#lang play
(require "t3.rkt")

(print-only-errors #t)

;; P1  b)
(test (parse '1) (num 1) )
(test (parse '-2) (num -2) )
(test (parse '{+ 1 2}) (add (num 1) (num 2)) )
(test (parse '{+ -1 2}) (add (num -1) (num 2)) )
(test (parse '{+ {+ 1 3} 2}) (add (add (num 1) (num 3)) (num 2)) )
(test (parse '{+ 2 {+ 1 3}}) (add (num 2) (add (num 1) (num 3))) )
(test (parse '{nil}) (nil) )
(test (parse '{list}) (nil) )
(test (parse '{cons 1 2}) (conz (num 1) (num 2)) )
;; syntactic sugar
(test (parse '{list 1 2 3}) (conz (num 1) (conz (num 2) (conz (num 3) (nil)))) )
(test (parse '{list 1 {cons 2 3}}) (conz (num 1) (conz (conz (num 2) (num 3)) (nil))) )
(test (parse '{list 1 2 {list 3 4 5}}) 
    (conz (num 1) (conz (num 2) (conz (conz (num 3) (conz (num 4) (conz (num 5) (nil)))) (nil)))) )

;; P1 d) parse-pattern
(test (parse-pattern '3) (numP 3) )
(test (parse-pattern '{nil}) (nilP) )
(test (parse-pattern 'x)  (varP 'x) )
(test (parse-pattern '{cons 1 x}) (parse-pattern '(cons 1 x)) )
;; syntactic sugar
(test (parse-pattern '{list}) (nilP) )
(test (parse-pattern '{list 1 x 3}) (conzP (numP 1) (conzP (varP 'x) (conzP (numP 3) (nilP)))) )
(test (parse-pattern '{list 1 {cons 2 3} 4}) (conzP (numP 1) (conzP (conzP (numP 2) (numP 3)) (conzP (numP 4) (nilP)))) )

;; P1 e) 
(test (parse 'x) (id 'x) )
(test (parse '{fun x x}) (fun (varP 'x) (id 'x)) )
(test (parse '{fun {cons x xs} x}) (fun (conzP (varP 'x) (varP 'xs)) (id 'x)) )
(test (parse '{f x}) (app (id 'f) (id 'x)) )
(test (parse '{+ x 1}) (add (id 'x) (num 1)) )
(test (parse '{cons x 1}) (conz (id 'x) (num 1)) )
(test (parse '{list x 1}) (conz (id 'x) (conz (num 1) (nil))) )
(test (parse '{fun 1 2}) (fun (numP 1) (num 2)) )

;; P1 g)
(test (generate-substs (numP 3) (numV 3)) (success '()) )
(test (generate-substs (numP 3) (numV 4)) (failure "MatchError: given number does not match pattern") )
(test (generate-substs (varP 'x) (numV 3)) (success ( list (cons 'x (numV 3)))) )
(test (generate-substs (numP 8) (nilV)) (failure "MatchError: expected a number") )

(test (generate-substs (varP 'y) (numV 42)) (success (list (cons 'y (numV 42)))) )
(test (generate-substs (varP 'z) (conzV (numV 10) (nilV))) (success (list (cons 'z (conzV (numV 10) (nilV))))) )
(test (generate-substs (varP 'w) (closureV (varP 'a) (id 'a) empty-env))
      (success (list (cons 'w (closureV (varP 'a) (id 'a) empty-env)))) )

(test (generate-substs (conzP (numP 1) (numP 2)) (conzV (numV 1) (numV 2))) (success '()) )
(test (generate-substs (conzP (numP 3) (numP 5)) (conzV (numV 3) (numV 4))) (failure "MatchError: given number does not match pattern") )
(test (generate-substs (conzP (varP 'x) (numP 7)) (conzV (nilV) (numV 7))) (success (list (cons 'x (nilV)))) )
(test (generate-substs (conzP (varP 'a) (varP 'b)) (conzV (numV 99) (nilV)))
      (success (list (cons 'a (numV 99)) (cons 'b (nilV)))) )

(test (generate-substs (conzP (numP 9) (conzP (varP 'y) (nilP))) (conzV (numV 9) (conzV (numV 6) (nilV))))
      (success (list (cons 'y (numV 6)))) )
(test (generate-substs (conzP (numP 7) (conzP (numP 4) (nilP))) (conzV (numV 7) (conzV (numV 4) (nilV)))) (success '()) )
(test (generate-substs (conzP (varP 'x) (conzP (varP 'y) (numP 2))) (conzV (numV 3) (conzV (numV 1) (numV 2))))
      (success (list (cons 'x (numV 3)) (cons 'y (numV 1)))) )
(test (generate-substs (conzP (varP 'x) (conzP (numP 5) (numP 2))) (conzV (numV 3) (conzV (numV 5) (numV 1))))
      (failure "MatchError: given number does not match pattern") )

(test (generate-substs (nilP) (nilV)) (success '()) )
(test (generate-substs (nilP) (conzV (numV 3) (numV 4))) (failure "MatchError: expected nil") )
(test (generate-substs (nilP) (numV 6)) (failure "MatchError: expected nil") )


;; P1 h) interp
(test (interp (num 10) empty-env) (numV 10) )
(test (interp (add (num 4) (num 6)) empty-env) (numV 10) )
(test (interp (add (num 15) (num 5)) empty-env) (numV 20) )

(test (interp (nil) empty-env) (nilV) )
(test/exn (interp (add (nil) (num 4)) empty-env) "TypeError: expected a number")

(define env2 (extend-env 'z (numV 7) empty-env))
(test (interp (id 'z) env2) (numV 7) )
(test/exn (interp (id 'x) env2) "LookupError: variable x not found" )

(test (interp (conz (num 5) (nil)) empty-env) (conzV (numV 5) (nilV)) )
(test (interp (conz (num 4) (conz (num 3) (nil))) empty-env) (conzV (numV 4) (conzV (numV 3) (nilV))) )

(define inc-func (fun (varP 'x) (add (id 'x) (num 5))))
(test (interp inc-func empty-env) (closureV (varP 'x) (add (id 'x) (num 5)) empty-env) )
(test (interp (app inc-func (num 3)) empty-env) (numV 8) )

(define list-func (fun (conzP (varP 'head) (varP 'tail)) (id 'head)))
(test (interp list-func empty-env) (closureV (conzP (varP 'head) (varP 'tail)) (id 'head) empty-env) )
(test (interp (app list-func (conz (num 10) (nil))) empty-env) (numV 10) )

(define const-func (fun (numP 20) (num 42)))
(test (interp (app const-func (num 20)) empty-env) (numV 42) )
(test/exn (interp (app const-func (num 10)) empty-env) "MatchError: given number does not match pattern" )

(define nil-func (fun (nilP) (num 100)))
(test (interp (app nil-func (nil)) empty-env) (numV 100) )
(test/exn (interp (app nil-func (num 1)) empty-env) "MatchError: expected nil" )

(test/exn (interp (add (conz (num 1) (num 2)) (num 5)) empty-env) "TypeError: expected a number" )
(test/exn (interp (app inc-func (conz (num 2) (nil))) empty-env) "TypeError: expected a number" )
(test/exn (interp (app list-func (num 20)) empty-env) "MatchError: expected a cons constructor" )
(test/exn (interp (app const-func (nil)) empty-env) "MatchError: expected a number" )
(test/exn (interp (app nil-func (conz (num 1) (nil))) empty-env) "MatchError: expected nil" )


;; P2 b)