#lang play
(require "t3.rkt")

(print-only-errors #t)

;; P1  b)
(test (parse '1) (num 1) )
(test (parse '{+ 1 2}) (add (num 1) (num 2)) )
(test (parse '{nil}) (nil) )
(test (parse '{cons 1 2}) (conz (num 1) (num 2)) )
;; syntactic sugar
(test (parse '{list 1 2 3}) (conz (num 1) (conz (num 2) (conz (num 3) (nil)))) )
(test (parse '{list 1 {cons 2 3}}) (conz (num 1) (conz (conz (num 2) (num 3)) (nil))) )
(test (parse '{list 1 2 {list 3 4 5}}) 
    (conz (num 1) (conz (num 2) (conz (conz (num 3) (conz (num 4) (conz (num 5) (nil)))) (nil)))) )

;; P1 d) parse-pattern

(test (parse-pattern '3) (numP 3) )
(test (parse-pattern '(nil)) (nilP) )
(test (parse-pattern 'x)  (varP 'x) )
(test (parse-pattern '(cons 1 x)) (parse-pattern '(cons 1 x)) )
;; syntactic sugar
(test (parse-pattern '{list 1 x 3}) (conzP (numP 1) (conzP (varP 'x) (conzP (numP 3) (nilP)))) )
(test (parse-pattern '{list 1 {cons 2 3} 4}) (conzP (numP 1) (conzP (conzP (numP 2) (numP 3)) (conzP (numP 4) (nilP)))) )