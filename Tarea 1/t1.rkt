#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: David García
RUT: 266834624-4
|#

;; Parte a) Tipo de datos recursivo y su grámatica

;; Gramatica BNF 
#|
<CFraction> ::= (simple <value>)
              | (compound <value> <value> <CFraction>)
|#

(deftype CFraction
    (simple value)
    (compound value value fraction)
)

;; Parte b)
;; eval :: CFraction -> Rational


;; Parte c)
;; degree ::  CFraction -> Integer


;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)


;; Parte e)
;; eval2 :: CFraction -> Rational


;; degree2 ::  CFraction -> Integer


;; Observación: recordar que las fold-function usna los mismos tests que la function original o que quieren abstraer

;; Parte f)
;; mysterious-cf :: Integer -> CFraction


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer


;; mysterious-list :: Integer -> ListOf Float


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
