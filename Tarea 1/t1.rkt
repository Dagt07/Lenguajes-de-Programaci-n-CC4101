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
<CFraction> ::= (simple <Integer>)
              | (compound <Integer> <Integer> <CFraction>)
|#

(deftype CFraction
    (simple value)
    (compound value1 value2 fraction)
)

;; identificadores comunes a usar en los tests
(define simple_cf (simple 1))
(define my_cf (compound 12 1 (simple 4)))
(define my_cf_2 (compound 4 1 (compound 12 1 (simple 4))))
(define my_cf_3 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))

;; Parte b)
;; eval :: CFraction -> Rational
;; evalua una fracción continua, devolviendo el número racional que representa

(define (eval cfrac)
    (match cfrac
        [(simple val) val]
        [(compound val1 val2 cf) (+ val1 (/ val2 (eval cf)))]
    )
)

;; Parte c)
;; degree ::  CFraction -> Integer
;; devuelve el grado de una fracción continua

(define (degree cfrac)
    (match cfrac
        [(simple val) 0]
        [(compound val1 val2 cf) (+ 1 (degree cf))]
    )
)

;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)

(define (fold-cfraction func_for_simple func_for_compound)
    (λ (cfrac)
        (match cfrac
            [(simple val) (func_for_simple val)]
            [(compound val1 val2 cf) 
             (func_for_compound val1
                ((fold-cfraction func_for_simple func_for_compound) cf))]
        )
    )
)

;; Parte e)
;; redefinir eval y degree usando la abstracción de fold-cfraction
;; eval2 :: CFraction -> Rational
(define fold-eval 
    (fold-cfraction (λ (val) val)
                    (λ (val1 val2 cf) (+ val1 (/ val2 cf))) )
)

;; degree2 ::  CFraction -> Integer
(define fold-degree
    (fold-cfraction (λ (val) 0)
                    (λ (val1 val2 cf) (+ 1 cf)) )
)

;; Observación: recordar que las fold-function usan los mismos tests que la function original o que quieren abstraer

;; Parte f)
;; mysterious-cf :: Integer -> CFraction


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer


;; mysterious-list :: Integer -> ListOf Float


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
