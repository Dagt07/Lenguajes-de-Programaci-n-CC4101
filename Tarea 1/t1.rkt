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
;; función de orden superior que recibe dos funciones y una fracción continua, y aplica la función correspondiente a la fracción continua
(define (fold-cfraction func_for_simple func_for_compound)
    (λ (cfrac)
        (match cfrac
            [(simple val) (func_for_simple val)]
            [(compound val1 val2 cf) 
             (func_for_compound val1
                val2 ((fold-cfraction func_for_simple func_for_compound) cf))]
        )
    )
)

;; Parte e)
;; redefinir eval y degree usando la abstracción de fold-cfraction
;; eval2 :: CFraction -> Rational
;; evalua una fracción continua, devolviendo el número racional que representa usando el patron de abstracción fold
(define eval2
    (fold-cfraction (λ (val) val)
                    (λ (val1 val2 cf) (+ val1 (/ val2 cf))) )
)

;; degree2 ::  CFraction -> Integer
;; devuelve el grado de una fracción continua usando el patron de abstracción fold
(define degree2
    (fold-cfraction (λ (val) 0)
                    (λ (val1 val2 cf) (+ 1 cf)) )
)

;; Observación: recordar que las fold-function usan los mismos tests que la function original o que quieren abstraer

;; Parte f)
;; mysterious-cf :: Integer -> CFraction
;; genera una secuencia de fracciones continuas que aumenta de grado de acuerdo al argumento entero dado
(define (mysterious-cf n)
  (cond
    [(< n 0) (error "Error: argumento negativo")] 
    [(zero? n) (simple 6)] 
    [else (compound 6 (sqr 1) (mysterious-cf-helper 3 n))]) ;; definimos una función auxiliar que nos ayude a construir fracciones continuas siguiendo el patron dado
)

;; mysterious-cf-helper :: Integer Integer -> CFraction
;; función auxiliar para construir fracciones continuas compuestas
(define (mysterious-cf-helper k n)
  (if (equal? k (+ (* 2 n) 1))
      (simple 6)
      (compound 6 (sqr k) (mysterious-cf-helper (+ k 2) n))) ;; Construcción recursiva con incrementos impares
)


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Construye una lista de enteros comprendidos entre dos enteros dados (excluyendo el segundo)
(define (from-to start end)
  (if (>= start end)
      '()
      (cons start (from-to (+ start 1) end)))
)

;; mysterious-list :: Integer -> ListOf Float
;; Devuelve una lista tal que el i-ésimo elemento es calculado como la resta de la evaluación de (mysterious-cf i) menos 3
(define (mysterious-list n)
  (map (λ (i)
         (fl (- (eval (mysterious-cf i)) 3)) )  ;; Evalúa (mysterious-cf i), resta 3, y convierte a float
       (from-to 0 n))
)

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
#|
Lo probe para (mysterious-list 10), (mysterious-list 100), (mysterious-list 1000) y (mysterious-list 5000) y parece tender a Pi para k tendiendo al infinito
,ciertamente de acuerdo a lo observado, se acerca cada vez más a los dígitos de Pi, es decir, los primeros dígitos de Pi van coincidiendo con los dígitos de (mysterious-list k) a medida que k aumenta
|#

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Convierte un número racional no negativo a su representación en forma de fracción continua
(define (rac-to-cf r) 
    (if (negative? r)
        (error "Error: número negativo")
        (let* ([a (floor r)])
            (if (equal? (- r a) 0)
                (simple a)
                (compound a 1 (rac-to-cf (/ 1 (- r a))) )
            )
        )
    )
)