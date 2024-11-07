#lang play

#|

Hizo Ud uso de la whiteboard policy: NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;

;; Abstract syntax of Expressions ;; Gramatica BNF Expression

#|
<expr> ::= 
          ;;------------- 1.a -------------;;
          | (num <number>)
          | (and <expr> <expr>)
          | (nil)
          | (conz <expr> <expr>)   
          ;;------------- 1.e -------------;;
          ;;------------- 2.a -------------;;
|#
(deftype Expr
  (num n)
  (add l r)
  (nil)
  (conz first_elem second_elem)  
)

;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;

#|
;; Concrete syntax of expressions:
<s-expr> ::= 
          | num
          | (list '+ <s-expr> <s-expr>)
          | (list 'nil)
          | (list 'conz <s-expr> <s-expr>)
|#

;; parse : s-expr -> Expr
;; parses a s-expression into a Expr, ie, constructs the AST
(define (parse s-expr)
  (match s-expr
    ;;------------- 1.b -------------;;
    [(? number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'nil) (nil)]
    [(list 'cons first_elem second_elem) (conz (parse first_elem) (parse second_elem))]
    [(list 'list x elems ...) (foldr (lambda (elem acc) (conz elem acc)) (nil) (cons (parse x) (map parse elems)))]
    ;;------------- 1.e -------------;;

    ;;------------- 2.b -------------;;
  )
)

;;----- ;;
;; P1.c ;;
;;----- ;;

;; Abstract syntax of Patterns ;; Gramatica BNF Pattern
#|
<pattern> ::= 
          | (numP <number>)
          | (andP <pattern> <pattern>)
          | (nilP)
          | (conzP <pattern> <pattern>)
|#
(deftype Pattern
  (numP n)
  (varP x)
  (nilP)
  (conzP first_pat second_pat)
)

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
(define (parse-pattern s-expr)
  (match s-expr
    [(? number? n) (numP n)]
    [(? symbol? x) (varP x)]
    [(list 'nil) (nilP)]
    [(list 'cons first_pat second_pat) (conzP (parse-pattern first_pat) (parse-pattern second_pat))]
    [(list 'list x elems ...) (foldr (lambda (elem acc) (conzP elem acc)) (nilP) (cons (parse-pattern x) (map parse-pattern elems)))]
  )
)

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= ...
|#
(deftype Value
  (numV n)
  ; ...
  )

#|
BEGIN utility definitions
|#

#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#
(deftype Env
  (mtEnv)
  (extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
  (extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
  (foldr
   (lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
   env
   bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
  (match env
    [(mtEnv) (error "LookupError: variable ~a not found" x)]
    [(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
  (match v1
    [(numV n) (match v2
                [(numV m) (numV (+ n m))]
                [_ (error "TypeError: expected a number")])]
    [_ (error "TypeError: expected a number")]))


#|
END utility definitions
|#


;;----- ;;
;; P1.g ;;
;;----- ;;

#|
<result> e v ::= (failure e)
               | (success v)
|#
(deftype Result
  (failure e)
  (success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
(define (generate-substs p v) '???)

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
(define (interp expr env) '???)


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: ...

|#
