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
          | (id <symbol>)
          | (fun <symbol> <expr>)
          | (app <expr> <expr>)
          ;;------------- 2.a -------------;;
|#
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (nil)
  (conz first_elem second_elem)
  (fun arg body)
  (app f arg)
)

;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;

#|
;; Concrete syntax of expressions:
<s-expr> ::= 
          ;------------- 1.b -------------;;
          | num
          | (list '+ <s-expr> <s-expr>)
          | (list 'nil)
          | (list 'conz <s-expr> <s-expr>)
          ;;------------- 1.e -------------;;
          | sym
          | (list 'fun <sym> <s-expr>) ;; Son funciones unarias, por eso solo <sym> en lugar de (list <sym>)
          | (list <s-expr> <s-expr>)
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
    [(? symbol? x) (id x)]
    [(list 'fun x body) (fun (parse-pattern x) (parse body))]
    [(list f a) (app (parse f) (parse a))]
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
;; parses a s-expr into a Expr, ie, constructs the AST
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
<value> ::= 
        | (numV <number>)
        | (nilV)
        | (conzV <value> <value>) ;; Donde first y second ambos son de tipo Value
        | (closureV <pattern> <expr> <env>)
|#

(deftype Value
  (numV n)                                  
  (nilV)                             
  (conzV first second)                      
  (closureV pattern body env)
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
    [(mtEnv) (error 'LookupError "variable ~a not found" x)]
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
;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
(define (generate-substs p v)
  (match p
    ;; Caso 1: Patrón es un número
    [(numP n)
     (match v
       [(numV m) (if (= n m)
                     (success '())
                     (failure "MatchError: given number does not match pattern"))]
       [_ (failure "MatchError: expected a number")])]
    
    ;; Caso 2: Patrón es una variable
    [(varP x) (success (list (cons x v)))]
    
    ;; Caso 3: Patrón es nil
    [(nilP)
     (match v
       [nilV (success '())]
       [_ (failure "MatchError: expected nil")])]
    
    ;; Caso 4: Patrón es un par (conzP)
    [(conzP first_pat second_pat)
     (match v
       [(conzV first_val second_val)
        (match (generate-substs first_pat first_val)
          [(failure e) (failure e)]
          [(success subs1)
           (match (generate-substs second_pat second_val)
             [(failure e) (failure e)]
             [(success subs2) (success (append subs1 subs2))])])]
       [_ (failure "MatchError: expected a cons constructor")])]
    ))


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
