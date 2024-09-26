#lang play

#|
Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
- Jean Paul Duchens, P1.e 
-
|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;;----- ;;
;; P1.a ;;
;;----- ;;

#|
Abstract syntax of propositions:

<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and listOf(prop)) ;; listOf<prop> ;; podría ir la elipsis acá mismo o * o listOf
         | (p-or listOf(prop))
         | (p-where <prop> <symbol> <prop>)
         | (p-id <symbol>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-not prop)
  (p-and prop-ls)
  (p-or prop-ls)
  (p-id name)
  (p-where body id named-prop) ;; notar que respecto a with visto en clase, cambio el orden de los argumentos
)

;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'not <s-prop>)
          | (list 'and <s-prop> <s-prop> ...)
          | (list 'or <s-prop> <s-prop> ...)
          | (list s-prop 'where (list <symbol> <s-prop>))
          | <symbol> ;; p-id
|#

;; parse-prop : <s-prop> -> Prop
;; parses a s-propotition into a Prop, ie, constructs the AST
(define (parse-prop s-expr)
  (match s-expr
    ['true (tt)]
    ['false (ff)]
    [(? symbol? x) (p-id x)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and props ...) (if (< (length props) 2)
                                (error 'parse-prop "and expects at least two operands")
                                (p-and (map parse-prop props))) ]
    [(list 'or props ...) (if (< (length props) 2)
                               (error 'parse-prop "or expects at least two operands")
                               (p-or (map parse-prop props))) ]
    [(list body 'where (list (? symbol? id) named-prop))
     (p-where (parse-prop body) id (parse-prop named-prop))]
    
  )
)

;;----- ;;
;; P1.c ;;
;;----- ;;

;; BNF for PValue
;; abstraction to represent booleans values for not depending on the language used
#|
<pvalue> ::= (ttV)
          | (ffV)
|#

(deftype PValue 
  (ttV)
  (ffV)
)

;; from-Pvalue : PValue -> Prop
;; captures a PValue and returns the corresponding Prop with prop "boolean" value instead of PValue
(define (from-Pvalue p-value) 
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]
  )
)

;;----- ;;
;; P1.d ;;
;;----- ;;

;; p-subst : Prop Symbol Prop -> Prop
;; (subs target name substitution)
;; substitutes all the free ocurrencies of id 'name' in 'target' by 'substitution'
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id id) (if (equal? id name)
                   substitution
                   target)]
    [(p-not prop) (p-not (p-subst prop name substitution))]
    [(p-and prop-ls) (p-and (map (λ (prop) (p-subst prop name substitution)) prop-ls))]
    [(p-or prop-ls) (p-or (map (λ (prop) (p-subst prop name substitution)) prop-ls))]
    ;; where case
    [(p-where body id named-prop) 
      (if (equal? id name) ;; name (what we want to subst) vs id (ocurrence we found, ie, inner id)
        target ;; it wasnt a free ocurrence, we dont substitute
        (p-where (p-subst body name substitution) id (p-subst named-prop name substitution)))]
  )
)

;;----- ;;
;; P1.e ;;
;;----- ;;

;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps)
  (cond
    [(null? ps) (ffV)]
    [(equal? (p-eval (car ps)) (ttV)) (ttV)]
    [else (eval-or (cdr ps))]))

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps)
  (cond
    [(null? ps) (ttV)]
    [(equal? (p-eval (car ps)) (ffV)) (ffV)]
    [else (eval-and (cdr ps))]))

;; p-eval :: Prop(AS) -> PValue
;; Evaluates a proposition, aka, the interpreter.
(define (p-eval p) 
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-not prop) (if (equal? (p-eval prop) (ttV)) 
                        (ffV)
                        (ttV))]
    [(p-and prop-ls) (eval-and prop-ls)]
    [(p-or prop-ls) (eval-or prop-ls)]
    ;; where case
    [(p-where body id named-prop) 
      (p-eval (p-subst body id (from-Pvalue (p-eval named-prop))))]
    ;; p-id case
    [(p-id name) (error 'p-eval "Open expression (free occurrence of p-id ~a)" name)]
  )
)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Notes:
 - Aux said: dont consider the case where the real part of a imaginary number <num> its 
  ;; another expression, ie, (imaginary (add 1 1) 'i) is not a valid expression
|#

;;----- ;;
;; P2.a ;;
;;----- ;;

#|
Abstract syntax of expretions:

<expr> ::= (real <num>)
        | (imaginary <num>) 
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (with <expr> <expr>)
        | (id <sym>)
|#
(deftype Expr
  (real r)
  (imaginary r)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with bindings body)
  (id name)
)

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= 
        | <num>
        | (<num> 'i)
        | (list '+ <s-expr> <s-expr>)
        | (list '- <s-expr> <s-expr>)
        | (list 'if0 <s-expr> <s-expr> <s-expr>)
        | (list 'with (list (list <sym> <s-expr>)) <s-expr> <s-expr>)
        | <sym>
|#

;; parse : <s-expr> -> Expr
;; parses a s-expretion into a Expr, ie, constructs the AST
(define (parse s-expr)
  (match s-expr
    [(? number? n) (real n)]
    [(list n 'i) #:when (number? n) (imaginary n)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
      (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))] 
    [(list 'with (list bindings ...) body) ;; with * case
      (if (null? bindings)
        (error 'parse "with expects at least one binding") 
        ;; else (false branch)
        (with 
            (map (λ (binding) 
              (match binding [(list (? symbol? name) named-expr) (cons name (parse named-expr))]) ) 
            bindings) 
          (parse body))
      )
    ]
    [(? symbol? name) (id name)]
  )
)

;;----- ;;
;; P2.c ;;
;;----- ;;

#|
BNF for CValue
abstraction to represent Complex values for not depending on the language used
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; captures a CValue (ComplexValue) and returns the corresponding Expr (AST)
(define (from-CValue v)
  (match v
    [(compV r i) (cond [(and (zero? i) (not (zero? r))) (real r)]
                       [(and (zero? r) (not (zero? i))) (imaginary i)]
                       [else (add (real r) (imaginary i))] )]
    [_ (error 'from-CValue "Invalid CValue")]
  )
)

;; --- auxiliary functions ---
;; cvalue-real :: CValue -> Num
;; returns the real part of a complex number
(define (cvalue-real cvalue)
  (match cvalue
    [(compV r i) r]))

;; cvalue-imaginary :: CValue -> Num
;; returns the imaginary part of a complex number
(define (cvalue-imaginary cvalue)
  (match cvalue
    [(compV r i) i]))
;; ---------------------------

;; cmplx+ :: CValue CValue -> CValue
;; adds two complex numbers
(define (cmplx+ v1 v2) 
  (compV (+ (cvalue-real v1) (cvalue-real v2))
         (+ (cvalue-imaginary v1) (cvalue-imaginary v2)))
)

;; cmplx- :: CValue CValue -> CValue
;; subtracts two complex numbers
(define (cmplx- v1 v2)
  (compV (- (cvalue-real v1) (cvalue-real v2))
         (- (cvalue-imaginary v1) (cvalue-imaginary v2)))
)

;; cmplx0? :: CValue -> Boolean
;; returns a boolean indicating if the complex number is 0, ie, real and imaginary parts are 0
(define (cmplx0? v)
  (match (from-CValue v) 
    [(add (real 0) (imaginary 0)) #t]
    [_ #f]
  )
)

;;----- ;;
;; P2.d ;;
;;----- ;;

;; subst : una auxiliar, luego con un map car, member

;; subst :: Expr Symbol Expr -> Expr
;; (subst in what for)
;; substitutes all the free ocurrencies of id 'what' in 'in' by 'for'
(define (subst in what for) 

  ;; auxiliary function
  ;; append y esas cosas, ver aux 5
  (define (subst-aux bindings what for)
    (match bindings
      ['() '()] ;;caso base
      [(list (cons x expr) elemes ...)
        (if (equal? x what) ;; si se le está haciendo shadowing
          (append (list (cons x expr)) elemes) ;; no se hace la sustitución
          (append (list (cons x (subst expr what for))) (subst-aux elemes what for) ) ;; se hace la sustitución
        )
      ]
    )
  )

  (match in
  
    [(real n) (real n)]
    [(imaginary n) (imaginary n)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for) (subst t what for) (subst f what for))]
    [(id x) (if (symbol=? x what) for (id x))]
    [(with bindings body) ;;bindings its a list of (list <sym> <expr>)
      (define listaux (subst-aux bindings what for))
      (with 
            listaux
            ;; como la func shadowing
            ;; acá verifica si se hizo la sustitución en alguna de las bindings
            (if (member what (map car listaux)) ;; nos da el id de cada (list <sym> <expr>)
                body 
                (subst body what for))
      )
    ]
  )
)

;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
;; Evaluates an arithmetic expression
(define (interp expr) 
  (match expr
    [(real n) (compV n 0)]
    [(imaginary n) (compV 0 n)]
    [(add l-expr r-expr) (cmplx+ (interp l-expr) (interp r-expr))]
    [(sub l-expr r-expr) (cmplx- (interp l-expr) (interp r-expr))]
    [(if0 c t f) (if (cmplx0? (interp c)) 
                    (interp t)
                    (interp f))]
    ;; with case
    [(with bindings body) 
      (interp (foldl (λ (binding body) (subst body (car binding) (interp (cdr binding)))) body bindings))
    ]
    [(id x) (error 'interp "Open expression (free occurrence of id ~a)" x)] ;; quiza haya que cambiarla pero no creo
  )
)
