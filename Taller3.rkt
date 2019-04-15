#lang eopl

;CAMILO REBELLON GIRON 1556073-3743
;MATEO ECHEVERRY 1860098-3743
;LUIS ROMERO 1866236-3743

;;**************Especificacioneslexicas***************  
;<digitoDecimal> ::= <digito> <digito>*
;<digitoDecimal> ::= "-" <digito> <digito>*
;<digitoOctal> ::= "0x" ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ) *
;<digitoOctal> ::= "-" "0x" ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ) *
;<hexadecimal> ::= "hx" ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | "A" | "B" | "C" | "D" | "E" | "F" ) *
;<hexadecimal> ::= "-" "hx" ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | "A" | "B" | "C" | "D" | "E" | "F" ) *
;<flotante> ::= <digito> <digito> * "." <digito> <digito>*
;<flotante> ::= "-" <digito> <digito> * "." <digito> <digito>*
;<cadena> ::= "<simbolo> <simbolo> *"
;<comentario> ::= "\\" ( not#nnewline )
;<espacio> ::= <whitespace>

;<programa> ::= <struct-decl>* <expresion>* (a-programa)

;<expresion> ::= <bool-expresion> (bool-exp)
;            ::= <identificador> (var-exp)
;            ::= <numero> (num-exp)
;            ::= <cadena> (cadena-exp)
;            ::= <variable-decl> (decl-exp)
;            ::= "const" <identificador> "=" expresion (const-exp)
;            ::= "set" <identificador> "=" <expresion> (set-exp)
;            ::= "(" <expresion> <primitiva> <expresion> ")" (prim-exp) 
;            ::= "list" <tipo-exp> "(" (<expresion>)*(,) ")" (lista-exp)
;
;            ;Condicionales y estructuras de control
;            ::= "if" <expresion> "{" <expresion> "else" <expresion> "{" (if-exp)
;            ::= "switch" "(" identificador ")" "{" ( "case" expresion expresion) * "default:" expresion "}" ( switch-exp )
;            ::= "for" <identificador> "=" <expresion> ";" expresion ";" expresion "{" expresion "}" ( for-exp )
;            ::= "while" expresion "{" expresion "}" (while-exp)
;
;            ;Secuenciacion
;            ::= "begin" (expresion) * (;) "end" (begin-exp)
;
;            ;Funciones
;            ::= "func" <identificador> "(" (<identificador> <tipo-exp) * (,) expresion "return" expresion (fun-cexp)
;            ::= "call" <identificador> "(" <expresion> * (,) ")" ( call-exp)
;
;            ;Invocacion y llamado de objetos
;            ::= "new" <identificador> "(" <expresion> * (,) ")" (new-obj)
;            ::= "send" <identificador> "." <identificador> "(" <expresion> * (,) ")" (method-invoke)

;<numero-exp>::= digitoDecimal (decimal-num)
;		::= digitoOctal (octal-num)
;		::= digitoHexadecimal (hex-num)
;		::= flotante (float-num)
;
;<bool-expresion>
;		::= true (true-exp)
;		::= false (false-exp)
;	
;
;;; Primitivas
;<primitiva> 
;		::= + (sum-prim)
;		::= - (minus-prim)
;		::= * (mult-prim)
;		::= mod (modulo-prim)
;		::= & (concat-prim)
;		::= pow (elevar-prim)
;		::= < (menor-prim)
;		::= > (mayor-prim)
;		::= <= (menorigual-prim)
;		::= >= (mayorigual-prim)
;		::= != (diferent-prim)
;		::= ==(igual-prim)
;		::= not (not-prim)
;		::= and (and-prim)
;		::= or (or-prim)
;		::= xor (xor-prim)
;		::= length (length-prim)
;		::= nth (nth-prim) ;Para acceder a una posicion de una lista
;
;;; Tipos
;<tipo-exp>  ::= int (int-tipo)
;		::= octal (octal-tipo)
;		::= hex (hex-tipo)
;		::= float (float-tipo)
;		::= string (texto-tipo)
;		::= bool (bool-tipo)
;		::= void (void-tipo)
;
;;;Declaracionvariables
;<variable-decl> 
;		::= "varN" <identificador> "=" <expresion> (decl1-exp)
;		::= "varT" <identificador> <tipo-exp> (decl2-exp)
;	
;;; Objetos (como estructuras)
;<struct-decl> ::= "type" <identificador> "struct" "{" <atributos>* <metodos>* "}" (obj-decl)
;<atributos> ::= <tipo> <identificador> (attr-exp)
;<metodos> ::= "method" <identificador> "(" (<identificador> <tipo-exp>) * (,) expresion "return" expresion (method-exp)


;lexico

(define lexico
'((digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")
         (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7")
         (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (hexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")
         (arbno(or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (hexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")
         (arbno(or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (comentario
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit) "?")) symbol)
  (cadena
   ("\"" (or letter digit)(arbno (or letter digit))"\"") string)
  (white-sp
   (whitespace) skip)
  )
)

;gramatica

(define gramatica
  '((program ((arbno struct-decl)expression) a-program)

    ;;;Gramatica
    (expression (numero-expression) num-exp)
    (expression (bool-expression) bool-exp)
    (expression (identificador) var-exp)
    (expression (cadena) cadena-exp)
;    (expression (variable) decl-exp)
;
;    (expression ("const" identificador "=" expression) const-exp)
    (expression ("set" identificador "=" expression) set-exp)
    (expression ("list" tipo-exp "(" (separated-list expression ",") ")" ) lista-exp)
    (expression ("(" expression primitive expression ")") prim-exp)
;
;    ;;Condicionales y estructuras de control
    (expression ("if" expression "{" expression "else" expression "}" ) if-exp)
    (expression ("switch" " ( "identificador " ) " " { " (arbno "case" expression expression) "default" expression "}") switch-exp)
    (expression ("while" expression "{"expression "}") while-exp)
;
;    ;;Secuenciacion
    (expression ("begin" (separated-list expression ";") "end") begin-exp)
;
;    ;;Funciones
    (expression ("func" identificador "("(separated-list identificador tipo-exp "," ) ")" expression) func-exp)
    (expression("call" identificador "("(separated-list expression "," ) " )" ) call-exp)

    
;
;    ;;Objetos
;    (expression ("new" identificador "(" (separated-list expression "," ) " )" ) new−obj)
;    (expression ("send" identificador "." identificador "(" (separated-list expression "," ) ")" )method−invoke)
    

    ;Primitiva de bool-expression
    (bool-expression ("true") true-exp)
    (bool-expression ("false") false-exp)
    
    ;Primitiva de numero-expression
    (numero-expression (digitoDecimal) decimal-num)
    (numero-expression (digitoOctal) octal-num)
    (numero-expression (hexadecimal) hex-num)
    (numero-expression (flotante) float-num)

    ;;Tipado
    (tipo-exp ("int") int-tipo)
    (tipo-exp ("octal") octal-tipo)
    (tipo-exp ("hex") hex-tipo)
    (tipo-exp ("float") float-tipo)
    (tipo-exp ("string") texto-tipo )
    (tipo-exp ("bool") bool-tipo )
    (tipo-exp ("void") void-tipo )

    ;:Programa
    (struct-decl ("type" identificador "struct" identificador "{"( arbno atributos) (arbno metodos) "}") obj-decl)
    (atributos (tipo-exp identificador) attr-exp)
    (metodos ("method" identificador "(" (separated-list identificador tipo-exp "," ) ")" expression "return" expression) method-exp)

    ;;;Primitivas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") modulo-prim)
    (primitive ("&") concat-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferent-prim)
    (primitive ("==") igual-prim)
    (primitive ("not") not-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("xor") xor-prim)
    (primitive ("length") length-prim)
    (primitive ("nth") nth-prim)))


;Pruebas

;Construidos automáticamente:

(sllgen:make-define-datatypes lexico gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexico gramatica)))

;;;;;;;;;;;;;

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexico gramatica))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define inter
  (sllgen:make-rep-loop "IZI -> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      lexico
      gramatica)))

;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) ;\new1
        (eval-expression exp (empty-env))))))


; Ambiente inicial
(define init-env
  (lambda ()
     (empty-env)))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (num-exp (numero-expression) (eval-num-expre numero-expression env))
      (var-exp (identificador) (apply-env env identificador))
      (bool-exp (bool-expression) (eval-bool-expre bool-expression env))
      (cadena-exp (cadena) cadena)
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (switch-exp (identificador casos body defaul)
                  ((eval-expression identificador env)
                   (guardar-iden casos env)
                   (guardar-iden body env)
                   (eval-expression defaul env)
                   ))
      (set-exp (id rhs-exp)
               (setref!
                (apply-env-ref env id)
                (eval-expression rhs-exp env))1)
      (while-exp (test-exp body)
                 (if(eval-expression test-exp)
                 (eval-expression body (body))
                  '()))
      (begin-exp (exps)
          (let loop ((acc (eval-expression (car exps) env))
                     (exps exps))
           (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      (lista-exp (tipo lista)
                 (tipo)
                 (eval-rands lista env))
      (prim-exp (expre1 prim expre2)
                (apply-primitive (eval-expression expre1 env)
                                 prim
                                 (eval-expression expre2 env)))
      (func-exp (id exps expts body)
                (id)
                (let loop ((acc (eval-expression (car exps) env))
                     (exps exps))
                 (if (null? exps) acc
                     (loop (eval-expression (car exps) env) (cdr exps))))
                (let loop ((acc (eval-expression (car expts) env))
                     (expts expts))
                 (if (null? exps) acc
                     (loop (eval-expression (car expts) env) (cdr expts))))
                (eval-expression body env))
      (call-exp (id exps)
                (id)
                (let loop ((acc (eval-expression (car exps) env))
                     (exps exps))
                 (if (null? exps) acc
                     (loop (eval-expression (car exps) env) (cdr exps))))))))

(define guardar-iden
  (lambda (expre env)
    (cond
      [(null? expre)empty]
      [else (cons (car expre) (guardar-iden (cdr expre)))])))

(define eval-num-expre
  (lambda (exp env)
    (cases numero-expression exp
      (decimal-num (digitoDecimal) digitoDecimal)
      (octal-num (digitoOctal) digitoOctal)
      (hex-num (digitoHexadecimal) digitoHexadecimal)
      (float-num (flotante) flotante))))

(define eval-bool-expre
  (lambda (exp env)
    (cases bool-expression exp
      (true-exp () "true")
      (false-exp () "false"))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expressiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (expression1 prim expression2)
    (cases primitive prim
      (sum-prim () (operar expression1 + expression2))
      (minus-prim ()  (operar expression1 - expression2))
      (mult-prim () (operar expression1 * expression2))
      (modulo-prim () (sacar-modulo expression1 expression2))
      (concat-prim () (concate expression1 expression2))
      (elevar-prim () (elevar-numero expression1 expression2))
      (menor-prim () (operar expression1 < expression2))
      (mayor-prim ()  (operar expression1 > expression2))
      (menorigual-prim () (operar expression1 <= expression2))
      (mayorigual-prim () (operar expression1 >= expression2))
      (diferent-prim () (not (operar expression1 = expression2)))
      (igual-prim () (operar expression1 = expression2))
      (not-prim () (not expression1))
      (and-prim ()  (and expression1 expression2))
      (or-prim () (or expression1 expression2))
      (xor-prim () (xor expression1 expression2))
      (length-prim () (string-length expression1))
      (nth-prim () (concate expression1 expression2))
      )))


;Funcion concate
(define concate
  (lambda(expre1 expre2)
    (string-append expre1 expre2)))

;Funcion xor
(define xor
  (lambda (operador1 operador2)
    (and
     (not (and operador1 operador2))
     (or operador1 operador2))))

;;Funciones para operar en base hexadecimal
(define operaHex
  (lambda (num)
    (string->number(string-append "#x" (substring num 2)))))

(define operaHexNeg
  (lambda (num)
    (string->number(string-append "#x-" (substring num 3)))))

(define d->h
  (lambda (dec)
    (define digitos "0123456789ABCDEF")
    (cond
    [(> (quotient dec 16) 15)(string-append (d->h (quotient dec 16))(string (string-ref digitos (remainder dec 16))))]
    [else (string-append "hx"(string (string-ref digitos (quotient dec 16)))(string (string-ref digitos (remainder dec 16))))])))

;Funciones para operar en bace octal

(define operaOct
  (lambda (num)
    (string->number(string-append "#o" (substring num 2)))))

(define operaOctNeg
  (lambda (num)
    (string->number(string-append "#o-" (substring num 3)))))

(define d->o
  (lambda (dec)
    (cond
      [(> (quotient dec 8) 7)(string-append (d->o (quotient dec 8)) (number->string (remainder dec 8)))]
      [else (string-append "0x" (number->string (quotient dec 8))(number->string (remainder dec 8)))])))

;Funcion que resive los numeros para elevar
(define elevar-numero
  (lambda (expre1 expre2)
    (cond

      ;Elevar decimales y flotante
      [(and (number? expre1) (number? expre2))(pow expre1 expre2)]

      ;Elevar hexa
      [(and(eqv? (car (string->list expre1)) #\h)
           (eqv? (car (string->list expre2)) #\h))
       (cond
         [(>= (pow (operaHex expre1) (operaHex expre2)) 0)
          (d->h (pow (operaHex expre1) (operaHex expre2)))]
         [(< (pow (operaHex expre1) (operaHex expre2)) 0)
          (string-append "-" (d->h (* -1 (pow (operaHex expre1) (operaHex expre2)))))])]
      [(and(eqv? (car (cdr (string->list expre1))) #\h)
           (eqv? (car (string->list expre2)) #\h))
       (cond
         [(>= (pow (operaHexNeg expre1) (operaHex expre2)) 0)
          (d->h (pow (operaHexNeg expre1) (operaHex expre2)))]
         [(< (pow (operaHexNeg expre1) (operaHex expre2)) 0)
          (string-append "-" (d->h (* -1 (pow (operaHexNeg expre1) (operaHex expre2)))))])]

      ;Elevar octales
      [(and(eqv? (car (string->list expre1)) #\0)
           (eqv? (car (string->list expre2)) #\0))(d->h (pow (operaOct expre1) (operaOct expre2)))]
      [(and(eqv? (car (cdr (string->list expre1))) #\0)
           (eqv? (car (string->list expre2)) #\0))(d->h (pow (operaOctNeg expre1) (operaOct expre2)))])))

;Funcion recursiva para elevar un numero a otro
(define pow
  (lambda (expre1 expre2)
    (cond
      [(= expre2 0)1]
      [else (* expre1 (pow expre1 (- expre2 1)))])))

;Funcion para sacar el modulo entre dos numeros
(define sacar-modulo
  (lambda (expre1 expre2)
    (cond

      ;Elevar decimales y flotante
      [(and (number? expre1) (number? expre2))(modulo expre1 expre2)]

      ;Elevar hexa
      [(and(eqv? (car (string->list expre1)) #\h)
           (eqv? (car (string->list expre2)) #\h))
       (cond
         [(>= (modulo (operaHex expre1) (operaHex expre2)) 0)
          (d->h (modulo (operaHex expre1) (operaHex expre2)))]
         [(< (modulo (operaHex expre1) (operaHex expre2)) 0)
          (string-append "-" (d->h (* -1 (modulo (operaHex expre1) (operaHex expre2)))))])]
      [(and(eqv? (car (cdr (string->list expre1))) #\h)
           (eqv? (car (string->list expre2)) #\h))
       (cond
         [(>= (modulo (operaHexNeg expre1) (operaHex expre2)) 0)
          (d->h (modulo (operaHexNeg expre1) (operaHex expre2)))]
         [(< (modulo (operaHexNeg expre1) (operaHex expre2)) 0)
          (string-append "-" (d->h (* -1 (modulo (operaHexNeg expre1) (operaHex expre2)))))])]
      [(and(eqv? (car (string->list expre1)) #\h)
           (eqv? (car (cdr (string->list expre2))) #\h))
       (cond
         [(>= (modulo (operaHex expre1) (operaHexNeg expre2)) 0)
          (d->h (modulo (operaHex expre1) (operaHexNeg expre2)))]
         [(< (modulo (operaHex expre1) (operaHexNeg expre2)) 0)
          (string-append "-" (d->h (* -1 (modulo (operaHex expre1) (operaHexNeg expre2)))))])]
      [(and(eqv? (car (cdr (string->list expre1))) #\h)
           (eqv? (car (cdr (string->list expre2))) #\h))
       (cond
         [(>= (modulo (operaHexNeg expre1) (operaHexNeg expre2)) 0)
          (d->h (modulo (operaHexNeg expre1) (operaHexNeg expre2)))]
         [(< (modulo (operaHexNeg expre1) (operaHexNeg expre2)) 0)
          (string-append "-" (d->h (* -1 (modulo (operaHexNeg expre1) (operaHexNeg expre2)))))])]

      ;Elevar octales
      [(and(eqv? (car (string->list expre1)) #\0)
           (eqv? (car (string->list expre2)) #\0))(d->h (modulo (operaOct expre1) (operaOct expre2)))]
      [(and(eqv? (car (cdr (string->list expre1))) #\0)
           (eqv? (car (string->list expre2)) #\0))(d->h (modulo (operaOctNeg expre1) (operaOct expre2)))]
      [(and(eqv? (car (string->list expre1)) #\0)
           (eqv? (car (cdr (string->list expre2))) #\0))(d->h (modulo (operaOct expre1) (operaOctNeg expre2)))]
      [(and(eqv? (car (cdr (string->list expre1))) #\0)
           (eqv? (car (cdr (string->list expre2))) #\0))(d->h (modulo (operaOctNeg expre1) (operaOctNeg expre2)))])))

;Funcion para operar las primtivas
(define operar
  (lambda (operador1 signo operador2)
    (cond

      ;Operacion decimal y flotante
      [(and (number? operador1)(number? operador1))(signo operador1 operador2)]

      ;;;;;;;;;;;;;;;;;Operacion Hexa
      [(and(eqv? (car (string->list operador1)) #\h)
           (eqv? (car (string->list operador2)) #\h))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaHex operador1) (operaHex operador2))]
         [(>= (signo (operaHex operador1) (operaHex operador2)) 0)
          (d->h (signo (operaHex operador1) (operaHex operador2)))]
         [(< (signo (operaHex operador1) (operaHex operador2)) 0)
          (string-append "-" (d->h (* -1 (signo (operaHex operador1) (operaHex operador2)))))]
         [else (signo (operaHex operador1) (operaHex operador2))])]
      [(and(eqv? (car (cdr (string->list operador1))) #\h)
           (eqv? (car (string->list operador2)) #\h))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaHexNeg operador1) (operaHex operador2))]
         [(>= (signo (operaHexNeg operador1) (operaHex operador2)) 0)
          (d->h (signo (operaHexNeg operador1) (operaHex operador2)))]
         [(< (signo (operaHexNeg operador1) (operaHex operador2)) 0)
          (string-append "-" (d->h (* -1 (signo (operaHexNeg operador1) (operaHex operador2)))))]
         [else (signo (operaHexNeg operador1) (operaHex operador2))])]
      [(and(eqv? (car (string->list operador1)) #\h)
           (eqv? (car (cdr (string->list operador2))) #\h))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaHex operador1) (operaHexNeg operador2))]
         [(>= (signo (operaHex operador1) (operaHexNeg operador2)) 0)
          (d->h (signo (operaHex operador1) (operaHexNeg operador2)))]
         [(< (signo (operaHex operador1) (operaHexNeg operador2)) 0)
          (string-append "-" (d->h (* -1 (signo (operaHex operador1) (operaHexNeg operador2)))))]
         [else (signo (operaHex operador1) (operaHexNeg operador2))])]
      [(and(eqv? (car (cdr (string->list operador1))) #\h)
           (eqv? (car (cdr (string->list operador2))) #\h))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaHexNeg operador1) (operaHexNeg operador2))]
         [(>= (signo (operaHexNeg operador1) (operaHexNeg operador2)) 0)
          (d->h (signo (operaHexNeg operador1) (operaHexNeg operador2)))]
         [(< (signo (operaHexNeg operador1) (operaHexNeg operador2)) 0)
          (string-append "-" (d->h (* -1 (signo (operaHexNeg operador1) (operaHexNeg operador2)))))]
         [else (signo (operaHexNeg operador1) (operaHexNeg operador2))])]

      ;;;;;;;;;;;;;;;;;Operacion octal
      [(and(eqv? (car (string->list operador1)) #\0)
           (eqv? (car (string->list operador2)) #\0))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaOct operador1) (operaOct operador2))]
         [(>= (signo (operaOct operador1) (operaOct operador2)) 0)
          (d->o (signo (operaOct operador1) (operaOct operador2)))]
         [(< (signo (operaOct operador1) (operaOct operador2)) 0)
          (string-append "-" (d->o (* -1 (signo (operaOct operador1) (operaOct operador2)))))])]
      [(and(eqv? (car (cdr (string->list operador1))) #\0)
           (eqv? (car (string->list operador2)) #\0))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaOctNeg operador1) (operaOct operador2))]
         [(>= (signo (operaOctNeg operador1) (operaOct operador2)) 0)
          (d->o (signo (operaOctNeg operador1) (operaOct operador2)))]
         [(< (signo (operaOctNeg operador1) (operaOct operador2)) 0)
          (string-append "-" (d->o (* -1 (signo (operaOctNeg operador1) (operaOct operador2)))))]
         [else (signo (operaOctNeg operador1) (operaOct operador2))])]
      [(and(eqv? (car (string->list operador1)) #\0)
           (eqv? (car (cdr (string->list operador2))) #\0))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaOct operador1) (operaOctNeg operador2))]
         [(>= (signo (operaOct operador1) (operaOctNeg operador2)) 0)
          (d->o (signo (operaOct operador1) (operaOctNeg operador2)))]
         [(< (signo (operaOct operador1) (operaOctNeg operador2)) 0)
          (string-append "-" (d->o (* -1 (signo (operaOct operador1) (operaOctNeg operador2)))))]
         [else (signo (operaOct operador1) (operaOctNeg operador2))])]
      [(and(eqv? (car (cdr (string->list operador1))) #\0)
           (eqv? (car (cdr (string->list operador2))) #\0))
       (cond
         [(or (eqv? signo <)(eqv? signo >)(eqv? signo >=)(eqv? signo <=)(eqv? signo =))
          (signo (operaOctNeg operador1) (operaOctNeg operador2))]
         [(>= (signo (operaOctNeg operador1) (operaOctNeg operador2)) 0)
          (d->o (signo (operaOctNeg operador1) (operaOctNeg operador2)))]
         [(< (signo (operaOctNeg operador1) (operaOctNeg operador2)) 0)
          (string-append "-" (d->o (* -1 (signo (operaOctNeg operador1) (operaOctNeg operador2)))))]
         [else (signo (operaOctNeg operador1) (operaOctNeg operador2))])])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Ambiente

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)          
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))

;; ambiente de clase
(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;******************** referenciacion

;^;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    1))

;Inicializador de Inter
;(inter)