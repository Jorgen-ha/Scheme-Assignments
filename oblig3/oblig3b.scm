; Oblig 3b - Gruppemedlemmer; eiriak, arminka og jorgehf

(load "evaluator.scm")

; Oppgave 1 
; a)

(define exp1
  '(define (foo cond else)
     (cond ((= cond 2) 0)
           (else (else cond)))))

(set! the-global-environment (setup-environment))
; Evaluerer alle uttrykkene
(mc-eval exp1 the-global-environment)
(mc-eval '(define cond 3) the-global-environment)
(mc-eval '(define (else x) (/ x 2)) the-global-environment)
(mc-eval '(define (square x) (* x x)) the-global-environment)



#|
-Uttrykk (foo 2 square) returnerer 0 fordi vi setter argument "cond" som 2 og da returnerer den 0.
-Uttrykk (foo 4 square) returnerer 16 fordi argumentet ikke er 2 og tallet 4 blir videresendt til "square" uttrykket
som beregner 4 * 4 og returnerer 16.
-Siste uttrykk returnerer 2 fordi vi starter med "cond" = 3, siden tallet ikke 2 blir tall 4 videresendt til
(define (else x) (/ x 2)) som regner ut 4/2, ergo blir 2 returnert.
|#




; Oppgave 2 
; a)
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        ;;      her kan vi legge til flere primitiver.
        (list '1+ 
              (lambda (x) (+ x 1)))
        (list '1- 
              (lambda (x) (- x 1)))
        ))

; Test
(set! the-global-environment (setup-environment))
(display "Oppgave 2a) - demo")
(newline)
(display "Resultat av (1+ 4); ")
(mc-eval '(1+ 4) the-global-environment) ; Skal evaluere til 5
(display "Resultat av (1- 4); ")
(mc-eval '(1- 4) the-global-environment) ; Skal evaluere til 3

; b) 
(define (install-primitive! name procedure)
  (define-variable! name (list 'primitive procedure)
    the-global-environment))

; Test
(newline)
(display "Oppgave 2b) - demo")
(newline)
(install-primitive! 'square (lambda (x) (* x x)))
(display "Resultat av (square 4); ")
(mc-eval '(square 4) the-global-environment) ; Skal evaluere til 16


; Oppgave 3 
; a) 

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ; her legger vi til flere special form evalueringer
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))))

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ; her legger vi til flere special forms
        ((and? exp) #t)
        ((or? exp) #t)
        (else #f)))


;eval-and - evaluerer and-uttrykk
(define (eval-and exp env)
  (if (false? (mc-eval (cadr exp) env))
      #f
      (if (null? (cddr exp))
          (mc-eval (cadr exp) env)
          (eval-and (cons (car exp) (cddr exp)) env))))

;and? - sjekker om vi har et and-uttrykk
(define (and? exp)
  (tagged-list? exp 'and))




;eval-or-prosedyre - evaluerer or-uttrykk
(define (eval-or exp env)
  (if (true? (mc-eval (cadr exp) env))
      (mc-eval (cadr exp) env)
      (if (null? (cddr exp)) 
          #f
          (eval-or (cons (car exp) (cddr exp)) env))))

;or? - sjekker om vi har et or-uttrykk
(define (or? exp)
  (tagged-list? exp 'or))


(newline)
(display "Oppgave 3a) - demo")
(newline)
(display "Resultat av (and 1 2 3); ")
(mc-eval '(and 1 2 3) the-global-environment)
(display "Resultat av (and 1 #f 3); ")
(mc-eval '(and 1 #f 3) the-global-environment)
(newline)
(display "Resultat av (or 1 2 3); ")
(mc-eval '(or 1 2 3) the-global-environment)
(display "Resultat av (or #f #f 3); ")
(mc-eval '(or #f #f 3) the-global-environment)


; b)

;eval-if - evaluerer if-uttrykk
(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env) 
      (if (equal? 'else (car (cddddr exp)))
          (mc-eval (if-alternative exp) env) 
          (eval-if (cddddr exp) env))))
             
(define (make-if predicate consequent alternative)
  (list 'if predicate 'then consequent alternative))
  
;if? - sjekker om det er et if-uttrykk
;(define (if? exp) (tagged-list? (cddr exp) 'if))

;if-consequent - kjører hvis if-testen er true
(define (if-consequent exp) (cadddr exp))

;if-alternative - kjører hvis else skal kjøres
(define (if-alternative exp)
  (cadr (cddddr exp)))

(newline)
(display "Oppgave 3b) - demo")
(newline)

(define if-expr
  '(if (equal? "Joergen" "Joerge")
       then (display "test 1")
       elsif (equal? "x" "y")
       then (display "test 2")
       elsif (equal? "scheme" "python")
       then (display "test 3")
       else (display "test 4")))

(display "Resultat av if-test; ")
(mc-eval if-expr the-global-environment)
(newline)

; c)

(define (let? exp) (tagged-list? exp 'let))

; endring eval-sepcial-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((let? exp) (eval-let exp env)))) ; legger til støtte for let-utrykk

; endring i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((let? exp) #t) ; legger til støtte for let-utrykk
        (else #f)))

(define (eval-let exp env) (mc-eval (let->lambda exp env) env))

(define (let->lambda exp env)
  (let ((varn (map car (cadr exp)))
        (expn (map cadr (cadr exp)))
        (body (cddr exp)))        
  (append (list (append (list 'lambda varn) body)) expn)))

; d)

;(define (let->lambda exp env)
;  (let ((varn (map car (extract-varn-and-expn exp)))
;        (expn (map cadr (extract-varn-and-expn exp)))
;        (body (cdr (member 'in exp))))
;    (append (list (append (list 'lambda varn) body)) expn)))

;hjelpefunksjon; henter ut varn og expn
(define (extract-varn-and-expn exp)
  (define (extract-varn-and-expn-1 exp l)
    (let ((a (member 'and exp))
          (i (member 'in exp)))
      (cond (a (extract-varn-and-expn-1 (cdr a) (append l (list (list (car exp) (caddr exp))))))
            (i (append l (list (list (car exp) (caddr exp))))))))
  (extract-varn-and-expn-1 (cdr exp) '()))
      