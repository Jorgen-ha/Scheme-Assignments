; Gruppemedlemmer; jorgehf, eirikmar og arminka
(load "huffman.scm")

(define foo 42)
; 1a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car x)
  (list-ref (x list) 0))

(define (p-cdr x)
  (list-ref (x list) 1))

;1b)

;1
((lambda (foo x)
   (if(= x foo)
      'same
      'different))
 5 foo)

;2
((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel)

;1c)
(define (infix-eval exp)
  ((list-ref exp 1) (list-ref exp 0) (list-ref exp 2)))

;1d)

#|
{list 84 / 2} vil tolke '/' som en prosedyre mens {'(84 / 2)} tolker det som bare et tegn '/'.
Derfor vil (infix-eval bah) evaluere til en error.
|#

;2a) -  Halerekursiv versjon av decode
;Halerekursiv versjon - legger til hvert element i msg som reverseres før retur
(define (decode bits tree)
  (define (decode-1 bits current-branch msg)
    (if (null? bits)
        (reverse msg)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) msg))
              (decode-1 (cdr bits) next-branch msg)))))
  (decode-1 bits tree '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;2b) - Resultatet av å kalle prosedyren 'decode' fra oppgaven over med 
;      kodetreet og bitkoden gitt som eksempler blir;
(decode sample-code sample-tree)

;2c)  Encode - gitt symboler og tree, returner liste av 0 og 1
(define (encode syms tree)
  (define (encode-1 syms branch msg)
    (if (null? syms)
        (reverse msg)
        (if (leaf? branch)
            (encode-1 (cdr syms) tree msg)
            (if (sym-in-branch? (car syms) (left-branch branch))
                (encode-1 syms (left-branch branch) (cons 0 msg))
                (encode-1 syms (right-branch branch) (cons 1 msg))))))
  (encode-1 syms tree '()))

(define (sym-in-branch? sym branch)
  (if (leaf? branch)
      (if (eq? sym (symbol-leaf  branch))
          #t
          #f)
      (sym-in-list? sym (caddr branch))))

(define (sym-in-list? sym list)
  (if (null? list) 
      #f
      (if (eq? sym (car list))
          #t
          (sym-in-list? sym (cdr list)))))

;2d) grow-huffman-tree:
;    tar en liste av symbol/frekvens-par og returnerer et Huffmantre.                      

(define (grow-huffman-tree set)
  (define (grow-huffman-tree-1 ordSet)
    (if (> (length ordSet) 2)
        (grow-huffman-tree-1
         (adjoin-set (make-code-tree (car ordSet) (cadr ordSet))
                     (cdr (cdr ordSet))))
        ordSet))
  (grow-huffman-tree-1 (make-leaf-set set)))

;2e)
(define items '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
                (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
                (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
                              
(define new-tree (grow-huffman-tree items))

;-Hvor mange bits bruker det på å kode meldingen?
#|
(define message '(ninjas fight ninjas fight ninjas ninjas fight
                         samurais samurais fight samurais fight ninjas ninjas fight by night))
(encode message new-tree)

Det bruker 43 bits på å kode meldingen
|#

;-Hva er den gjennomsnittlige lengden på hvert kodeord som brukes?
#|
ninjas - 010 // fight - 10 // samurais - 11 // by - 0111 // night - 000
Den gjennomsnitlige lengden på hvert kodeord er 3.
|#

;– Til slutt: hva er det minste antall bits man ville trengt for å kode meldingen med en kode med fast lengde
;(fixed-length code) over det samme alfabetet? Begrunn kort svaret ditt.

#|
I alfabetet har vi 16 symboler, dvs at vi må velge en kode med 4 bits per symbol.
Meldingen i oppgaven består av 17 symboler. Da er 17 x 4 = 68 det minste antall bits vi
trenger for å kode meldingen (fixed-length code).
|#


;2f) huffman-leaves - tar et Huffman-tre som input og returnerer en liste
;                     med par av symboler og frekvenser
(define (huffman-leaves tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (if (leaf? left)
        (if (leaf? right)
            (list (list (symbol-leaf left) (weight-leaf left))
                  (list (symbol-leaf right) (weight-leaf right)))
            (append (list (list (symbol-leaf left) (weight-leaf left)))
                    (huffman-leaves right)))
        (if (leaf? right)
            (append (huffman-leaves left)
                    (list (list (symbol-leaf right) (weight-leaf right))))
            (append (huffman-leaves left) (huffman-leaves right))))))