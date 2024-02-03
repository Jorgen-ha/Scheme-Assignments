; Gruppemedlemmer; arminka, eirikmar og jorgehf

; Oppgave 1 
; a)

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (begin (set! count (+ count 1))
             count))))

; b) Se "tegninger.pdf"


; Oppgave 2
; a)

(define (make-stack stack)
  (define (push! elements)
    (set! stack (append (reverse elements) stack)))
  (define (pop!)
    (if (not (null? stack))
        (set! stack (cdr stack))))
  (define (dispatch . msg)
    (cond ((eq? (car msg) 'push!) (push! (cdr msg)))
          ((eq? (car msg) 'pop!) (pop!))
          ((eq? (car msg) 'stack) stack)))
  dispatch)

; b)

(define (push! . args)
  (apply (car args) 'push! (cdr args)))
    
(define (pop! stack)
  (stack 'pop!))

(define (stack stck)
  (stck 'stack))



; Oppgave 3

;  a) Se også "tegninger.pdf"
;    (cdr bar) --> (b c d e)
;    (cdddr bar) --> (d e)
;    (cdr (cdddr bar))--> (e)
;    (d) stopper å peke på (e) for å peke på (b c d e) i stedet
;
;    Grunnen til at vi får de verdiene vi får ved kallene på list-ref er at
;    (cdr bar) -> (b c d) blir sirkulær etter set-cdr! kallet. Så etter set-cdr! har vi
;    bar --> (a b c d [b c d] b c d...)
;             0 1 2 3  4 5 6  7 8 9...
;
;  b) Se også "tegninger.pdf"
;    Etter første set-car! kall har vi bah --> ((a towel) a towel)
;    Her peker (car bah) på (cdr bah) og derfor vil begge (a)ene endres
;    til 42 etter andre set-car! kall og vi ender med ((42 towel) 42 towel)
;
; c) 
(define (cycle? list)
  (if (number? list)
      #f
      (not (list? list))))

; d) list? gir false på bar fordi etter definisjonen vår av lister skal de
;    termineres med en tom liste, hvilket bar aldri gjør. Derimot gjør bah det, 
;    og derfor returneres true når vi kaller predikatet på denne 

