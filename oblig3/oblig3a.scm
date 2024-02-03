; Gruppemedlemmer; arminka, eiriak og jorgehf

(load "prekode3a.scm")

; Oppgave 1 
; a) og b) Done!
; Memoize-prosedyren er tungt inspirert av koden fra exercise 3.27 i SICP.
; Ellers lager vi en tabell for å ta vare på prosedyrene som skal memoizes
; _FØR_ de ble memoizert, så vi enkelt kan gå tilbake til originalprosedyren
; ved bruk av unmemoize. 

(define org-proc-table (make-table))

(define (mem msg proc)
  (let ((org-proc proc))
    (define (memoize proc)
      (let ((table (make-table)))
        (let ((new-proc
               (lambda x
                 (let ((prev-res (lookup x table)))
                   (or prev-res 
                       (let ((res (apply proc x)))
                         (insert! x res table)
                         res))))))
          (insert! new-proc org-proc org-proc-table)
          new-proc)))            
    (define (unmemoize proc)
      (lookup proc org-proc-table))
    (if (eqv? msg 'memoize)
        (memoize proc)
        (if (eqv? msg 'unmemoize)
            (unmemoize proc)))))


(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; c) Det første vi ser er at vi benytter oss av "define" isteden for "set!"
;    når vi ønsker definere mem-fib, men det som utgjør den større forskjellen
;    er at vi ikke lenger "endrer" prosedyren vi kaller på, men legger det til
;    en ny prosedyre. Det dette gjør i praksis er at "fib" i dette tilfellet
;    blir kalt internt i "mem" og "memoize", og det første kallet blir
;    blir memoizert (fib 3). Men deretter kaller jo "fib" rekursivt på seg selv
;    fra inne i "fib", og her er det ingen memoizering siden vi ikke har endret
;    "fib", bare "lagt til" mem-fib. 

;    Resultatet blir at kun første kall på "fib" blir memoizert, og alle de 
;    rekursive kallene blir som ved kjøring av den vanlige "fib"-prosedyren,
;    og ikke memoizert. 


; Oppgave 2
; a)

(define (list-to-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list-to-stream (cdr list)))))

(define (stream-to-list stream . qnty)
  (if (or (null? stream) (and (not (null? qnty)) (eq? (car qnty) 0)))
      '()
      (if (null? qnty)
          (cons (stream-car stream) (stream-to-list (stream-cdr stream)))
          (cons (stream-car stream) (stream-to-list (stream-cdr stream) (- (car qnty) 1))))))

; b)
(define (stream-take n stream)
  (if (eq? n 0)
      the-empty-stream
      (cons-stream (stream-car stream) (stream-take (- n 1) (stream-cdr stream)))))

; c) Et potensielt problem med Petter Smarts forslag er at memq må gå igjennom
;    hele streamen, som gjør at poenget med streams, som er å kun se et element
;    av gangen og "slippe" gå igjennom hele, blir borte. I tillegg har vi 
;    problemet med at streams kan være uendelig lange, og da vil memq aldri 
;    terminere...

; d)
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter (lambda (x) (not (eq? x (stream-car stream)))) stream)))))