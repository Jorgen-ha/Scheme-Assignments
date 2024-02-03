;Oppgave 1 
;a) - e) - se PDF med navn "struktur-tegning.pdf"

;Nedenfor følger koden for å trekke ut 42 fra gitte lister, kun ved bruk av 
;'car' og 'cdr'. Fordi 'bar' må være definert før den kan legges til i listene
;har vi det først.
(define bar 3)

;f) 
(car (cdr (list 0 42 #t bar)))

;g) 
(cdr (car (list '(0 42) '(#t bar))))

;h) 
(car (car (cdr (list '(0) '(42 #t) '(bar)))))

;i) Listen fra oppgave g) var; ((0 42) (#t bar)). 
;Listen kan lages bare ved 'cons' på følgende måte;
(cons '(0 42) (cons '(#t 3) '()))

;og med 'list' på følgende måte;
(list '(0 42) '(#t 3))



; Oppgave 2
; a)

(define (take n items)
    (cond ((< (length items) n) items)
            ((< 0 n) (cons (car items) (take (- n 1) (cdr items))))
        (else '())))

;Funksjonen sjekker først om lengden på lista er lik eller mindre enn n, er den
;returnerer vi lista. Videre sjekker vi om n er større enn 0, hvilket vil føre
;til et kall på 'cons' som lager et par av det første elementet i lista, samt
;det første elementet av alle lister deretter så lenge n er større enn 0, som
;kommer av det rekursive kallet på 'take' med n-1 og 'cdr' av lista. Når enn 
;blir 0, gitt at n ikke er større enn lengden på lista, returnerer vi den tomme
;lista, som også markerer det siste 'cons'-kallet vi trenger for å få den
;endelige lista. 


; b) - halerekursjon-versjon av a)
(define (take n items)
    (if (or (= (length items) n) (< (length items) n))
        items
        (take n (reverse (cdr (reverse items))))))

;I motsetning til i a), hvor vi får en kjede av ventende kall på 'cons' frem
;til vi får den tomme lista, som deretter vil gjøre at alle kall en etter en 
;blir utført, og lenken med par blir satt sammen til en liste, vil vi her med
;halerekursjon oppleve at returverdien til det siste kallet også er den
;endelige lista vår. 


;c) take-while

(define (take-while pred items)
    (if (or (null? items) (not (pred (car items))))
        ('())
        (cons (car items) (take-while pred (cdr items)))))

;'take-while' sjekker først om lista er null eller om predikatet gitt evaluerer
;til 'false' på det første elementet i lista. Dersom en av disse evaluerer til
;'true' vil den tomme lista returneres. Dersom ingen av disse evaluerer til 
;'true' vil vi lage et nytt par av det første elementet i lista, og kalle 
;rekursivt på funksjonen med samme predikat og resten av lista. Til slutt vil
;vi ende opp med en liste som etterspurt, kun bestående av elementer som 
;"oppfyller" predikatet.


;d) map2 
(define (map2 arg items1 items2 )
    (if (or (null? items1) (null? items2))
        '()
        (cons (arg (car items1) (car items2)) 
            (map2 arg (cdr items1) (cdr items2)))))

;Funksjonen sjekker først om noen av listene er null, og dersom det skulle være
;tilfelle returneres den tomme lista, som markerer slutten på lista vår. Ellers
;vil den lage et nytt par bestående av det første elementet i den første lista,
;'arg'-et med det første elementet i liste nummer 2, og det som til slutt blir
;en tom liste ved det rekursive kallet på 'map2' med samme argument, og resten
;av de to listene. 


;e) lambda for å regne snitt 
; Vi kan kombinere map2 med lambda-uttrykket nedenfor for å beregne 
; gjennomsnittet av to tall: 
(lambda (x y) 
    (/ (+ x y) 2)) 

;Som fører til følgende kode:
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))
