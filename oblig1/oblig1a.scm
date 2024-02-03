; Oblig 1a

; Oppgave 1

; a) Uttrykket evaluerer til verdien 30. Dette fordi den ganger sammen summen 
;    av 4 og 2, som blir 6, med 5.


; b) Uttrykket evaluerer til feilen "application: not a procedure; expected 
;    a procedure that can be applied to arguments given: 5". Dette fordi vi 
;    har en parentes foran 5-tallet som impliserer at vi skal sette inn en ny
;    operator som skal utføres på påfølgende argumenter. 5 er derimot kun en 
;    verdi, et passende argument, og ikke en operator. Vi kan ikke utføre 
;    operasjonen "5" på eventuelle argumenter/verdier som gis videre i denne 
;    parentesen (her blir det riktignok ikke gitt noen). 


; c) Uttrykket evaluerer til feilen "application: not a procedure; expected a
;    procedure that can be applied to arguments given: 4". Dette av samme grunn
;    som i b), fordi vi mater REPL'en med en verdi/et argument (4) uten å si 
;    hvilken operasjon som skal utføres på denne først. Det fører til at det 
;    som står videre i parentesen, "+ 2", heller ikke utføres. Det forventes 
;    også her en operator som sier hva som skal gjøres med verdiene/argumentene 
;    som følger. Her er denne puttet mellom 4- og 2-tallet, som resulterer i 
;    feil.


; d) Uttrykket evaluerer til verdien 22. Dette fordi vi definerer variabelen 
;    "bar" til å være 44 delt på 2.
;


; e) Uttrykket evaluerer til verdien 11. Dette fordi vi trekker verdien 11 fra 
;    "bar", som vi vet fra d) har verdien 22.


; f) Uttrykket evaluerer til verdien 12. Dette fordi at vi deler produktet av 
;    "bar" (22), 3, 4 og 1 ganget sammen, på "bar". Altså 264 delt på 22, som
;     blir 12.

; Oppgave 2


; a)
;   1 - Uttrykket evaluerer til "paff!". Dette fordi at det første uttrykket
;       evaluerer til "true". OR fungerer slik at dersom ett av uttrykkene i  
;       listen av uttrykk evaluerer til "true", så evaluerer hele uttrykket til
;       "true". Som vi vet evaluerer alt som ikke er eksplisitt "false" i 
;       Scheme til "true", som altså er tilfelle også for "paff!". 

;   2 - Uttrykket evaluerer til "#f". Dette fordi uttrykket aller først i 
;       listen av uttrykk som skal evaluerers, evaluerer til "false". AND 
;       fungerer slik at dersom et uttrykk evaluerer til "false" returneres 
;       "false", og resten av eventuelle uttrykk etter dette blir ikke 
;       evaluert. Her slår dette til i første test, fordi 1 ikke er lik 2.

;   3 - Uttrykket evaluerer til "poff!". Dette fordi at 42 er et positivt tall.
;       "if" fungerer slik at dersom testen "slår til", altså er det vi sjekker
;       "true", så returneres første alternativ, mens dersom det ikke stemmer 
;       returneres alternativ nr 2. 


; b)
;if-versjon av prosedyren sign.
(define (sign n)
  (if (< n 0)
      (- 1)
      (if (= n 0)
          0
          1)))


;cond-versjon av prosedyren sign
(define (sign n)
  (cond ((> n 0) 1)
        ((= n 0) 0)
        ((< n 0) (- 1))))


; c) AND/OR-versjon av prosedyren sign
(define (sign n)
  (or (and (< n 0) -1)  ;Sjekker om n er mindre enn 0 med AND, hvis "false" går
                        ;videre. Hvis "true" returneres -1 fra funksjonen
      (and (> n 0) +1)  ;Sjekker om n er større enn 0 med AND, hvis "false" går
                        ;videre. Hvis "true" returneres +1 fra funksjonen
      0))               ;Dersom vi er kommet hit er de to "AND"-uttrykkene 
                        ;false, dermed er tallet 0, og dette returneres (fordi 
                        ;alt som ikke er "false" er "true").



; Oppgave 3
; a) 
(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))


; b)
(define (plus n m)
  (if (= m 0)
    n
    (plus(add1 n)(sub1 m))))
  ;Legger til en på n frem til m blir null.



; c) Prosedyren min i b) gir opphav til en iterativ prosess
;    ifølge forelesningene. Dette fordi at det ikke er noen 
;    ventende prosedyrekall vi etterlater oss etter hvert kall,
;    men heller at returverdien av siste iterasjon er returverdien
;    til hele prosedyren. Det gjøres samme operasjon hver iterasjon,
;    som er at det legges til en på n og trekkes fra en på m, 
;    til vi når basistilfelle og bryter ut. 

; Rekursiv prosess
(define (plus n m)
  (if (= m 0)
    n 
    (add1 (plus n (sub1 m))))) 

; I motsetning til den første proseydren for (plus n m) får vi her 
; en kjede med ventende kall på add1, som vokser til m = 0. m blir 0
; når vi har trukket fra 1 mange nok ganger, med sub1-funksjonen, og 
; prosedyren vil avslutte med en rekke av add1-kall som først vil utføres
; når plus-funksjonen returnerer n. Antallet ganger vi trakk fra 1 fra m 
; før vi fikk 0 er det samme antallet add1-kall som venter på å bli utført.



; d) 
; Original
(define (power-close-to b n)  
  (power-iter b n 1))

(define (power-iter b n e)
  (if (> (expt b e) n)
    e 
    (power-iter b n (+ 1 e))))
; ------------------------ ;

; Blokkstruktur
(define (power-close-to b n)  
  (define (power-iter e)
    (if (> (expt b e) n)
      e 
      (power-iter (+ 1 e))))
    (power-iter 1))
; ------------------------ ;

; I prosedyrene ovenfor har vi skrevet om så vi nå bruker blokkstruktur. 
; Dette medfører også at vi kan forenkle den interne prosedyren, fra å ta 
; 3 argumenter, til å nå kun ta ett. Dette kan vi gjøre fordi at de 2 andre
; argumentene får vi i "forelder-prosedyren", og disse kan man bruke direkte 
; også i den interne prosedyren (som kan ses på som underordnet 
; hoved-/forelederprosedyren). 



; e) Det er ikke mulig å forenkle den interne definisjonen av fib-iter når man 
;    bruker blokkstruktur. Dette fordi at argumentene som skal brukes i den 
;    interne prosedyren ikke allerede tas inn i forelderprosedyren (med unntak
;    av n som brukes som count). Vi kunne tenkt oss at count er et overflødig
;    argument i den interne prosessen, fordi vi benytter n som vår counter når 
;    vi kaller på den interne prosedyren, men dersom vi fjerner dette 
;    argumentet får vi vanskeligheter med å trekke fra 1 fra dette tallet ved  
;    hver iterasjon.
