#|
;++++++++++++++++++++++
;
;; Kobras    6658699
;; Pöhlmann  6663579
;
;++++++++++++++++++++++
|#


#lang racket

(require se3-bib/flaggen-module)

;;;; Nebendefinitionen von Variablen
; Offset, um zwischen Uppercase und Lowercase Charakteren in der ASCII Tabelle zu wechseln
(define char-offset 32)
; untere Grenze der lowercase Chars in der ASCII Tabelle
(define char-lowercase-lower-border 96)
; obere Grenze der lowercase Chars in der ASCII Tabelle
(define char-lowercase-upper-border 123)

;;;; Aufgabe 1.1
; Liste, die jedem Uppercase char, jeder Ziffer, dem Komma und dem Punkt den phonetischen Wert zuweißt
; Reihenfolge: A-Z 0-9 , .
; Buchstaben Indizes 0-25; Zahlen Indizes 26-35; Komma Index 36; Punkt Index 37
#|
    Erklärung: Speicherung von Char und Phonetik-Symbol als Paare
        Zugriff auf einen Char: car list-ref this index
        Zugriff auf ein Symbol: cdr list-ref this index
        Index-Referenz: Siehe oben
    Begründung: schneller Zugriff auf Zuweisungspaare
        Zugriff auf Symbol oder char mit minimal wenig Mehraufwand (car/cdr)
|#
(define nato-phonetik
  '((#\A Alpha) (#\B Bravo) (#\C Charlie) (#\D Delta) (#\E Echo) (#\F Foxtrott)
                (#\G Golf) (#\H Hotel) (#\I India) (#\J Juliett) (#\K Kilo)
                (#\L Lima) (#\M Mike) (#\N November) (#\O Oscar) (#\P Papa)
                (#\Q Quebec) (#\R Romeo) (#\S Sierra) (#\T Tango) (#\U Uniform)
                (#\V Viktor) (#\W Whiskey) (#\X X-ray) (#\Y Yankee) (#\Z Zulu)
                (#\0 Nadazero) (#\1 Unaone) (#\2 Bissotwo) (#\3 Terrathree)
                (#\4 Kartefour) (#\5 Pentafive) (#\6 Soxisix) (#\7 Setteseven)
                (#\8 Oktoeight) (#\9 Novenine) (#\, Decimal) (#\. Stop)))

;;;; Aufgabe 1.2
; 
(define (getPhonetikPaar char)
  (findeSymbolInListe 0 char))

(define (getPhonetikSymbol char)
  (cdr (getPhonetikPaar char)))

;
(define (findeSymbolInListe index char)
  (if (equal? char (car (list-ref nato-phonetik index)))
      (list-ref nato-phonetik index)
      (if (< index 37) ;wenn index in Liste
          (findeSymbolInListe (+ 1 index) char);else
          (error "Uncaught Exception: IndexOutOfBoundsException"))))



;;;; Aufgabe 1.3 (Zusatzaufgabe)
; Wandelt einen lowercase alphabetic char in einen uppercase char um
; param char: der zu konvertierende char
; return: das uppercase pendant zum input, wenn input in [a-z], sonst den input
(define (charToUpperCase char)
  (if (char-isLowerCase char)
      (integer->char (- (char->integer char) char-offset))
      char
      ) ; end if
  ) ; end func

;;; Hilfsfunktion für 1.3
; param char: ein Char, der geprüft werden soll, ob er in [a-z] ist
; return: true, wenn char in [a-z]
(define (char-isLowerCase char)
  (and (< (char->integer char) char-lowercase-upper-border)
       (> (char->integer char) char-lowercase-lower-border)
       ) ; end and
  ) ; end func


;;;; Aufgabe 1.4
; Der Eingabetext darf nur die Symbole A-Z a-z 0-9 , . enthalten.

(define (textToPhonetik string)
    (stringIterierer 0 (string->list string) '())
) ; end func

(define (stringIterierer index eingabeliste ausgabeliste)
  (let ([ausgabeliste
         (append ausgabeliste
               (getPhonetikSymbol (charToUpperCase (list-ref eingabeliste index))))])
  (if (< index (- (length eingabeliste) 1))
      (stringIterierer (+ 1 index) eingabeliste ausgabeliste)
      ausgabeliste)
    ) ; end let
) ; end func



;;;;; AUfgabe 2.1
; Reihenfolge: A-Z 0-9
; Buchstaben Indizes 0-25; Zahlen Indizes 26-35
#|
    Erklärung: Speicherung von Char und Flagge als Paare
        Zugriff auf einen Char: car list-ref this index
        Zugriff auf ein Symbol: cdr list-ref this index
        Index-Referenz: Siehe oben
    Begründung: schneller Zugriff auf Zuweisungspaare
        Zugriff auf Symbol oder char mit minimal wenig Mehraufwand (car/cdr)
|#
(define flaggenAlphabet
  '((#\A A) (#\B B) (#\C C) (#\D D) (#\E E) (#\F F)
                (#\G G) (#\H H) (#\I I) (#\J J) (#\K K)
                (#\L L) (#\M M) (#\N N) (#\O O) (#\P P)
                (#\Q Q) (#\R R) (#\S S) (#\T T) (#\U U)
                (#\V V) (#\W W) (#\X X) (#\Y Y) (#\Z Z)
                (#\0 Z0) (#\1 Z1) (#\2 Z2) (#\3 Z3) (#\4 Z4)
                (#\5 Z5) (#\6 Z6) (#\7 Z7) (#\8 Z8) (#\9 Z9)))


;;;; Aufgabe 2.2
; Eingabe: Ein Charakter aus: A-Z 0-9
; Ausgabe: eine Liste aus Symbol und Flagge
; Diese Methode gibt zu einem Symbol die entsprechende Unterliste der Liste aus Flagge udn Symbol aus aus.
(define (getFlaggenPaar char)
  (findeFlaggeInListe 0 char))

; Eingabe: Ein Charakter aus: A-Z 0-9
; Ausgabe: die entsprechende Flagge
; Diese Methode gibt zu einem Symbol die entsprechende Flagge aus.
(define (getFlaggenSymbol char)
  (eval (list-ref (getFlaggenPaar char) 1)))

; Findet ein Symbol in der Liste der Flaggen. (der rekursive Teil)
; index: der index der Liste, an dem gesucht wird
; char: der char nach dem in der Liste gesucht wird
(define (findeFlaggeInListe index char)
  (if (equal? char (car (list-ref flaggenAlphabet index)))
      (list-ref flaggenAlphabet index)
      (if (< index 35) ;wenn index in Liste
          (findeFlaggeInListe (+ 1 index) char);else
          (error "Uncaught Exception: IndexOutOfBoundsException"))))


;;;; Aufgabe 2.3
; Der Eingabetext darf nur die Symbole A-Z a-z 0-9 enthalten.
; Die Ausgabe beinhaltet nur Flaggen.
; Die Eingabe muss als String erfolgen.

(define (textToFlagge string)
    (stringIteriererFlaggen 0 (string->list string) '())
) ; end func

(define (stringIteriererFlaggen index eingabeliste ausgabeliste)
  (let ([ausgabeliste
         (append ausgabeliste
               (list (getFlaggenSymbol (charToUpperCase (list-ref eingabeliste index)))))])
  (if (< index (- (length eingabeliste) 1))
      (stringIteriererFlaggen (+ 1 index) eingabeliste ausgabeliste)
      ausgabeliste)
    ) ; end let
) ; end func


; Der Eingabetext darf alle Symbole enthalten.
; Die Ausgabe beinhaltet Flaggen und Symbole.
; Die Eingabe muss als String erfolgen.

(define (textToFlaggeAlles string)
    (stringIteriererFlaggenAlles 0 (string->list string) '())
) ; end func

(define (stringIteriererFlaggenAlles index eingabeliste ausgabeliste)
  (let ([ausgabeliste
         (append ausgabeliste
               (list (getFlaggenSymbolAlles (charToUpperCase (list-ref eingabeliste index)))))])
  (if (< index (- (length eingabeliste) 1))
      (stringIteriererFlaggenAlles (+ 1 index) eingabeliste ausgabeliste)
      ausgabeliste)
    ) ; end let
) ; end func

(define (getFlaggenPaarAlles char)
  (findeFlaggeInListeAlles 0 char))

(define (getFlaggenSymbolAlles char)
  (eval (list-ref (getFlaggenPaarAlles char) 1)))

;
(define (findeFlaggeInListeAlles index char)
  (if (equal? char (car (list-ref flaggenAlphabet index)))
      (list-ref flaggenAlphabet index)
      (if (< index 35) ;wenn index in Liste
          (findeFlaggeInListeAlles (+ 1 index) char);else
          (list 1 char)))); erstellt eine Liste, mit dem Eingabesymbol als cdr, falls kein Eintrag zu ihm vor liegt, damit es selber wieder ausgegeben wird (s. Methode getFalggenSymboleAlles).