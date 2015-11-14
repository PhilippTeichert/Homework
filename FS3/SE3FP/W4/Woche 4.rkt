#|
;++++++++++++++++++++++
;
;; Kobras    6658699
;; Pöhlmann  6663579
;
;++++++++++++++++++++++
|#


#lang racket

; klappt nicht, deshalb unten einfach nochmal der gesamte benötigte Code
;(require (lib (module m (string->path (string-append (path->string (current-directory)) "Woche_3.rkt")))))

#|
Aufgabe 1

1. 0
min und max verhindern nicht die Auswerung
2. '(+ (- 2 13) 11)
' verhindert die Auswertung
3. 'Jahre
das car vom cdr, ' verhindert die Auswertung
4. '()

5. 'Weihnachtsfest

6. '(Listen ganz einfach und)
cons packt die elemente einfach in ein pair, ein pair mit der leeren liste im innersten pair ist eine Liste
7. '(Paare . auch)
keine leere Liste im innersten pair => keine Liste
8. #t
das ' verhindert die Auswertung, deshalb werden die Symbole in die Liste geschrieben
9. #f
equal? Gleichheit der Werte (auch Funktionen)
eqv? Char & Int Gleichheit
eq? Identität
|#



#|
Aufgabe 2

1.
<Notruf> ::= <Überschrift> <Standortangabe> <Notfallart> <Zusatzinformationen> <Peilphrase>
             <Peilzeichen> <Peilzeichen> <Unterschrift> OVER
<Überschrift> ::= <Notzeichen> <Notzeichen> <Notzeichen> "HIER IST" <Schiffsname>
                  <Schiffsname> <Schiffsname> <Rufzeichen buchstabiert> <Notzeichen> <Schiffsname>
                  <Schiffsname buchstabiert> <Rufzeichen buchstabiert>
<Notzeichen> ::= "MAYDAY"
<Schiffsname> ::= {<Buchstabe>}
<Buchstabe> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" |
                "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
<Rufzeichen buchstabiert> ::= <Phonetikzeichen> <Phonetikzeichen> <Phonetikzeichen> <Phonetikzeichen>
<Phonetikzeichen> ::= "ALFA" | "BRAVO" | "CHARLIE" | "DELTA" | "ECHO" | "FOXTROTT" |
                      "GOLF" | "HOTEL" | "INDIA" | "JULIETT" | "KILO" | "LIMA" |
                      "MIKE" | "NOVEMBER" | "OSCAR" | "PAPA" | "QUEBECK" | "ROMEO" |
                      "SIERRA" | "TANGO" | "UNIFORM" | "VIKTOR" | "WHISKEY" |
                      "X-RAY" | "YANKEE" | "ZULU"
<Schiffsname buchstabiert> ::= {<Phonetikzeichen>}
<Standortangabe> ::= <Ort> <Zeit>
<Ort> ::= {<Buchstabe> | <Ziffer>} | {<Wort>}
<Wort> ::= {<Buchstabe>}
<Zeit> ::= <Ziffer> <Ziffer> <Ziffer> <Ziffer> <Buchstabe> <Buchstabe> <Buchstabe>
<Ziffer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<Notfallart> ::= {<Buchstabe>|<Ziffer>}
<Zusatzinformationen> ::= {<Buchstabe>|<Ziffer>}
<Peilphrase> ::= "ICH SENDE DEN TRAEGER"
<Peilzeichen> ::= "-"
<Unterschrift> ::= <Schiffsname> <Rufzeichen buchstabiert>

2.
|#

; Hilfsfunktion für den Zeilenumbruch
(define zeilenumbruch
  " \n ")
(define zeilenumbruch2
  (newline (current-output-port)))

; Hilfsfunktion für das Notzeichen
(define Notzeichen
  "MAYDAY")

; Hilfsfunktion für ein Leerzeichen zwischen einzelnen Wörtern
(define leerzeichen
  " ")

; Hilfsfunktion für ein Peilzeichen
(define Peilzeichen
  "-")

; Funktion zur Generierung einer Notmeldung
; Parameter (alle als Strings):
;     Schiffsname               Der Name der Hilfe benötigenden Schiffs
;     Rufzeichen                Das Rufzeichen des Hilfe benötigenden Schiffs
;     Position                  Die Position des Hilfe benötigenden Schiffs
;     Zeit                      Die aktuelle Zeit beim Absetzen des Hilferufes
;     Notfallart                Die Art des Notfalls (Zeilenumbrüche mit " \n ")
;     Zusatzinformationen       zusätzliche Angaben zur Art der benötigkten Hilfe (Zeilenumbrüche mit " \n ")
(define (Notmeldungsgenerator Schiffsname Rufzeichen Position Zeit Notfallart [Zusatzinformationen ""])
(let([Schiffsname (toUpperCase Schiffsname)]
[Rufzeichen (toUpperCase Rufzeichen)]
[Position (toUpperCase Position)]
[Zeit (toUpperCase Zeit)]
[Notfallart (toUpperCase Notfallart)]
[Zusatzinformationen (toUpperCase Zusatzinformationen)])
(display(string-append
        (Ueberschrift Schiffsname Rufzeichen)
        zeilenumbruch
        (Standortangabe Position Zeit)
        zeilenumbruch
        Notfallart
        (cond [(> (string-length Zusatzinformationen) 0)
        (string-append
         zeilenumbruch
         Zusatzinformationen)])
        zeilenumbruch
        "ICH SENDE DEN TRAEGER"
        leerzeichen
        Peilzeichen
        Peilzeichen
        zeilenumbruch
        (Unterschrift Schiffsname Rufzeichen)
        zeilenumbruch
        "OVER"))))

; Hilfsfunktion für die Generierung einer Überschrift
; Parameter (alle als Strings):
;     Schiffsname  Der Name der Hilfe benötigenden Schiffs
;     Rufzeichen   Das Rufzeichen des Hilfe benötigenden Schiffs
(define (Ueberschrift Schiffsname Rufzeichen)
    (string-append
        Notzeichen
        leerzeichen
        Notzeichen
        leerzeichen
        Notzeichen
     zeilenumbruch
        "HIER IST"
     zeilenumbruch
    (string-append*
        Schiffsname
        leerzeichen
        Schiffsname
        leerzeichen
        Schiffsname
        leerzeichen
        (textToPhonetikAlles Rufzeichen))
     zeilenumbruch
    (string-append*
        Notzeichen
        leerzeichen
        Schiffsname
        leerzeichen
        "ICH BUCHSTABIERE"
        leerzeichen
        (textToPhonetikAlles Schiffsname))
     zeilenumbruch
    (string-append*
        "RUFZEICHEN"
        leerzeichen
        (textToPhonetikAlles Rufzeichen))))

; Hilfsfunktion für die Generierung einer Standortangabe
; Parameter (alle als Strings):
;     Position     Die Position des Hilfe benötigenden Schiffs
;     Zeit         Die aktuelle Zeit beim Absetzen des Hilferufes
(define (Standortangabe Position Zeit)
  (string-append
   "NOTFALLPOSITION"
        leerzeichen
        Position
        zeilenumbruch
        "NOTFALLZEIT"
        leerzeichen
        Zeit))

; Hilfsfunktion für die Generierung einer Unterschrift
; Parameter (alle als Strings):
;     Schiffsname  Der Name der Hilfe benötigenden Schiffs
;     Rufzeichen   Das Rufzeichen des Hilfe benötigenden Schiffs
(define (Unterschrift Schiffsname Rufzeichen)
  (string-append*
   Schiffsname
   leerzeichen
   (textToPhonetikAlles Rufzeichen)))
   
#|
3.
Für Notruf der Babette bitte eingeben:
(Notmeldungsgenerator "babette" "dejy" "ungefaehr 10 sm nordoestlich leuchtturm kiel" "1000 utc" "schwerer wassereinbruch wir sinken" "keine verletzten \n vier mann gehen in die rettungsinsel \n schnelle hilfe erforderlich")

Für Notruf der Amira bitte eingeben:
(Notmeldungsgenerator "amira" "amry" "53°56'N, 006°31'O" "1640 utc" "in schwerer see gekentert" "15 mann an bord \n das schiff ist 15 meter lang \n roter rumpf")
|#


#|
Aufgabe 3

a)
Innere Reduktion:
Die Terme werden von innen nach außen reduziert.

(hoch3 (* 3 (+ 1 (hoch3 2))))
->  (hoch3 (* 3 (+ 1 (* 2 2 2))))   ;(hoch3)
->  (hoch3 (* 3 (+ 1 8)))           ;(*)
->  (hoch3 (* 3 9))                 ;(+)
->  (hoch3 27)                      ;(+)
->  (* 27 27 27)                    ;(hoch3)
->  19683                           ;(*)

Äußere Reduktion:
Die Terme werden von außen nach innen reduziert.

(hoch3 (* 3 (+ 1 (hoch3 2))))
->  (* (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))   ;(hoch3)
->  (* (* 3 (+ 1 (* 2 2 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))   ;(hoch3)
->  (* (* 3 (+ 1 8)) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))           ;(*)
->  (* (* 3 9) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))                 ;(+)
->  (* 27 (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))                      ;(*)
->  (* 27 (* 3 (+ 1 (* 2 2 2))) (* 3 (+ 1 (hoch3 2))))                      ;(hoch3)
->  (* 27 (* 3 (+ 1 8)) (* 3 (+ 1 (hoch3 2))))                              ;(*)
->  (* 27 (* 3 9) (* 3 (+ 1 (hoch3 2))))                                    ;(+)
->  (* 27 27 (* 3 (+ 1 (hoch3 2))))                                         ;(*)
->  (* 27 27 (* 3 (+ 1 (* 2 2 2))))                                         ;(hoch3)
->  (* 27 27 (* 3 (+ 1 8)))                                                 ;(*)
->  (* 27 27 (* 3 9))                                                       ;(+)
->  (* 27 27 27)                                                            ;(*)
->  19683                                                                   ;(*)


b)
Racket verwendet für normale Funktionen die innere Reduktion.
Bei Spezialformen wird die äußere angewandt.

c)
Die Funktion landet in einer Endlosschleife.
Wendet man das cond jedoch mit den Werten an, die zu dieser Zeit in der Funktion stehen:
(new-if (> 6 5)
        120
        ("fail")
so erhält man das erwartete Ergebnis (nämlich 120).
Doch sobald man folgendes eingibt:
(new-if (> 6 5)
        120
        (faculty (* 6 120) (+ 6 1) 5))
landet man wieder in der Endlosschleife.

Dies liegt daran, dass cond sich - im Gegensatz zu if - der inneren Rekursion bedient.
So wird jedes mal, beim Betreten des new-if erst alles ausgewertet, bevor das Ergebnis ausgegeben wird.
Jedoch wird in dem else Teil eine Rekursion erzeugt und so kommt Racket nie ans Ende und rechnet immer weiter.

Um solche Endlosschleifen zu vermeiden, sind Spezialfunktionen - wie if eine ist - unerlässlich.

|#

(define (new-if condition? then-clause else-clause)
  (cond (condition? then-clause)
        (else else-clause)))

(define (faculty product counter max-count)
  (new-if (> counter max-count)
          product
          (faculty (* counter product)
                   (+ counter 1)
                   max-count)))















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Import Woche 3 (mit diversen Änderungen...)
;;;; Nebendefinitionen von Variablen
; Offset, um zwischen Uppercase und Lowercase Charakteren in der ASCII Tabelle zu wechseln
(define char-offset (- (char->integer #\a) (char->integer #\A)))
; untere Grenze der lowercase Chars in der ASCII Tabelle
(define char-lowercase-lower-border (- (char->integer #\a) 1))
; obere Grenze der lowercase Chars in der ASCII Tabelle
(define char-lowercase-upper-border (+ (char->integer #\z) 1))

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
  '((#\A "ALPHA") (#\B "BRAVO") (#\C "CHARLIE") (#\D "DELTA") (#\E "ECHO") (#\F "FOXTROTT")
                (#\G "GOLF") (#\H "HOTEL") (#\I "INDIA") (#\J "JULIETT") (#\K "KILO")
                (#\L "LIMA") (#\M "MIKE") (#\N "NOVEMBER") (#\O "OSCAR") (#\P "PAPA")
                (#\Q "QUEBECK") (#\R "ROMEO") (#\S "SIERRA") (#\T "TANGO") (#\U "UNIFORM")
                (#\V "VIKTOR") (#\W "EHISKEY") (#\X "X-RAY") (#\Y "YANKEE") (#\Z "ZULU")
                (#\0 "NADAZERO") (#\1 "UNOONE") (#\2 "BISSOTWO") (#\3 "TERRATHREE")
                (#\4 "KARTEFOUR") (#\5 "PENTAFIVE") (#\6 "SOXISIX") (#\7 "SETTESEVEN")
                (#\8 "OKTOEIGHT") (#\9 "NOVENINE") (#\, "DECIMAL") (#\. "STOP")))

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


; Der Eingabetext darf alle Symbole enthalten.
; Die Ausgabe beinhaltet Flaggen und Symbole.
; Die Eingabe muss als String erfolgen.

(define (textToPhonetikAlles string)
    (stringIteriererPhonetikAlles 0 (string->list string) '())
) ; end func

(define (stringIteriererPhonetikAlles index eingabeliste ausgabeliste)
  (let ([ausgabeliste
         (append ausgabeliste
               (append (list (getPhonetikSymbolAlles (charToUpperCase (list-ref eingabeliste index)))) '(" ")))])
  (if (< index (- (length eingabeliste) 1))
      (stringIteriererPhonetikAlles (+ 1 index) eingabeliste ausgabeliste)
      (letztes_element_weg ausgabeliste))
    ) ; end let
) ; end func

(define (getPhonetikPaarAlles char)
  (findePhonetikInListeAlles 0 char))

(define (getPhonetikSymbolAlles char)
  (list-ref (getPhonetikPaarAlles char) 1))

;
(define (findePhonetikInListeAlles index char)
  (if (equal? char (car (list-ref nato-phonetik index)))
      (list-ref nato-phonetik index)
      (if (< index (- (length nato-phonetik) 1)) ;wenn index in Liste
          (findePhonetikInListeAlles (+ 1 index) char);else
          (list 1 char))))
; Die letzte Zeile erstellt eine Liste, mit dem Eingabesymbol als cdr, falls
; kein Eintrag zu ihm vor liegt, damit es selber wieder ausgegeben
; wird (s. Methode getFlaggenSymboleAlles).

; Löscht das letzte Element eiener Liste
; Die Liste
(define (letztes_element_weg list)
  (reverse (cdr (reverse list))))

; Schreibt alle Elemente eines Strings groß
; Der String
(define (toUpperCase string)
  (list->string (map charToUpperCase (string->list string))))