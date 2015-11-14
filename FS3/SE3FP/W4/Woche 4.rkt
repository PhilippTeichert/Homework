#lang racket

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
<Notruf> ::= <Überschrift> <Standortangabe> <Notfallart> <Zusatzinformationen> <Peilzeichen> <Peilzeichen> <Unterschrift> OVER
<Überschrift> ::= <Notzeichen> <Notzeichen> <Notzeichen> HIER IST <Schiffsname> <Schiffsname> <Schiffsname> <Rufzeichen buchstabiert> </head>
<Notzeichen> ::= MAYDAY
</head> ::= <Notzeichen> <Schiffsname> <Schiffsname buchstabiert> <Rufzeichen buchstabiert>

2.
|#

; Hilfsfunktion für den Zeilenumbruch
(define zeilenumbruch
  (newline (current-output-port)))

; Funktion zur Generierung einer Notmeldung
; Parameter (alle als Strings):
;     Schiffsname  Der Name der Hilfe benötigenden Schiffs
;     Rufzeichen   Das Rufzeichen des Hilfe benötigenden Schiffs
;     Position     Die Position des Hilfe benötigenden Schiffs
;     Notfallart   Die Art des Notfalls und zusätzliche Angaben zur Art der benötigkten Hilfe
(define (Notmeldungsgenerator Schiffsname Rufzeichen Position Notfallart)
  (string-append Ueberschrift
                 )