;+++++++++++++++++++++++
;
;;  Kobras    6658699
;;  Pöhlmann  6663579
;
;+++++++++++++++++++++++
;
;;  Seppke/Gr. 9 - Abgabe 14.12.2015 12:00
;
;+++++++++++++++++++++++

#lang swindle
(require se3-bib/prolog/prologInScheme)

;;;;;;;;; Aufgabe 1: Prolog in Racket
;;;; Aufgabe 1.1: Unifikation

#|
a) unifiziert ?Marke = MacBook ;
              ?Farbe = schwarz.
b) unifiziert nicht weil Gold != Silber
c) unifiziert ?Rang = Gold.
d) angenommen, bei der 2. Eingabe sei eine zusätzliche ")" am Ende, dann würde
              ?andere = ((Petra Pfiffig Gold) (Lena Lustig Silber))
              unifizieren.
e) unifiziert ?farbe = Pik ;
              ?wert = As.
f) unifiziert ?farbe = Pik ;
              ?wert2 = ?wert ;
              ?wert = As.
|#


;;;; Aufgabe 1.2: Anfragen

;; Einlesen der Datenbasis
(<- (ausleihe "P 201" 100))
(<- (ausleihe "P 30" 102))
(<- (ausleihe "P 32" 104))
(<- (ausleihe "P 50" 104))

(<- (vorbestellung "P 201" 104))
(<- (vorbestellung "P 201" 102))
(<- (vorbestellung "P 30" 100))
(<- (vorbestellung "P 30" 104))

(<- (leser Neugierig Nena 100 1989))
(<- (leser Linux Leo 102 1990))
(<- (leser Luator Eva 104 1988))

#|
;1.
;Anfrage:
(?- (vorbestellung "P 201" ?Leser))
;Ausgabe:
;    ?Leser = 104
;    ;
;    ?Leser = 102
;    ;
;    No.

;2.
;Anfrage:
(?- (leser Linux Leo ?LNR ?))
;Antwort:
;    ?LNR = 102
;    ;
;    No.

;3.
;Anfrage:
(?- (vorbestellung "P 30" ?LNR) (leser ?Nachname ?Vorname ?LNR ?))
;Antwort:
;    ?Nachname = Neugierig
;    ?Vorname = Nena
;    ?LNR = 100
;    ;
;    ?Nachname = Luator
;    ?Vorname = Eva
;    ?LNR = 104
;    ;
;    No.

;4.
;Anfrage:
(?- (leser ?Nachname ?Vorname ?LNR ?Geburtsjahr) (test (< ?Geburtsjahr (- 2016 60))) (ausleihe ? ?LNR))
;Antwort:
;    No.

;5.
;Anfrage:
(?- (leser ?Nachname ?Vorname ?LNR ?) (ausleihe ?Buch1 ?LNR) (ausleihe ?Buch2 ?LNR)
    (!= ?Buch1 ?Buch2))
;Antwort:
;    ?Nachname = Luator
;    ?Vorname = Eva
;    ?LNR = 104
;    ?Buch1 = P 32
;    ?Buch2 = P 50
;    ;
;    ?Nachname = Luator
;    ?Vorname = Eva
;    ?LNR = 104
;    ?Buch1 = P 50
;    ?Buch2 = P 32
;    ;
;    No.
|#