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

#lang racket
(require "setkarten-module.rkt")

;;;;;;;; Aufgabe 1
;;;; Aufgabe 1.1
#|
Eine Funktion ist dann eine Funktion höherer Ordnung,
wenn sie eine andere Funktion als Parameter übernimmt
|#
;;;; Aufgabe 1.2
#|
a: ja, weil sie eine übergebene zweistellige Relation auf eine Liste anwendet,
   wobei das Zwischenergebnis jeweils als linker Teil der Relation gewertet wird
b: nein, weil sie nur eine Zahl übernimmt und keine Funktion
c: ja, weil die übergebene Funktion 'f' auf arg1 und arg2 angewendet wird
d: nein, weil sie nur eine Zahl übernimmt und keine Funktion
|#
;;;; Aufgabe 1.3
#|
f wird mit max belegt;
arg1 wird mit 5 belegt;
arg2 wird mit 7 belegt.

|#
;;;; Aufgabe 1.4
(foldl (curry / 2) 1 '(1 1 2 3))
(map cons '(1 2 3 4) '(1 2 3 4))
(filter pair? '((a b ) () 1 (())))
(map (compose (curry + 32) (curry * 1.8))
     '(5505 100 0 1 1000 -273.15))
#|
1.  2/3
    Begründung: #keineAhnung, hätte 1/3 erwartet.

2.  '((1 . 1) (2 . 2) (3 . 3) (4 . 4))
    Begründung: map wendet cons jeweils auf die beiden Listenelemente
                mit dem gleichen Index an
3.  '((a b) (()))
    Begründung: (a b) als Liste mit zwei Elementen ist ein Pair.
                Die Leere liste hat auch den Typ Pair.

4. '(9941.0 212.0 32 33.8 1832.0 -459.66999999999996)
   Begründung: jeder einzelne übergebene Wert wird in einen anderen Wert umgerechnet
               diese umgerechneten Werte werden wieder als Liste ausgegeben
               compose fügt die Parameter zu einem arithmetischen Ausdruck zusammen

|#

;;;; Aufgabe 1.5 (Zusatzaufgabe)
#|
Konvertierung erfolgt °C -> °F mit °F = °C * 1.8 + 32.
Die Umkehrung findet statt durch °C = (°F - 32) * 5/9 (Quelle: http://www.celsius-fahrenheit.de/)
|#
;; Wendet obige Formel auf eine Liste von Temperaturen an.
;; @param lst Eine Reihe von Fahrenheit-Werten, die Umgerechnet werden sollen
(define (fahrenheit->celsius lst)
  (map (compose (curryr / 9) (curryr * 5) (curryr - 32))
       lst))
#|
curryr wurde statt curry gewählt, um sicherzustellen, dass die Reihenfolge der
Rechenparameter korrekt ist.
Wird bei der Division curry genommen, so wird 9 durch die Temperatur geteilt,
nicht die Temperatur durch 9. Bei der Multiplikation sollte es aufgrund des
Kommutativgesetzes egal sein. Bei der Subtraktion dreht sich  das Vorzeichen
des Ergebnisses um.
|#

;; Testwerte für obige Funktion. Aufgerufene Werte:
; 0°F ~ -17.8°C
; 100°F ~ 37,8°C
; 32°F ~ 0°C
; 212°F ~ 100°C
; 42°F ~ 5.6°C (gewählt aus randomness. Weil 42.)
(fahrenheit->celsius '(0 100 32 212 42))


;;;;;;;; Aufgabe 2

;; Gegeben eine Liste von Zahlen, wird die Quadratwurzel aller Elemente berechnet.
;; @param lst Die Liste von Zahlen, deren Quadratwurzel gesucht wird.
(define (root-of-all lst)
  (map sqrt lst))

;; Gegeben eine Liste von Zahlen, wird jede dieser Zahlen durch $divisor geteilt.
;; Die Ergebnisliste wird nach Ganzzahlen gefiltert: Alle Elemente, die jetzt noch
;; vorhanden waren, stammen von Elementen der Ausgangsliste ab, die von $divisor
;; geteilt werden. Es wird wieder mit $divisor multipliziert, um die ursprünglichen
;; Elemente wiederherzustellen.
;; @param lst Eine Liste von Zahlen
;; @param divisor Die Zahl, nach der geprüft werden soll, ob sie Listenelemente teilt
;; nach Aufgabenstellung ist $divisor als Default-Wert mit 3 belegt
;; $divisor darf nicht 0 sein
(define (n-divides lst [divisor 3])
  (map (lambda (x) (* x divisor)) 
       (filter integer? (map (lambda (x) (/ x divisor))
                             lst))))

;; Gegeben eine Liste von Zahlen, wird zunächst von jeder einzelnen die untere Grenze
;; $n abgezogen. Die negativen Ergebnisse werden herausgefiltert und durch das
;; Addieren von $n auf den Rest stellt die ursprünglichen Werte wieder her, die
;; größer als die Untergrenze waren.
;; Die so entstandene Liste wird nach odd? gefiltert. Die Ergebnisliste wird mit
;; foldl und dem neutralen Element der Addition zur Summe kollabiert.
;; @param lst Eine Liste von Zahlen
;; @param n Untere Grenze für die Summe der ungeraden Zahlen aus $lst
;; nach Aufgabenstellung ist $n als Default-Wert mit 10 belegt
(define (sum-of-odd-greater-n lst [n 10])
  (foldl + 0
         (filter odd?
                 (map (lambda (x) (+ x n))
                      (filter positive? (map (lambda (x) (- x n))
                                             lst))))))

;; Test-Aufurf für Aufgabe 2.1: Wurzel aller Listenelemente
;; Ergebnis: Stimmt so
(root-of-all '(1 2 3 4 5 9 16 25 31))
;; Test-Aufruf für Aufgabe 2.2: Teilbar durch 3
;; Ergebnis: '(3 6 9)
(n-divides '(1 2 3 4 5 6 7 8 9))
;; Test-Aufruf für Aufgabe 2.3: Summe ungerader Zahlen größer 10
;; Ergebnis: 39
(sum-of-odd-greater-n '(3 4 10 2 11 13 9 14 15))

;;;;;;;;; Aufgabe 3
;;;; Aufgabe 3.1
; n: 1, 2, or 3
; the-pattern: 'waves, 'oval, 'rectangle
; the-mode: 'outline, 'solid, 'hatched
; the-color: 'red, 'green, 'blue
(define Anzahl '(1 2 3))
(define Pattern '('waves 'oval 'rectangle))
(define Fuellung '('outline 'solid 'hatched))
(define Colour '('red 'green 'blue))

(define (erstelleKarte n Muster Form Farbe)
  ('(n Muster Form Farbe)))

(define (zeigeKarte Karte)
  (show-set-card
   (list-ref Karte 0)
   (list-ref Karte 1)
   (list-ref Karte 2)
   (list-ref Karte 3)))


(define (erstelleUndZeigeNKarten n)
  (zeigeKarte
   (erstelleKarte
    (list-ref Anzahl (random 3))
    (list-ref Pattern (random 3))
    (list-ref Fuellung (random 3))
    (list-ref Colour (random 3))))
  (cond (n > 1)
        (erstelleUndZeigeNKarten (- n 1))))


;;;; Aufgabe 3.2

;;;; Aufgabe 3.3

;;;; Aufgabe 3.4 (Zusatzaufgabe)