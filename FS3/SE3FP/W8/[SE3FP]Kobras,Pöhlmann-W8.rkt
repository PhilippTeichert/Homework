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

|#
;;;; Aufgabe 1.4
#|
1.\n

2.\n

3.\n

4.\n

|#
;;;; Aufgabe 1.5 (Zusatzaufgabe)
#|
Konvertierung erfolgt °C -> °F.
|#

;;;;;;;; Aufgabe 2

(define (root-of-all lst)
  (map sqr lst))

(define (three-divides lst)
  (filter (lambda (x) / 3) lst))

(define (sum-of-odd-greater-ten lst)
  (#t))

;;;;;;;;; Aufgabe 3
;;;; Aufgabe 3.1

;;;; Aufgabe 3.2

;;;; Aufgabe 3.3

;;;; Aufgabe 3.4 (Zusatzaufgabe)