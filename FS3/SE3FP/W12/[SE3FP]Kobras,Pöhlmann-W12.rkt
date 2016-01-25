;+++++++++++++++++++++++
;
;;  Kobras    6658699
;;  Pöhlmann  6663579
;
;+++++++++++++++++++++++
;
;;  Seppke/Gr. 9 - Abgabe 25.01.2016 12:00
;
;+++++++++++++++++++++++


; #lang lazy ; für Aufgabe 2 auskommentieren
#lang racket ; für Aufgabe 3 auskommentieren

;;;;;;;; Aufgabe 2

; Eine Funktion, um das Abzählspiel "die Böse 7" zu realisieren
(define (sieben [aktuelleZahl 1])
  (if (or (= (modulo aktuelleZahl 7) 0) (not (equal? #f (member #\7 (string->list (number->string aktuelleZahl))))))
      (cons 'sum (sieben (+ 1 aktuelleZahl)))
      (cons aktuelleZahl (sieben (+ 1 aktuelleZahl)))))

;;;;;;;; Aufgabe 3

(require se3-bib/tools-module)
(require racket/trace)

; Eine Funktion, die rekursiv eine Fakultät berechnet
(define (fakultaet Zahl)
  (if (= Zahl 0)
      1
      (* Zahl (fakultaet (sub1 Zahl)))))

; rekursiven Aufruf an memo binden für Laufzeiteffizienz
(set! fakultaet (memo fakultaet))
; Zum tracen, um zu sehen, ob es auch wirklich den Memo-Teil benutzt
(trace fakultaet)
(fakultaet 10)
(fakultaet 15)