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


;;;;;;;; Aufgabe 1

;;;; Aufgabe 1.1

(define spiel #(0 0 0 0 0 9 0 7 0
                0 0 0 0 8 2 0 5 0
                3 2 7 0 0 0 0 4 0
                0 1 6 0 4 0 0 0 0
                0 5 0 0 0 0 3 0 0
                0 0 0 0 9 0 7 0 0
                0 0 0 6 0 0 0 0 5
                8 0 2 0 0 0 0 0 0
                0 0 4 2 0 0 0 0 8))

; nur zu Testzwecken
(define fehlerhaftesSpiel #(3 9 0 0 0 9 0 7 0
                            0 0 0 0 8 2 0 5 0
                            3 2 7 0 0 0 0 4 0
                            0 1 6 0 4 0 0 0 0
                            0 5 0 0 0 0 3 0 0
                            0 0 0 0 9 0 7 0 0
                            0 0 0 6 0 0 0 0 5
                            8 4 2 0 0 0 0 0 0
                            0 0 4 2 0 0 0 0 8))

;;;; Aufgabe 1.1.1

;(write-string "TODO 1.1.1")
;TIMESTAMP Aufgabe 1.1.4

; liest eine Zeile und eine Spalte in einen Index um
; spalte: die Spalte
; zeile: die Zeile
(define xy->index (lambda (spalte zeile)
  (+ spalte (* 9 zeile))))

;;;; Aufgabe 1.1.2


; Gibt die Indizes der x-ten Zeile aus
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define zeile->indizes (lambda (x)
  (indizesZeile (findeStartindexZeile x))))

; Findet den Startindex für eine gegebene Zeile x
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define findeStartindexZeile (lambda (x)
  (cond [(= x 0) 0]
        [(= x 1) 9]
        [(= x 2) 18]
        [(= x 3) 27]
        [(= x 4) 36]
        [(= x 5) 45]
        [(= x 6) 54]
        [(= x 7) 63]
        [(= x 8) 72]
        [else (error "Dieser Index existiert nicht.")])))
#|
(define indizesZeile (lambda (x)
  (map (lambda (x) (+ x (index))) (make-list 9 x))))
|#

; findet die Indizes der Zeile für einen gegebenen Startindex x
; x: eine Zahl
(define indizesZeile (lambda (x)
  (list x (+ x 1) (+ x 2) (+ x 3) (+ x 4) (+ x 5) (+ x 6) (+ x 7) (+ x 8))))

; Gibt die Indizes der x-ten Spalte aus
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define spalte->indizes (lambda (x)
  (indizesSpalte (findeStartindexSpalte x))))

; Findet den Startindex für eine gegebene Spalte x
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define findeStartindexSpalte (lambda (x)
  (cond [(and (<= 0 x) (<= x 8)) x]
        [else (error "Dieser Index existiert nicht.")])))

; findet die Indizes der Spalte für einen gegebenen Startindex x
; x: eine Zahl
(define indizesSpalte (lambda (x)
  (list x (+ x 9) (+ x 18) (+ x 27) (+ x 36) (+ x 45) (+ x 54) (+ x 63) (+ x 72))))

; Gibt die Indizes des x-ten Quadranten aus
; (Die Nummern der Quadranten sind von links nach rechte von oben nach unten durchnummeriert, mit:
; oben links ist 0, oben rechts ist 2, mitte links ist 3, mitte rechts ist 5, unten links ist 6, unten rechts ist 8
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define quadrant->indizes (lambda (x)
  (indizesQuadrant (findeStartindexQuadrant x))))

; Findet den Startindex für einen gegebenen Quadranten x
; x: eine Zahl zwischen 0 und 8 (je inklusive)
(define findeStartindexQuadrant (lambda (x)
  (cond [(= x 0) 0]
        [(= x 1) 3]
        [(= x 2) 6]
        [(= x 3) 27]
        [(= x 4) 30]
        [(= x 5) 33]
        [(= x 6) 54]
        [(= x 7) 57]
        [(= x 8) 60]
        [else (error "Dieser Index existiert nicht.")])))

; findet die Indizes des Quadranten für einen gegebenen Startindex x
; x: eine Zahl
(define indizesQuadrant (lambda (x)
  (list x (+ x 1) (+ x 2) (+ x 9) (+ x 10) (+ x 11) (+ x 18) (+ x 19) (+ x 20))))

;;;; Aufgabe 1.1.3

; gibt aus einem gegebenen Spiel "spiel" die Werte der Felder mit den Indizes aus der Indexliste "indexliste" aus
; spiel: das Spielfeld mit dem gerechnet wird
; indexliste; die Indexliste mit der gerechnet wird
(define spiel->eintraege (lambda (spiel indexliste)
  (map (lambda (x) (vector-ref spiel x)) indexliste)))

;;;; Aufgabe 1.1.4

; gibt zurück, ob das Spiel sich sich in einem konsistenten Zustand befindet
; spiel: das Spiel, welches geprüft wird
(define spiel-konsistent? (lambda (spiel)
  (and (regelZeile spiel) (regelSpalte spiel) (regelQuadrant spiel))))

; prüft, ob die Regel X (s. Aufgabenzettel 10 Seite 2) für ein gegebenes Spielfeld "Spiel" gilt
; spiel; das zu überprüfende Spiel
; funktion: die FUnktion, die die eine Indexmenge ausliest, die geprüft werden kann
(define regelX (lambda (spiel funktion)
    (not (member #f (map
                (lambda (wert) (pruefeRegel (spiel->eintraege spiel (funktion wert))))
                (build-list 9 values))))))
#|
; Funktioniert alles nicht so Die Frage ist jetzt: warum nicht?
(define (test spiel)
    (apply my-and (map
                (lambda (wert) (pruefeRegel (spiel->eintraege spiel (zeile->indizes wert))))
                (build-list 9 values))))

(define my-plus (lambda (a . [rest 0])
  (+ a (apply my-plus rest))))

(define my-and (lambda (erstesElement . [Rest '(#t)])
  (and erstesElement (apply my-and Rest))))

; zu langer Code der durch obiges gekürzt werden sollte
(define regelZeileAlt (lambda (spiel)
  (let ([ersteZeile (zeile->indizes 0)]
        [zweiteZeile (zeile->indizes 1)]
        [dritteZeile (zeile->indizes 2)]
        [vierteZeile (zeile->indizes 3)]
        [fuenfteZeile (zeile->indizes 4)]
        [sechsteZeile (zeile->indizes 5)]
        [siebteZeile (zeile->indizes 6)]
        [achteZeile (zeile->indizes 7)]
        [neunteZeile (zeile->indizes 8)])
    (and (pruefeRegel (spiel->eintraege spiel ersteZeile))
         (pruefeRegel (spiel->eintraege spiel zweiteZeile))
         (pruefeRegel (spiel->eintraege spiel dritteZeile))
         (pruefeRegel (spiel->eintraege spiel vierteZeile))
         (pruefeRegel (spiel->eintraege spiel fuenfteZeile))
         (pruefeRegel (spiel->eintraege spiel sechsteZeile))
         (pruefeRegel (spiel->eintraege spiel siebteZeile))
         (pruefeRegel (spiel->eintraege spiel achteZeile))
         (pruefeRegel (spiel->eintraege spiel neunteZeile))))))
|#

; prüft, ob in einer gegebenen Indexliste "indizes" jede Ziffer nur maximal einmal vorkommt (außer 0)
; indizes: die Indexliste, die geprüft wird
(define pruefeRegel (lambda (indizes)
  (and (<= (length (filter (lambda (x) (= x 1)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 2)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 3)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 4)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 5)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 6)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 7)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 8)) indizes)) 1)
       (<= (length (filter (lambda (x) (= x 9)) indizes)) 1))))


; prüft, ob die Regel 1 (s. Aufgabenzettel 10 Seite 2) für ein gegebenes Spielfeld "Spiel" gilt
; spiel; das zu überprüfende Spiel
(define regelZeile (lambda (spiel)
  (regelX spiel zeile->indizes)))

; prüft, ob die Regel 2 (s. Aufgabenzettel 10 Seite 2) für ein gegebenes Spielfeld "Spiel" gilt
; spiel; das zu überprüfende Spiel
(define regelSpalte (lambda (spiel)
  (regelX spiel spalte->indizes)))

; prüft, ob die Regel 3 (s. Aufgabenzettel 10 Seite 2) für ein gegebenes Spielfeld "Spiel" gilt
; spiel; das zu überprüfende Spiel
(define regelQuadrant (lambda (spiel)
  (regelX spiel quadrant->indizes)))

; gibt zurück, ob das Spiel bereits gelöst ist
; spiel: das Spiel, welches geprüft wird
(define spiel-geloest? (lambda (spiel)
  (not (vector-member 0 spiel))))



















