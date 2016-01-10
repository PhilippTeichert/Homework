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

;;;;;;;; Aufgabe 1.1

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
(define fehlerhaftesSpiel #(3 9 0 X 0 9 0 7 0
                            0 0 0 0 8 2 0 5 0
                            3 2 7 0 0 0 0 4 0
                            0 1 6 0 4 0 0 0 0
                            0 5 0 0 0 0 3 0 0
                            0 0 0 0 9 0 7 0 0
                            0 0 0 6 0 0 0 0 5
                            8 4 2 0 0 0 0 0 0
                            0 0 4 2 0 0 0 0 8))

; nur zu Testzwecken
(define unloesbaresSpiel #(1 0 0 0 0 9 0 7 0
                           0 0 0 0 8 2 0 5 0
                           3 2 7 0 0 0 0 4 0
                           0 1 6 0 4 0 0 0 0
                           0 5 0 0 0 0 3 0 0
                           0 0 0 0 9 0 7 0 0
                           0 0 0 6 0 0 0 0 5
                           8 0 2 0 0 0 0 0 0
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
  (and (<= (length (filter (lambda (x) (equal? x 1)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 2)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 3)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 4)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 5)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 6)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 7)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 8)) indizes)) 1)
       (<= (length (filter (lambda (x) (equal? x 9)) indizes)) 1))))


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

;;;;;;;; Aufgabe 1.2

;;;; Aufgabe 1.2.1

; erstellt einen Vektor abhängig von "spiel" mit 0en, wo die Zahl "ziffer" noch eingesetzt werden kann#
; spiel: das Spiel, auf dem geprüft wird (es muss in folgender Form sein: "(and/c vector? (not/c immutable?))")
; ziffer: die Ziffer die geprüft wird
(define markiere-ausschluss (lambda (spiel ziffer)
  (if (spiel-konsistent? spiel)
      (markiereSpielfeld spiel ziffer)
      (error "Speilzustand inkonsistent"))))

; markiert das Spielfeld auf grundlage der Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
(define markiereSpielfeld (lambda (spiel ziffer)
  (markiereZeilen spiel ziffer)
  (markiereSpalten spiel ziffer)
  (markiereQuadranten spiel ziffer)))

; markiert die Zeilen auf Grundlage der Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
(define markiereZeilen (lambda (spiel ziffer)
  (markiereX spiel ziffer zeile->indizes)))

; markiert eine Indexmenge auf Grundlage der Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
; funktion: eine Funktion zur Bestimmung der Indexmenge
(define markiereX (lambda (spiel ziffer funktion)
          (let ([indizes
                 (funktion 0)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 0 durch Xe
          (let ([indizes
                 (funktion 1)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 1 durch Xe
          (let ([indizes
                 (funktion 2)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 2 durch Xe
          (let ([indizes
                 (funktion 3)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 3 durch Xe
          (let ([indizes
                 (funktion 4)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 4 durch Xe
          (let ([indizes
                 (funktion 5)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 5 durch Xe
          (let ([indizes
                 (funktion 6)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 6 durch Xe
          (let ([indizes
                 (funktion 7)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])) ; wenn nötig, ersetze alle 0en aus Zeile 7 durch Xe
          (let ([indizes
                 (funktion 8)])
            (cond [(pruefeX (spiel->eintraege spiel indizes) ziffer)
                (ersetzeX indizes spiel)])))) ; wenn nötig, ersetze alle 0en aus Zeile 8 durch Xe

; prüft, ob die Ziffer "ziffer" in der Menge an Indizes "indizes" vorhanden ist
(define pruefeX (lambda (indizes ziffer)
  (= (length (filter (lambda (x) (equal? x ziffer)) indizes)) 1)))

; ersetzt alle 0en in einer Indexmenge durch Xe
(define ersetzeX (lambda (indizes spiel)
  (map
   (lambda (index)
     (cond [(equal? '(0) (spiel->eintraege spiel `(,index))) (vector-set! spiel index 'X)]))
   indizes)))
         
; markiert die Spalten auf Grundlage der Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
(define markiereSpalten (lambda (spiel ziffer)
  (markiereX spiel ziffer spalte->indizes)))
; markiert die Quadranten auf Grundlage der Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
(define markiereQuadranten (lambda (spiel ziffer)
  (markiereX spiel ziffer quadrant->indizes)))

;;;; Aufgabe 1.2.2

; findet eindeutige Positionen in einem gegebenen Spielfeld "spiel" für eine gegebene Ziffer "ziffer", wobei erst die Ausschussorte markiert werden
; spiel: das Spielfeld
; ziffer: die Ziffer
(define eindeutige-positionen (lambda (spiel ziffer)
  (let ([neuesSpielfeld (vector-copy spiel)])
    (markiere-ausschluss neuesSpielfeld ziffer)
    (remove-duplicates (eindeutigeIndizes neuesSpielfeld)))))

; findet eindeutige Positionen in einem gegebenen Spielfeld "spiel" für eine gegebene Ziffer "ziffer"
; spiel: das Spielfeld
; ziffer: die Ziffer
(define eindeutigeIndizes (lambda (spiel)
  (append
   (eindeutigeZahlen spiel zeile->indizes)
   (eindeutigeZahlen spiel spalte->indizes)
   (eindeutigeZahlen spiel quadrant->indizes)
   )))

; findet alle einzelnen 0en in der Indexliste die durch die Funktion "funktion" erzeugt wird
; spiel: das Spielfeld
; funktion: die Funktion
(define eindeutigeZahlen (lambda (spiel funktion)
  (append
   (eindeutigerAbschnitt spiel funktion 0)
   (eindeutigerAbschnitt spiel funktion 1)
   (eindeutigerAbschnitt spiel funktion 2)
   (eindeutigerAbschnitt spiel funktion 3)
   (eindeutigerAbschnitt spiel funktion 4)
   (eindeutigerAbschnitt spiel funktion 5)
   (eindeutigerAbschnitt spiel funktion 6)
   (eindeutigerAbschnitt spiel funktion 7)
   (eindeutigerAbschnitt spiel funktion 8)
   )))

; findet alle Indizes dee 0en in einer Indexliste, die durch die Funktion "funktion" mit dem Parameter "ziffer"erzeugt wird
; spiel: das Spielfeld
; funktion: die Funktion
; ziffer: der Parameter der Funktion "funktion"
(define eindeutigerAbschnitt (lambda (spiel funktion ziffer)
  (let ([nullen (gibIndex (funktion ziffer) 0 spiel)])
    (if (= (length nullen) 1)
      nullen
      '()))))

(define neuesSpielfeld (vector-copy spiel))
; gibt den Index der Ziffer "ziffer" in der Indexliste "indizes" zurück
; indizes: die Indexliste
; ziffer: die Ziffer
; spiel: das Spielfeld
(define gibIndex (lambda (indizes ziffer spiel)
  (filter (lambda (index) (equal? (spiel->eintraege spiel `(,index)) `(,ziffer))) indizes)))

;;;; Aufgabe 1.2.3

; loest ein Spiel und gibt das gelöste Spielfeld danach aus
; spiel: das zu lösende Spiel
(define loese-spiel (lambda (spiel)
  (let ([neuesSpiel (vector-copy spiel)])
    (if (spiel-konsistent? neuesSpiel)
      (gibErgebnis neuesSpiel)
      (error "Spiel inkonsistent")))))

; prüft rekursiv alle Regeln für alle Ziffern durch
; spiel: das zu lösende Spiel
(define gibErgebnis (lambda (spiel)
  (if (spiel-geloest? spiel)
      spiel
      (let ([positionen (map (lambda (ziffer) (eindeutige-positionen spiel ziffer)) (range 1 10))])
        (if (equal? positionen '(() () () () () () () () ()))
            (error "Sudoku ist ohne Backtracking nicht lösbar")
            (map (lambda (index) (setzeZiffer (list-ref positionen index) (+ 1 index) spiel)) (range 0 9)))
        (gibErgebnis spiel)))))

; setzt eine Ziffer "ziffer" an die gefundenen Stellen "indizes"
; indizes: die gefundenen Stellen, wo die Ziffer "ziffer" eindeutig hin kommt
; ziffer: die zu setzende Ziffer
; spiel: das Spiel
(define setzeZiffer (lambda (indizes ziffer spiel)
  (cond [(> (length indizes) 0)
         (vector-set! spiel (car indizes) ziffer)
         (setzeZiffer (cdr indizes) ziffer spiel)])))


;;;;;;;; Aufgabe 1.3

(require 2htdp/image)

;;;; Aufgabe 1.3.1

; die Länge des Bildes
(define laenge 400)

; Erzeugt die Null-Ebene, auf der die Objekte platziert werden.
(define null-plain (square laenge  "outline" "black"))

; malt ein Sudoku-Grundgerüst
(define sudokuGrundgeruest
  (let* ([neueLaenge (/ laenge 9)]
         [neueLaengeHalbe (/ neueLaenge 2)]
         [feldX (lambda (x) (+ neueLaengeHalbe (* neueLaenge x)))])
  (underlay
   null-plain
   ; erste Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 0) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 0) null-plain)
   ; zweite Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 1) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 1) null-plain)
   ; dritte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 2) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 2) null-plain)
   ; vierte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 3) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 3) null-plain)
   ; fünfte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 4) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 4) null-plain)
   ; sechste Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 5) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 5) null-plain)
   ; siebte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 6) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 6) null-plain)
   ; achte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 7) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 7) null-plain)
   ; neunte Zeile
   (place-image (square neueLaenge "outline" "black") (feldX 0) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 1) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 2) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 3) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 4) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 5) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 6) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 7) (feldX 8) null-plain)
   (place-image (square neueLaenge "outline" "black") (feldX 8) (feldX 8) null-plain)
   ; Trennlinien Horizontal
   (place-image (rectangle laenge 3 "solid" "black") (/ laenge 2) 0 null-plain)
   (place-image (rectangle laenge 3 "solid" "black") (/ laenge 2) (* neueLaenge 3) null-plain)
   (place-image (rectangle laenge 3 "solid" "black") (/ laenge 2) (* neueLaenge 6) null-plain)
   (place-image (rectangle laenge 3 "solid" "black") (/ laenge 2) (* neueLaenge 9) null-plain)
   ; Trennlinien Vertikal
   (place-image (rectangle 3 laenge "solid" "black") 0 (/ laenge 2) null-plain)
   (place-image (rectangle 3 laenge "solid" "black") (* neueLaenge 3) (/ laenge 2) null-plain)
   (place-image (rectangle 3 laenge "solid" "black") (* neueLaenge 6) (/ laenge 2) null-plain)
   (place-image (rectangle 3 laenge "solid" "black") (* neueLaenge 9) (/ laenge 2) null-plain)
   )))

; die Funktion, um ein Spielfeld "spiel" zu zeichnen
; siel: das Spielfeld
(define zeichne-spiel (lambda (spiel)
  (underlay
    (zeichneFeld spiel (- (length (vector->list spiel)) 1))
    sudokuGrundgeruest
   )))

#|
(define zeichneFeld (lambda (spiel)
  (map
   (lambda (index)
     (maleIndex (curry list-ref (vector->list spiel))
                index))
   (range 0 81))))
|#

; liest das Spielfeld aus und malt es auf das Sudoku-Raster
; spiel: das Spielfeld
; index: der aktuelle Index (der Iterator)
(define zeichneFeld (lambda (spiel index)
  (if (< index 0)
      (place-image (square 0 "solid" "black") 0 0 null-plain)
      (underlay
        (maleIndex ((curry list-ref (vector->list spiel)) index) index)
        (zeichneFeld spiel (- index 1))))))

; für für einen Index "index" das Element "element" in das Bild ein
; element: das Element an Index "index"
; index: der Index
(define maleIndex (lambda (element index)
  (let* ([x (modulo index 9)]
        [y (/ (- index x) 9)]
        [neueLaenge (/ laenge 9)]
        [neueLaengeHalbe (/ neueLaenge 2)]
        [feldX (lambda (x) (+ neueLaengeHalbe (* neueLaenge x)))])
    (cond [(equal? element 'X)
           (place-image (square neueLaenge "solid" "red") (feldX x) (feldX y) null-plain)]
          [(ohne0UndX element)
           (place-image (text (number->string element) (round neueLaengeHalbe) "black") (feldX x) (feldX y) null-plain)]
          [else (place-image (square 0 "solid" "black") 0 0 null-plain)]))))

; prüft, ob es sich um eine Ziffer zwischen 1 und 9 (je inklusive) handelt
; zahl: die zu überprüfende Zahl
(define ohne0UndX (lambda (zahl)
  (and (number? zahl)
       (< 0 zahl)
       (> 10 zahl))))

;(zeichne-spiel fehlerhaftesSpiel)
;(zeichne-spiel unloesbaresSpiel)
;(print (color-frame "black" (zeichne-spiel spiel)))
;((lambda (spiel) (let ([neuesSpiel (vector-copy spiel)]) (markiere-ausschluss neuesSpiel 5) (zeichne-spiel neuesSpiel))) spiel)


;;;; Aufgabe 1.3.2

; loest ein Spiel und gibt das gelöste Spielfeld danach aus
; spiel: das zu lösende Spiel
(define loese-spiel-grafisch (lambda (spiel [zwischenschritte? #f])
  (let ([neuesSpiel (vector-copy spiel)])
    (if (spiel-konsistent? neuesSpiel)
      (append (list "Initialzustand:" (zeichne-spiel spiel))
        (gibErgebnisGrafisch neuesSpiel zwischenschritte? 1))
      (error "Spiel inkonsistent")))))

; prüft rekursiv alle Regeln für alle Ziffern durch
; spiel: das zu lösende Spiel
(define gibErgebnisGrafisch (lambda (spiel zwischenschritte? index)
  (if (spiel-geloest? spiel)
      (list "Lösung:" (zeichne-spiel spiel))
      (let ([positionen (map (lambda (ziffer) (eindeutige-positionen spiel ziffer)) (range 1 10))])
        (if (equal? positionen '(() () () () () () () () ()))
            (error "Sudoku ist ohne Backtracking nicht lösbar")
            (map (lambda (index) (setzeZiffer (list-ref positionen index) (+ 1 index) spiel)) (range 0 9)))
        (append (cond [zwischenschritte? (if (spiel-geloest? spiel)
                                             '()
                                             (list (string-append "Schritt " (number->string index) ":") (zeichne-spiel spiel)))]
                            [else '()])
        (gibErgebnisGrafisch spiel zwischenschritte? (+ 1 index)))))))
