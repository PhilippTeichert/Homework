#lang racket
;;;;Aufgabe 1.1
;;Gegeben ein Intervall als Pair werden n gleichmäßig voneinander entfernte Zahlen aus diesem Intervall angezeigt.
;;Inklusive der ersten Zahl und exklusive der letzten
(define (range interval n)
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (step (/ laenge n))
           (newPair (cons (+ start step) stop))]
      (if (> n 1) ;wenn es noch einen rekursionsschritt zu tun gibt
          (cons start (range newPair (- n 1)));verbinde dein erstes element des intervalles mit dem ergebnis der rechnung einen schritt kleiner
          (cons start '())))));sonst häng den aktuellen start als rekursionsabschluss an die leere Liste

;;;;Aufgabe 1.2
;;Gegeben ein Intervall als Pair werden n gleichmäßig voneinander entfernte Zahlen aus diesem Intervall angezeigt.
;;Inklusive der ersten Zahl und exklusive der letzten
;;Das ergebnis ist die Startliste an die die gefundenen Elemente dran gehängt werden (default '())
(define (range_endrekursiv interval n [ergebnis '()])
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (step (/ laenge n))
           (offset (- stop step))
           (newPair (cons start offset))]
      (if (> n 1) ;wenn es noch einen rekursionsschritt zu tun gibt
          (range_endrekursiv newPair (- n 1) (cons offset ergebnis));verbinde dein letztes element des intervalles mit dem bisherigen ergebnis
          (cons offset ergebnis)))));sonst häng das letzte element an die liste und gib sie als rekursionsabschluss aus

;;;;Aufgabe 1.3
;;Gegeben ein Intervall als Pair werden n gleichmäßig voneinander entfernte Zahlen aus diesem Intervall angezeigt.
;;Inklusive der ersten Zahl und exklusive der letzten
(define (range_hoch interval n)
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (step (/ laenge n))]
      ;(map
       ;(lambda (x) (+ x start))
       (build-list n (lambda (x) (+ (* x step) start))))));)

;;;;Aufgabe 2.1
;;Gegeben eine Funktion und Intervall als Pair werden n gleichmäßig voneinander entfernte Zahlen aus diesem Intervall angezeigt und die funktion auf sie anwendet und das ergebnis in einer Liste aus Paaren ausgibt.
;;Inklusive der ersten Zahl und exklusive der letzten
;; die funktion f muss an der stelle 0 definiert sein
(define (function->points f interval n)
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (step (/ laenge n))]
       (build-list n (lambda (x) (let [(num (* x step))](cons num (f num))))))))
;;(cons start (f start))
 
;;;;Aufgabe 2.2
;;Gegeben eine Liste an Werten aus Aufgabe 1 ein Intervall als Pair die Werte werden auf das neue intervall abgebildet
;;Inklusive der ersten Zahl und inklusive der letzten
(define (rescale1d list interval)
  (let* [(listNull (map (curryr - (argmin (curry + 0) list)) list));setzt das kleinste element der lsite auf 0 und passt alle anderen elemente an (zieht das kleinste element von jedem wert der liste ab)
        (listNullBisEins (map (curryr / (argmax (curry + 0) listNull)) listNull));damit die liste nur werte zwischen 0 und 1 enthält (damit lässt sich einfacher weiter rechnen) wird jedes element durch das größte geteilt
        (listSkaliert (map (curry * (- (cdr interval) (car interval))) listNullBisEins))];damit ist die liste richtig skaliert, fängt aber bei 0 an
        (map (curry + (car interval)) listSkaliert)));damit wird der offset auf die liste addiert (der erste wert des intervalls)
      
;(cons start (f start))
;(rescale1d (range '(0 . 10) 40) '(10 . 50))
 
;;;;Aufgabe 2.2
;;Gegeben eine Liste an Pairs aus Aufgabe 2.1 und zwei Intervall als Pairs der erste Werte eines Pairs aus der Liste von Pairs wird werden auf das erste intervall abgebildet, der zweite auf das zweite
;;Inklusive der ersten Zahl und inklusive der letzten
(define (rescale2d list interval1 interval2)
  (let [(list1 (map car list))
        (list2 (map cdr list))]
    (map cons (rescale1d list1 interval1) (rescale1d list2 interval2))))
;(rescale2d (function->points sqr '(0 . 10) 5) '(10 . 50) '(5 . 25))

;;;;Aufgabe 2.3
;;
(require 2htdp/image)

(define breite 800)
(define hoehe 600)

; Erzeugt die Null-Ebene, auf der die Objekte platziert werden.
(define null-plain (rectangle breite hoehe "solid" (color 0 0 0 0)))
; Definiert einen Punkt.
(define punkt (ellipse 1 1 "solid" "blue"))

;; malt die Punkte in einem feld
(define (draw-points pointlist)
  (let [(point (place-image punkt (caar pointlist) (- hoehe (cdar pointlist)) null-plain))]
    (if (= (length pointlist) 1)
        point
        (underlay
             point;Abbruchbedingung: Male den letzten Punkt
             (draw-points (cdr pointlist))))))
;(draw-points (function->points sqr '(0 . 10) 5))

;;;;Aufgabe 2.4
;;zeichnet den Funktionsgraphen der Funktion f mit n vielen Punkten im Intervall interval
;;linien? ist ein boolean der per default auf nein gesetzt wird (er bestimmt, ob Linien oder Punkte ausgegeben werden)
(define (plot-function f interval n [linien? #f])
  (let [(allePunkte (rescale2d
                    (function->points f interval n)
                    (cons 0 breite)
                    (cons 0 hoehe)))]
    (if linien?
        (draw-lines allePunkte)
        (draw-points allePunkte))))
  
;(print (color-frame "black" (plot-function sqr '(0 . 200) 100)))
;(print (color-frame "black" (plot-function (lambda (x) (* (/ 1 50) (sqr x))) '(0 . 200) 100 #t)))
;gibt das selbe Bild aus (nur einmal mit Punkten udn einmal als Linien


;;;;Aufgabe 2.5
(require 2htdp/universe)
(define marker (ellipse 5 5 "outline" "red"))
;;Zeichnet den Funktionsgraphen der Funktion f mit n vielen Punkten im Intervall interval
;;linien? ist ein boolean der per default auf nein gesetzt wird (er bestimmt, ob Linien oder Punkte ausgegeben werden)
;;t ist der Schritt, welcher markiert wird
(define (live-plot-function f interval n t [linien? #f])
  (let [(allePunkte (rescale2d
                    (function->points f interval n)
                    (cons 0 breite)
                    (cons 0 hoehe)))
        (Index (modulo t n))]
    (let [(besondererPunkt (list-ref allePunkte Index))]
          (underlay (place-image marker (car besondererPunkt) (- hoehe (cdr besondererPunkt)) null-plain)
                    (if linien?
                        (draw-lines allePunkte)
                        (draw-points allePunkte))))))
;(animate (curry live-plot-function (lambda (x) (* (/ 1 50) (sqr x))) '(0 . 300) 200))
;(animate (curryr (curry live-plot-function sin (cons 0 (* 2 pi)) 200) #t))
;für lustige rechts-raus-links-rein-geschichten


;;;;Aufgabe 2.6 (Zusatzaufgabe)
;;zeichnet ein Bild mit Linien die von Punkt zu Punkt führen
(define (draw-lines pointlist)
  (let [(line (place-image (line (- (caar pointlist) (caadr pointlist));x-richtung
                                 (- (cdadr pointlist) (cdar pointlist));y-richtung
                                 "blue");farbe
                           (caar pointlist)
                           (- hoehe (cdar pointlist))
                           null-plain))]
    (if (= (length pointlist) 2)
        line;abbruchbedingung:male die letzte linie
        (underlay line (draw-lines (cdr pointlist))))))
;(draw-lines (function->points sqr '(0 . 10) 5))















;;racketbeispiel animate

  (define (create-UFO-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))
 
(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))
 
;(animate create-UFO-scene)