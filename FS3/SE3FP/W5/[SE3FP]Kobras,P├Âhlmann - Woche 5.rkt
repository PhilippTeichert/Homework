#|
;++++++++++++++++++++++
;
;; Kobras    6658699
;; Pöhlmann  6663579
;
;++++++++++++++++++++++
;
;; Seppke/Gr.9 - Woche 5
;
;++++++++++++++++++++++
|#

#lang racket

#|
Aufgabe 1


Gliederung:
    1.Die Rezessiven Merkmale raten.
    2.Dominante Merkmale herausfinden.
    3.Mögliche rezessive Merkmale für gegebene Dominante herausfinden.
    4.Erstellung eines Kindschmetterlings durch seine Eltern.
    5.Darstellung der Schmetterlinge.
    6.Erzeugen von n Schmetterlingen, ohne dass die Eltern jedes mal neue rezessive Merkmale erhalten.

1.
rateRezessivesMerkmal rät für jede Eigenschaft(Flügelfarbe, Musterung, etc.) ein rezessives Merkmal.
Erst wird geprüft, in welcher Liste sich das dominante Merkmal befindet und dann wird aus dieser Liste eines mit einem Index >= dem des dominanten Merkmals (eines, welches rezessiver ist, als das dominante) gewählt.
Parameter:
    Merkmal     Ein dominantes Merkmal des Schmetterlings.

2.
dominantesMerkmal findet bei einem gegebenen Merkmalpaar das dominante Merkmal heraus.
Parameter:
    Merkmal1   Der erste Merkmal.
    Merkmal2   Das zweite Merkmal.

3.
generiereSchmetterling generiert bei gegebenen dominanten Merkmalen einen Schmetterling mit zufälligen rezessiven Merkmalen.
Prameter:
    Flügelfarbe     Die dominante Flügelfarbe.
    Musterung       Die dominante Musterung.
    Fühlerform      Die dominante Fühlerform.
    Flügelform      Die dominante Flügelform.

4.
erstelleKind erstellt ein Kind von zwei gegebenen Elternschmetterlingen.
Parameter:
    Schmetterling1      Das erste Elternteil.
    Schmetterling2      Das zweite Elternteil.

5.
zeigeSchmetterling zeigt einen Schmetterling als Bild an.
Parameter:
    Schmetterling   Der anzuzeigende Schmetterling.

6.
erzeugeKindSchmetterlinge erzeugt beliebig viele Kinderschmetterlinge bei Angabe der Eltern.
Parameter:
    Flügelfarbe1    Die dominante Flügelfarbe des ersten Elternschmetterlings.
    Musterung1      Die dominante Musterung des ersten Elternschmetterlings.
    Fühlerform1     Die dominante Fühlerform des ersten Elternschmetterlings.
    Flügelform1     Die dominante Flügelform des ersten Elternschmetterlings.
    Flügelfarbe2    Die dominante Flügelfarbe des zweiten Elternschmetterlings.
    Musterung2      Die dominante Musterung des zweiten Elternschmetterlings.
    Fühlerform2     Die dominante Fühlerform des zweiten Elternschmetterlings.
    Flügelform2     Die dominante Flügelform des zweiten Elternschmetterlings.
    Anzahl          Die Anzahl der zu erzeugenden Kinderschmetterlinge.


Entwurf:
    Ein Schnetterling wird definiert als eine Liste von Paaren in der Reihenfolge:
        Flügelfarbe
        Musterung
        Fühlerform
        Flügelform
    Dies hat folgende Vorteile:
        Das dominante Merkmal kann immer als erstes Element eines Paares gespeichert werden, was den Zugriff erleichtert.
        Eine Liste lässt dich einfach durchsuchen, dies erleichtert weder den Zugriff.
        Es gibt Funktionen für Listen, die hier verwendet werden können.
        Ich habe schon eine konkrete Vorstellung, wie man das implementieren könnte.



Implementation:
|#


(define Fluegelfarbe '(red green blue yellow))
(define Musterung    '(dots stripes star))
(define Fuehlerform  '(straight curved curly))
(define Fluegelform  '(rhomb hexagon ellipse))

(define (rateRezessivesMerkmal Merkmal)
    (let ([Liste (rezessiveMerkmale Merkmal)])
    (list-ref Liste (random (length Liste)))))

(define (dominantesMerkmal Merkmal1 Merkmal2)
    (let ([fluegelfarbeIndex1 (enthaelt Fluegelfarbe Merkmal1)]
        [musterIndex1 (enthaelt Musterung Merkmal1)]
        [fuehlerIndex1 (enthaelt Fuehlerform Merkmal1)]
        [fluegelformIndex1 (enthaelt Fluegelform Merkmal1)]
        [fluegelfarbeIndex2 (enthaelt Fluegelfarbe Merkmal2)]
        [musterIndex2 (enthaelt Musterung Merkmal2)]
        [fuehlerIndex2 (enthaelt Fuehlerform Merkmal2)]
        [fluegelformIndex2 (enthaelt Fluegelform Merkmal2)])
    (cond [(and fluegelfarbeIndex1 fluegelfarbeIndex2) (gibKleinerenInhalt Fluegelfarbe fluegelfarbeIndex1 fluegelfarbeIndex2)]
        [(and musterIndex1 musterIndex2) (gibKleinerenInhalt Musterung musterIndex1 musterIndex2)]
        [(and fuehlerIndex1 fuehlerIndex2) (gibKleinerenInhalt Fuehlerform fuehlerIndex1 fuehlerIndex2)]
        [(and fluegelformIndex1 fluegelformIndex2) (gibKleinerenInhalt Fluegelform fluegelformIndex1 fluegelformIndex2)])))

; Erstellt einen Schmetterling mit den gegebenen dominanten Merkmalen. Die rezessiven Merkmale werden zufllig generiert.
; Parameter:
;   Fluegelfarbe    Die dominante Flügelfarbe.
;   Musterung       Die dominante Musterung.
;   Fuehlerform     Die dominante Fühlerform.
;   Fluegelform     Die dominante Flügelform.
(define (generiereSchmetterling Fluegelfarbe Musterung Fuehlerform Fluegelform)
    (list (cons Fluegelfarbe (rateRezessivesMerkmal Fluegelfarbe))
        (cons Musterung (rateRezessivesMerkmal Musterung))
        (cons Fuehlerform (rateRezessivesMerkmal Fuehlerform))
        (cons Fluegelform (rateRezessivesMerkmal Fluegelform))))

(define (erstelleKind Schmetterling1 Schmetterling2)
    (let ([ListeFluegelfarbe1 (list
            (car (list-ref Schmetterling1 0))
            (cdr (list-ref Schmetterling1 0)))]
        [ListeFluegelfarbe2 (list
            (car (list-ref Schmetterling2 0))
            (cdr (list-ref Schmetterling2 0)))]
        [ListeMuster1 (list
            (car (list-ref Schmetterling1 1))
            (cdr (list-ref Schmetterling1 1)))]
        [ListeMuster2 (list
            (car (list-ref Schmetterling2 1))
            (cdr (list-ref Schmetterling2 1)))]
        [ListeFuehler1 (list
            (car (list-ref Schmetterling1 2))
            (cdr (list-ref Schmetterling1 2)))]
        [ListeFuehler2 (list
            (car (list-ref Schmetterling2 2))
            (cdr (list-ref Schmetterling2 2)))]
        [ListeFluegelform1 (list
            (car (list-ref Schmetterling1 3))
            (cdr (list-ref Schmetterling1 3)))]
        [ListeFluegelform2 (list
            (car (list-ref Schmetterling2 3))
            (cdr (list-ref Schmetterling2 3)))]
        [KindFluegelfarbe1 (random 2)]
        [KindFluegelfarbe2 (random 2)]
        [KindMuster1 (random 2)]
        [KindMuster2 (random 2)]
        [KindFuehler1 (random 2)]
        [KindFuehler2 (random 2)]
        [KindFluegelform1 (random 2)]
        [KindFluegelform2 (random 2)])
    (list
        (cons
            (list-ref ListeFluegelfarbe1 KindFluegelfarbe1)
            (list-ref ListeFluegelfarbe2 KindFluegelfarbe2))
        (cons
            (list-ref ListeMuster1 KindMuster1)
            (list-ref ListeMuster2 KindMuster2))
        (cons
            (list-ref ListeFuehler1 KindFuehler1)
            (list-ref ListeFuehler2 KindFuehler2))
        (cons
            (list-ref ListeFluegelform1 KindFluegelform1)
            (list-ref ListeFluegelform2 KindFluegelform2)))))
    

(define (zeigeSchmetterling Schmetterling)
    (show-butterfly
        (dominantesMerkmal
            (car (list-ref Schmetterling 0))
            (cdr (list-ref Schmetterling 0)))
        (dominantesMerkmal
            (car (list-ref Schmetterling 1))
            (cdr (list-ref Schmetterling 1)))
        (dominantesMerkmal
            (car (list-ref Schmetterling 2))
            (cdr (list-ref Schmetterling 2)))
        (dominantesMerkmal
            (car (list-ref Schmetterling 3))
            (cdr (list-ref Schmetterling 3)))))

; Methode zum Erstellen und Anzeigen der Kinderschar
(define (erzeugeKindSchmetterlinge Schmetterling1 Schmetterling2 Anzahl)
    (if (> Anzahl 0)
        (cons 
            (zeigeSchmetterling (erstelleKind Schmetterling1 Schmetterling2))
            (erzeugeKindSchmetterlinge Schmetterling1 Schmetterling2 (- Anzahl 1)))
        '()))

; Hilfsmethode zum Anzeigen der Eltern zusammen mit der Kinderschar.
(define (zeigeKinderschar Fluegelfarbe1 Musterung1 Fuehlerform1 Fluegelform1 Fluegelfarbe2 Musterung2 Fuehlerform2 Fluegelform2 Anzahl)
    (let ([Schmetterling1
            (generiereSchmetterling Fluegelfarbe1 Musterung1 Fuehlerform1 Fluegelform1)]
        [Schmetterling2
            (generiereSchmetterling Fluegelfarbe2 Musterung2 Fuehlerform2 Fluegelform2)])
    (list "Die sotlzen Eltern:"
    (zeigeSchmetterling Schmetterling1)
    (zeigeSchmetterling Schmetterling2)
    "Die Kinderschar:"
    (erzeugeKindSchmetterlinge Schmetterling1 Schmetterling2 Anzahl))))


; Hilfsfunktionen

; Prüft, ob sich ein Element in einer Liste befindet.
; Prameter:
;   Liste       Die Liste, in der geprüft wird.
;   Element     Das gesuchte Element.
(define (enthaelt Liste Element)
    (enthaelt_rek Liste Element 0))

; Hilfsfunktion für die Rekursion von (enthaelt Liste Element)
(define (enthaelt_rek Liste Element index)
    (if (>= index (length Liste))
        #f
        (if (equal? (list-ref Liste index) Element)
            index
            (enthaelt_rek Liste Element (+ index 1)))))

; Gibt eine Liste ab einem bestimmten Index aus.
; Parameter:
;   Liste   Die Liste, die ab einem bestimmten Index ausgegeben werden soll.
;   index   Der Index, ab dem die Liste ausgegeben werden soll.
(define (gibListeAusAb Liste index)
    (if(>= index (length Liste))
        '()
        (cons (list-ref Liste index) (gibListeAusAb Liste (+ index 1)))))



; Gibt eine Liste aus allen rezessiven Merkmalen zu einem Gegebenen Merkmal aus.
; Parameter:
;   Merkmal     Das gegebene Merkmal.
(define (rezessiveMerkmale Merkmal)
    (let ([fluegelfarbeIndex (enthaelt Fluegelfarbe Merkmal)]
        [musterIndex (enthaelt Musterung Merkmal)]
        [fuehlerIndex (enthaelt Fuehlerform Merkmal)]
        [fluegelformIndex (enthaelt Fluegelform Merkmal)])
    (cond [fluegelfarbeIndex (gibListeAusAb Fluegelfarbe fluegelfarbeIndex)]
        [musterIndex (gibListeAusAb Musterung musterIndex)]
        [fuehlerIndex (gibListeAusAb Fuehlerform fuehlerIndex)]
        [fluegelformIndex (gibListeAusAb Fluegelform fluegelformIndex)])))

; Gibt den Wert mit dem kleineren Index aus der Liste aus. Dies ist laut der Implementation der Listen der dominantere.
; Parameter:
;   Liste   Die Liste, in der gearbeitet wird.
;   index1  Der erste Index.
;   index2  Der zweite Index.
(define (gibKleinerenInhalt Liste index1 index2)
    (if (< index1 index2)
        (list-ref Liste index1)
        (list-ref Liste index2)))




#|
Ein Gang durch das Programm:
Es wird zeigeKinderschar aufgerufen.
Diese erstellt aus den gegebenen Daten 2 Schmetterlinge mit generiereSchmetterling.
Diese Methode lässt die rezessiven Merkmale von rateRezessivesMerkmal raten, indem die Liste der Merkmale durchgegangen wird und geprüft wird, gegenüber welchen Merkmalen das übergebene Merkmal dominant ist, und gibt den Schmetterling dann als Liste von Paaren aus.
Diese beiden Elternschmetterlinge werden nun angezeigt.
Mit diesen Elternschmetterlingen wird nun durch erzeugeKindSchmetterlinge ein Kind erstellt.
Hierzu werden je 2 Listen mit je beiden Merkmalen der Eltern (red, dots, rhomb, etc.) zu jedem Merkmal (Flügelfarbe, Musterung, etc.) erstellt und dann zufällig ein Element aus jeder dieser Listen ausgewählt.
So erhält man je ein Element pro Merkmal vom Elternteil1 und eins vom Elternteil2.
Dieses erstellte Kind wird nun angezeigt.
Wenn noch weitere Kinder erstellt werden sollen, wird die Methode rekursiv mit denselben Schmetterlingen wieder aufgerufen, ohne dass die rezessiven Merkmale neu gewürfelt werden.




Testfälle:
Test1:
    (zeigeKinderschar 'yellow 'star 'curly 'ellipse 'yellow 'star 'curly 'ellipse 3)
    Zum Testen, ob auch wirklich nur die Merkmale der Eltern übergeben werden, hier ein Test mit den rezessivsten Merkmalen von allen.
    Hier sollten bei mehrmaligem Testen immer wieder dieselben 3 Kinder raus kommen.

Test2:
    (zeigeKinderschar 'red 'dots 'straight 'rhomb 'red 'dots 'straight 'rhomb 3)
    Zum Testen, ob auch wirklich alle Merkmale der Eltern übergeben werden, hier ein Test mit den dominantesten Merkmalen von allen.
    Hier sollten bei mehrmaligem Testen zufallsbedingt unterschiedliche Kinder raus kommen.

Test3:
    (zeigeKinderschar 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 3)
    Zum Testen, ob die Methode auch wirklich prüft, was für Eingaben sie bekommt, ein Negativtest mit unsinniger Eingabe.
    Hier sollte bei mehrmaligem Testen immer irgendwie an der Ausgabe zu erkennen sein, dass die Eingabe unvollständig/unsinnig/fehlerhaft war.

Test4:
    (zeigeKinderschar (list-ref Fluegelfarbe (random 4)) (list-ref Musterung (random 3)) (list-ref Fuehlerform (random 3)) (list-ref Fluegelform (random 3)) (list-ref Fluegelfarbe (random 4)) (list-ref Musterung (random 3)) (list-ref Fuehlerform (random 3)) (list-ref Fluegelform (random 3)) 6)
    generiert zwei zufällige Eltern
|#


; Aufgabe 2


; Eine Methode, die prüft, ob das Kind ein Kind der Eltern sein kann. Es wird intern geprüft, ob das Kind dominantere Merkmale hat, als die beiden Eltern. Wenn nicht, dann kann es ihr Kind sein, sonst nicht.
; Parameter:
;   Flügelfarbe1    Die dominante Flügelfarbe des ersten Elternschmetterlings.
;   Musterung1      Die dominante Musterung des ersten Elternschmetterlings.
;   Fühlerform1     Die dominante Fühlerform des ersten Elternschmetterlings.
;   Flügelform1     Die dominante Flügelform des ersten Elternschmetterlings.
;   Flügelfarbe2    Die dominante Flügelfarbe des zweiten Elternschmetterlings.
;   Musterung2      Die dominante Musterung des zweiten Elternschmetterlings.
;   Fühlerform2     Die dominante Fühlerform des zweiten Elternschmetterlings.
;   Flügelform2     Die dominante Flügelform des zweiten Elternschmetterlings.
;   FlügelfarbeKind Die dominante Flügelfarbe des Kindes.
;   MusterungKind   Die dominante Musterung des Kindes.
;   FühlerformKind  Die dominante Fühlerform des Kindes.
;   FlügelformKind  Die dominante Flügelform des Kindes.
(define (Elterntest Fluegelfarbe1 Musterung1 Fuehlerform1 Fluegelform1 Fluegelfarbe2 Musterung2 Fuehlerform2 Fluegelform2 FluegelfarbeKind MusterungKind FuehlerformKind FluegelformKind)
    (let ([ElternFluegelfarbe (dominantesMerkmal Fluegelfarbe1 Fluegelfarbe2)]
        [ElternMuster (dominantesMerkmal Musterung1 Musterung2)]
        [ElternFuehler (dominantesMerkmal Fuehlerform1 Fuehlerform2)]
        [ElternFluegelform (dominantesMerkmal Fluegelform1 Fluegelform2)])
    (if
        (and
            (equal? (dominantesMerkmal ElternFluegelfarbe FluegelfarbeKind) ElternFluegelfarbe)
            (equal? (dominantesMerkmal ElternMuster MusterungKind) ElternMuster)
            (equal? (dominantesMerkmal ElternFuehler FuehlerformKind) ElternFuehler)
            (equal? (dominantesMerkmal ElternFluegelform FluegelformKind) ElternFluegelform))
        "Dieses Kind kann von diesen Eltern sein."
        "Dieses Kind kann nicht von diesen Eltern sein.")))

#|
Für die Tests gebe ein:
Für Antonia und Anton als Eltern und das Kind
Toni:
    (Elterntest 'blue 'stripes 'curly 'hexagon 'green 'star 'curved 'rhomb 'red 'star 'curly 'rhomb)
Tini:
    (Elterntest 'blue 'stripes 'curly 'hexagon 'green 'star 'curved 'rhomb 'green ' dots 'straight 'rhomb)
Tina:
    (Elterntest 'blue 'stripes 'curly 'hexagon 'green 'star 'curved 'rhomb 'yellow 'stripes 'curved 'ellipse)
|#




































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Das Butterfly Modul
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;#lang racket
(require 2htdp/image)
(provide 
 some-butterflies
 show-butterfly
 set-size!)

(define size 100)
(define kopfbreite (/ size 7))
(define bodycolor 'black)

(define (set-size! s)
  ;setze die Laenge der Schmetterlinge
  (set! size s))

(define (streifen)
  (above
   (rhombus (/ size 4.8) 170 'solid 'black)
   (rhombus (/ size 2.8) 170 'solid 'black)
   (rhombus (/ size 1.8) 170 'solid 'black)
   (rhombus (/ size 2.8) 170 'solid 'black)
   (rhombus (/ size 4.8) 170 'solid 'black)
   ))

(define (punkte)
  (beside
   (overlay 
    (circle (/ size 12.8)  'solid 'black)
    (circle (/ size 9.8)  'solid 'gray)
    )
   (overlay 
    (circle (/ size 7.8)  'solid 'black)
    (circle (/ size 5.8)  'solid 'gray)
    )
   (overlay 
    (circle (/ size 12.8) 'solid 'black)
    (circle (/ size 9.8) 'solid 'gray)
    )))

(define (stern)
 (star-polygon (/ size 5) 5 3 "solid" 'black))

(define (rauten-fluegel farbe)
  (rhombus (/ size 1.5) 120 'solid farbe))

(define (hexagon-fluegel farbe)
  (scale/xy 2 1 (regular-polygon (/ size 3.5) 6 'solid farbe)))

(define (ellipsen-fluegel farbe)
  (ellipse (* size 1.1) (/ size 1.8) 'solid farbe))
 

(define (fluegel-segm muster fluegelform)
  ;zeichne ein Segment des Fluegels als Rhombus
    (if muster (overlay muster fluegelform) fluegelform ))

(define (linker-fluegel muster fluegelform)
  ; setze zwei Rhomben zum linken Fluegel zusammen
  (let ([l_oben 
         (rotate -40 (fluegel-segm muster fluegelform))])
    (above l_oben (flip-vertical l_oben))))


(define (fluegel muster fluegelform)
  ; das Fluegelpaar: spiegele den linken Fluegel als rechten Fluegel
  (let ([links (linker-fluegel muster fluegelform)])
    (beside links  (flip-horizontal links))))

(define (fuehler-krumm)
  ; zeichne gekruemmte Fuehler
  (crop 0 (/ kopfbreite 2) 
        (* 2 kopfbreite) (/ kopfbreite 0.7)
        (circle kopfbreite 'outline bodycolor)))

(define (fuehler-gerade)
  ; zeichne gerade Fuehler
  (crop 0 (* 2 kopfbreite) 
        (* 2 kopfbreite) (* kopfbreite 1.5)
        (rhombus (* kopfbreite 2) 60 'outline bodycolor)))

(define (fuehler-gewellt)
  ; zeichne gewellte Fuehler
  ;  (beside
  (add-curve
   (add-curve 
    (add-curve
     (add-curve
      (rectangle 2 2 'outline 'white)
      0 0 0 1/2
      10 10 0 1/2
      bodycolor)
     
     10 10 0 1/2
     20 20 0 1/2
     bodycolor)
    20 20 0 1/2
    30 10 0 1/2
    bodycolor)
   30 10 0 1/2
   40 0 0 1/2
   bodycolor))

(define (body fuehler)
  ; zeichne Rumpf, Kopf und Fuehler
  (above 
   fuehler
   (circle (/ kopfbreite 2)'solid bodycolor)
   (ellipse kopfbreite ( * size 0.8) 'solid bodycolor)))

(define (butterfly muster fuehler fluegelform)
  (overlay (body fuehler) (fluegel muster fluegelform)))

(define (show-butterfly 
         the_color the_pattern the_feeler-shape the_wing-shape)
  ; the_color: 'red, 'green, 'blue, 'yellow
  ; the_pattern: 'plain, 'stripes, 'dots
  ; the_shape: 'straight, 'curved
  (butterfly
   (case the_pattern
     [(star) (stern)]
     [(stripes) (streifen)]
     [(dots) (punkte)]
     [else #f]); default: uni
   (case the_feeler-shape
     [(straight) (fuehler-gerade)]
     [(curved) (fuehler-krumm)]
     [(curly) (fuehler-gewellt)]
     [else #f])
   (case the_wing-shape
     [(rhomb) (rauten-fluegel the_color)]
     [(hexagon) (hexagon-fluegel the_color)]
     [(ellipse) (ellipsen-fluegel the_color)]
     [else #f])
  ))


;testing
(define some-butterflies
  ; ein Bild mir drei Schmetterlingen Seite an Seite
  (beside 
   (show-butterfly 'red 'stripes 'curved 'rhomb)
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (show-butterfly 'green 'stripes 'straight 'hexagon)
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (show-butterfly 'blue 'dots 'curly 'ellipse)
   ))

(define Antonia-Anton-Toni
  ; ein Bild mir drei Schmetterlingen Seite an Seite
  (beside 
   (above (show-butterfly 'blue 'stripes 'curved 'hexagon)
          (text "Antonia" 24 'black))
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (above (show-butterfly 'green 'star 'curly 'rhomb)
          (text "Anton" 24 'black))
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (above (scale 0.5 (show-butterfly 'red 'star 'curved 'hexagon))
          (text "Toni" 24 'black))
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (above (scale 0.5 (show-butterfly 'green 'dots 'straight 'rhomb))
          (text "Tini" 24 'black))
   (rhombus (/ size 10) 90 'outline 'orange); abstand
   (above (scale 0.5 (show-butterfly 'yellow 'stripes 'curly 'ellipse))
          (text "Tina" 24 'black))
   ))
;(save-image some-butterflies "butterfly-pic.png")
;(save-image Antonia-Anton-Toni "Antonia-Anton-Toni.png")
 ;some-butterflies
 ;Antonia-Anton-Toni
