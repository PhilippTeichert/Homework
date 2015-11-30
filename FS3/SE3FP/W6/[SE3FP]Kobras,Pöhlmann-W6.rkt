;+++++++++++++++++++++++
;
;;  Kobras    6658699
;;  Pöhlmann  6663579
;
;+++++++++++++++++++++++
;
;;  Seppke/Gr. 9 - Abgabe 30.11.2015
;
;+++++++++++++++++++++++


#lang racket

(require 2htdp/image)

;;;;;;;; Aufgabe 1
#|
linear rekursiv := es gibt keine verzweigte R icosnekursion,
                   es gibt entweder einen rekursiven Aufruf oder es gibt keinen
baum-rekursiv   := bedingter rekursiver Aufruf, der vom Parameter des Aufrufes
                   anhängt (z.B. odd? bzw. even?), oder Teilung der Rechenaufgabe
                   (Divide&Conquer)
geschachtelt    := beim rekursiven Aufruf übergibt sich die Funktion sich selber
                   als Argument
direkt          := die Funktion ruft sich selber rekursiv auf
indirekt        := Funktion a ist genau dann indirekt rekursiv, wenn sie eine
                   Funktion b aufruft, die wiederum Funktion a aufruft

kopfstueck
    linear rekursiv:
        #t, weil der rekursive Aufruf zwar in einem bedingten Block steht,
                 jedoch keine verzweigte Rekursion stattfindet
    baum-rekursiv:
        #f, weil linear
    geschachtelt:
        #f, weil die Funktion sich nicht selber als Argument übergibt
    direkt:
        #t, weil sie sich selbst aufruft und nicht von einer von ihr aufgerufenen
                 Funktion aufgerufen wird
    indirekt:
        #f, weil sie direkt ist
endstueck
    linear rekursiv:
        #t, weil der rekursive Aufruf zwar in einem bedingten Block steht,
                 jedoch keine verzweigte Rekursion stattfindet
    baum-rekursiv:
        #f, weil linear
    geschachtelt:
        #f, weil die Funktion sich nicht selber als Argument übergibt
    direkt:
        #t, weil sie sich selbst aufruft und nicht von einer von ihr aufgerufenen
                 Funktion aufgerufen wird
    indirekt:
        #f, weil direkt
merge
    linear rekursiv:
        #f, weil der rekursive Aufruf abhängig davon, welcher Listenkopf kleiner
                 ist, mit anderen Parametern aufgerufen wird und folglich ein
                 anderes Ergebnis zurückgibt
    baum-rekursiv:
        #t, weil siehe linear (verzweigte Aufrufe)
    geschachtelt:
        #f, weil es sich nicht sich selbst als Argument übergibt
    direkt:
        #t, weil es sich selbst aufruft, anstatt eine andere Funktion aufzurufen,
                 von der es wieder aufgerufen wird
    indirekt:
        #f, weil direkt
merge-sort
    linear rekursiv:
        #f, weil die aufgeteilte Liste in einer Verzweigung sortiert wird
    baum-rekursiv:
        #t, weil die Liste zum Sortieren aufgeteilt wird
    geschachtelt:
        #f, weil die Funktion sich nicht selber als Argument übernimmt
    direkt:
        #t, weil merge-sort sich selbst aufruft und erst ausgewertet wird,
            bevor es merge übergeben wird
    indirekt:
        #f, weil direkt
|#

;;;; Aufgabe 2.1
;; @param num Die einzuordnende Zahl
;; @param lst Die Liste, in die num einsortiert werden soll
(define (insertion-sort num lst)
  (cond
    [(or (empty? lst) (<= num (first lst))) (cons num lst)]
    [else (cons (first lst) (insertion-sort num (rest lst)))]))
; Test (insertion-sort 6 '(1 2 4 8 9 11)) ergibt '(1 2 4 6 8 9 11).

;;;; Aufgabe 2.2
;; @param comparator Der zu verwendende Vergleichsoperator
;; @param lst Die zu sortierende Liste
(define (quick-sort comparator lst)
  (match lst
    ['() '()]
    [(cons x xs) 
     (let-values ([(xs-gte xs-lt) (partition (curry comparator x) xs)])
       (append (quick-sort comparator xs-lt) 
               (list x) 
               (quick-sort comparator xs-gte)))]))
; Test 1: (quick-sort < '(8 9 3 4 6 1 2)) gibt '(1 2 3 4 6 8 9).
; Test 2: (quick-sort > '(8 9 3 4 6 1 2)) gibt '(9 8 6 4 3 2 1).


;;;; Aufgabe 2.3
(define icons
  (list
   (star-polygon 35 5 2 "solid" "gold")
   (ellipse 44 44 "solid" "red")
   (rectangle 38 38 "solid" "blue")
   (isosceles-triangle 45 65 "solid" "darkgreen")))



;;;;;;;; Aufgabe 3

; Erzeugt einen schwaren Hintergrund.
(define bgimg (empty-scene 800 600 (color 0 0 055 220)))
; Erzeugt die Null-Ebene, auf der die Objekte platziert werden.
(define null-plain (rectangle 800 600 "solid" (color 0 0 0 0)))

; Erzeugt einen Rahmen ums Bild.
(define border-upper
  (place-image (rectangle 1900 25 "solid" (color 0 0 0 55))800 0 null-plain))
(define border-left
  (place-image (rectangle 25 1900 "solid" (color 0 0 0 55))0 600 null-plain))
(define border-lower
  (place-image (rectangle 1900 25 "solid" (color 0 0 0 55))800 600 null-plain))
(define border-right
  (place-image (rectangle 25 1900 "solid" (color 0 0 0 55))800 00 null-plain))

; Erzeugt die Landschaft im Hintergrund.
(define ambient (underlay
                    (place-image (ellipse 2000 550 "solid" (color 255 255 255 150)) 300 500 null-plain)
                    (place-image (rectangle 1200 450 "solid" (color 255 255 255 180)) 300 500 null-plain)
                    (place-image (ellipse 1400 400 "solid" (color 255 255 255 180)) 300 500 null-plain)
                    (place-image (rectangle 1200 300 "solid" (color 245 245 245 255)) 300 500 null-plain)))

; Erzeugt den Wald im Hintergrund.
(define trees
  (above/align "center"               
               (overlay/xy
                (overlay/xy
                 (overlay/xy
                  (rectangle 1 1 "solid" (color 0 0 0 0))                
                  6 11
                  (triangle 10  "solid" (color 0 155 0 255)))
                 4 15
                 (triangle 15  "solid" (color 0 155 0 255)))
                2 20
                (triangle 20  "solid" (color 0 155 0 255)))
               (rectangle 5 10 "solid" "brown")))


; Erzeugt einen Tannenwald. Bäume werden zufällig an passenden Orten verteilt.
; @paramm treenum Die Anzahl der zu platzierenden Bäume
; @param bgimg Die Ebene, auf der die Bäume zu platzieren sind
(define (forest treenum bgimg)
  (if (= treenum 1)
      (place-image trees (random 400) (+ 245 (random 150)) bgimg)
      (underlay (place-image trees (+ 20(random 740)) (+ 245 (random 90)) bgimg)
                (forest (- treenum 1) bgimg))))

; Erzeugt Sterne mit Unschärfe und Koronasphäre
; 'my-star' statt 'star', weil 'star' bereits in 2htdp enthalten ist
(define my-star (underlay
               (star 15 "solid" (color 255 250 0 70))
               (radial-star 8 4 10 "solid" "orange")
               (star 8 "solid" "yellow")))

; Erzeugt einen Mond mit Koronasphäre
(define moon (underlay
              (circle 50 "solid" (color 225 225 0 30))
              (circle 30 "solid" (color 225 225 225 255))))

; Erzeugt den Sternenhimmel.
; @param starnum Anzahl der Sterne
; @param bgimg Die Ebene, auf der die Sterne platziert werden sollen

(define (sky starnum bgimg)
  (if (= starnum 1)
      (place-image my-star (+ (random 750) 100) (+ 30 (random 180)) bgimg)
      (underlay (place-image (rotate (+ 1 (random 659)) (scale (/ (+ (random 99) 1) 100) my-star)) (+ (random 750) 25) (+ 30 (random 180)) bgimg)
                (sky (- starnum 1) bgimg))))

; Erzeugt ein skalierbares Haus.
; Position der Fenster dabei leicht variabel
; @param scale Skalierungsfaktor für unterschiedlich große Häuser
(define (house scale)
  (above/align
              "center"
              (underlay/xy
               (isosceles-triangle (* 70 scale) (* 90 scale) "solid" (color 255 0 0 200))
               (* 42 scale) (* 20 scale)
               (circle (* 8 scale) "solid" "yellow"))
              (underlay/xy (square (* 70 scale) "solid" "brown")
                           (* 32 scale) (* 15 scale)
                           (square (* 20 scale) "solid" "yellow"))))

; Erzeugt Weihnachtskugeln mit einem RGB-Farbwert.
(define (ball color1 color2 color3)
  (above/align "center" 
               (ellipse 5 2 "solid" "gold")
               (ellipse 20 20 "solid" (color color1 color2 color3))))

; Erzeugt einen Weihnachtsbaum mit 6 Kugeln. Kugeln bekommen zufällige Farben gegeben. Jede Kugel ist auf einer eigenen Grafikebene angepint
; cm-tree für ChristMasTree
(define cm-tree (underlay/xy
                        (underlay/xy
                         (underlay/xy
                          (underlay/xy
                           (underlay/xy
                            (underlay/xy
                             (above/align "center"    ; Baum ohne Kugeln
                                          (overlay/xy
                                           (overlay/xy
                                            (overlay/xy
                                             (overlay/xy
                                              (overlay/xy
                                               (rectangle 1 1 "solid" (color 0 0 0 0))
                                               40 2
                                               (radial-star 6 8 30 "solid" (color 255 155 0 255)))
                                              30 30
                                              (isosceles-triangle 60 70 "solid" "darkgreen"))
                                             20 50
                                             (isosceles-triangle 80 70 "solid" "darkgreen"))
                                            10 70
                                            (isosceles-triangle 100 70 "solid" "darkgreen"))
                                           0 90
                                           (isosceles-triangle 120 70 "solid" "darkgreen"))
                                          (rectangle 30 40 "solid" "brown")) ; Ende Baum
                             40 100
                             (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155))))
                            60 70
                            (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155))))
                           70 120
                           (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155))))
                          90 150
                          (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155))))
                         20 160
                         (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155))))
                        45 135
                        (ball (+ 100 (random 155)) (+ 100 (random 155)) (+ 100 (random 155)))))


; Erzeugt das Gesamtbild
(define paint (underlay
              ; Hintergrund Anfang
              bgimg
              ambient
              ; Hintergrund Ende
              ; Rahmen um das Bild Anfang
              border-upper
              border-left
              border-lower
              border-right
              ; Rahmen um das Bild Ende
              ; Landschaft Anfang
              (forest 120 null-plain)              
              (sky 205 null-plain)
              ; Landschaft Ende
              ; Hauptelemente Anfang
              ; Syntax: place-igame image-to-place x y parent-plain
              (place-image moon 600 75 null-plain)
              (place-image (house 0.8) 650 500 null-plain)
              (place-image (house 1) 100 500 null-plain)
              (place-image cm-tree 300 470 null-plain)
              (place-image cm-tree 500 400 null-plain)
              ;Hauptelemente Ende
              ))

(print "Merry Christmas! :-)")
(printf "\n")
(print (color-frame "black" paint))
(printf "\n")
(print "Alpha Mike Foxtrott! ;-)")
