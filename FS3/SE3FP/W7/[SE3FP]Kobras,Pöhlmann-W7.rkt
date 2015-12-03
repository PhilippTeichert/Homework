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
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (n (length list))
           (step (/ laenge (- n 1)))]
      ;(apply
       ;(lambda (x) (+ x start))
       (build-list n (lambda (x) (+ (* x step) start))))));)
;(cons start (f start))
;(rescale1d (range '(0 . 10) 40) '(10 . 50))
 
;;;;Aufgabe 2.2
;;Gegeben eine Liste an Pairs aus Aufgabe 2.1 und zwei Intervall als Pairs der erste Werte eines Pairs aus der Liste von Pairs wird werden auf das erste intervall abgebildet, der zweite auf das zweite
;;Inklusive der ersten Zahl und inklusive der letzten
(define (rescale2d list interval1 interval2)
  (let [(list1 (map car list))
        (list2 (map cdr list))]
    (einslinkseinsrechts
     (rescale1d list1 interval1)
     (rescale1d list2 interval2))))

;;;;Hilfsfunktion zur Listenbildung
;; bildet aus 2 lsiten eine liste aus paaren, wobei das element an stelle x in liste 1 als car des elements an stelle x in der ausgabelsite zu finden ist. für liste 2 analog mit cdr
(define (einslinkseinsrechts list1 list2)
  (if (empty? list1)
      '()
      (cons
       (cons (car list1) (car list2))
       (einslinkseinsrechts (cdr list1) (cdr list2)))))
      
  #|(let [(start1 (car interval1))
        (stop1 (cdr interval1))
        (start2 (car interval2))
        (stop2 (cdr interval2))]
    (let* [(laenge1 (- stop1 start1))
           (laenge2 (- stop2 start2))
           (n (length list))
           (step1 (/ laenge1 (- n 1)))
           (step2 (/ laenge2 (- n 1)))]
      ;(apply
       ;(lambda (x) (+ x start))
       (build-list '(n . n) (lambda (x) (cons (+ (* (car x) step1) start1) (+ (* (cdr x) step2) start2)))))));)|#
;(cons start (f start))
;(rescale2d (function->points sqr '(0 . 10) 5) '(10 . 50) '(5 . 25))