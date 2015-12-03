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

;;;;Aufgabe 2.1
;;Gegeben eine Liste an Werten aus Aufgabe 1 ein Intervall als Pair die Werte werden auf das neue intervall abgebildet
;;Inklusive der ersten Zahl und inklusive der letzten
(define (rescaled list interval)
  (let [(start (car interval))
        (stop (cdr interval))]
    (let* [(laenge (- stop start))
           (n (length list))
           (step (/ laenge n))]
      ;(apply
       ;(lambda (x) (+ x start))
       (build-list n (lambda (x) (+ (* x step) start))))));)
;;(cons start (f start))