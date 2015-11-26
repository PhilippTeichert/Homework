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

;;;;;;;; Aufgabe 1
#|
kopfstueck
    linear rekursiv:
        #t, weil ...
    baum-rekursiv:
        #f, weil ...
    geschachtelt:
        #f, weil ...
    direkt:
        #t, weil ...
    indirekt:
        #f, weil ...
endstueck
    linear rekursiv:
        #t, weil ...
    baum-rekursiv:
        #f, weil ...
    geschachtelt:
        #f, weil ...
    direkt:
        #t, weil ...
    indirekt:
        #f, weil ...
merge
    linear rekursiv:
        #t, weil ...
    baum-rekursiv:
        #f, weil ...
    geschachtelt:
        #f, weil ...
    direkt:
        #t, weil ...
    indirekt:
        #f, weil ...
merge-sort
    linear rekursiv:
        #f, weil ...
    baum-rekursiv:
        #t, weil ...
    geschachtelt:
        #t, weil ...
    direkt:
        #f, weil ...
    indirekt:
        #f, weil ...
|#

;;;;;;;; Aufgabe 2
;;;; Aufgabe 2.1
(define (insertion-sort lst)
  (echo "done"))
;;;; Aufgabe 2.2 (Zusatzaufgabe)
(define (quicksort lst)
  (echo "done"))
;;;; Aufgabe 2.3



;;;;;;;; Aufgabe 3







;;;;;;;; Hilfsfunktionen
(define (echo str)
  (display str))