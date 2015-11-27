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
linear rekursiv := es gibt keine verzweigte Rekursion,
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

























         