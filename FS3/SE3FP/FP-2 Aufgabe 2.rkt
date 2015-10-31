#lang racket

;Aufgabe 2.1
;n ist die Zahl, von der die Fakultät berechnet werden soll.
(define (fak n)
  (if (= n 0)
      1
      (* n (fak (- n 1)))))

;Aufgabe 2.2
;r ist die Zahl, die hoch n genommen werden soll.
(define (power r n)
  (if (= n 0)
      1
      (if (odd? n)
          (* r (power r (- n 1)))
          (if (even? n)
              (sqr (power r (/ n 2)))
              "n ist keine natürliche Zahl"))))

;Aufgabe 2.3
;Die Abbruchbedingung für e. Kann nach belieben verändert werden.
(define abbruchbedingung1
  (/ 1 (power 10 1000)))

;Der rekursive Teil der Berechnung. Abbruchbedingung ist, dass der Bruch kleiner wird, als 10^1000.
;n ist die Zahl über dem Bruchstrich.
(define (rek_e n)
  (if (> (/ n (fak (- n 1))) abbruchbedingung1)
      (+ (/ n (fak (- n 1)))
         (rek_e (+ n 1)))
      0))

;Berechnet die Eulerzahl rekursiv.
(define E
  (/ (rek_e 1) 2))

;Anzeigemethode für die ersten 1000 Nachkommastellen.
(define zeige_e
  (* E (power 10 1001)))

;Aufgabe 2.4
;Diese Folge konvergiert nicht so schnell, wie die von e, weil der Teil unter dem Bruch bei e bedeutend schnell kleiner wird.
;Die Abbruchbedingung für pi. Kann nach belieben verändert werden.
(define abbruchbedingung2
  (/ 1 (power 10 4)))

;Der rekursive Teil der Berechnung. Abbruchbedingung ist, dass der Bruch kleiner wird als 10^4.
(define (rek_pi n)
  (if (> (/ 1 (+ (* 2 n) 1)) abbruchbedingung2)
      (- (/ 1 (+ (* 2 n) 1))
         (rek_pi (+ n 1)))
      0))

;Berechnet Pi rekursiv.
(define PI
  (* (- 1 (rek_pi 1)) 4))

;Anzeigemethode für die ersten 1000 Nachkommastellen.
(define zeige_pi
  (* PI (power 10 1001)))