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


#lang swindle

;;;;;;;; Aufgabe 1: CLOS und generische Funktionen

;;;; Aufgabe 1.1: Definition von Klassen

;; Erzeugt einen Key für ein Schriftstück
;; @param autor Autor des Schriftstückes
;; @param jahr Erscheinungsjahr des Schriftstückes
;; @param titel Titel des Schriftstückes
(define (create-key autor jahr titel)
  (string-append titel autor (number->string jahr)))

;; Schablonenklasse für verschiedene Veröffentlichungen
;; @field key: ein eindeutiger Schlüssel
;; @field autor: Name der Autorinnen und Autoren
;; @field jahr: Das Erscheinungsjahr
;; @field titel: Der Titel der Veröffentlichung
(defclass literaturbeitrag ()
  (key
   :reader read-key
   :initarg :init-key)
  (autor
   :reader read-autor
   :initarg :init-autor)
  (jahr
   :reader read-jahr
   :initarg :init-jahr)
  (titel
   :reader read-titel
   :initarg :init-titel))

;; Schablonenklasse für Bücher
;; Erbt von 'literaturbeitrag'
;; @field verlag: Verlag des Buches
;; @field verlagsort: Standort des Verlages
;; @field reihe: Reihe, in der das Buch erschienen ist
;; @field seriennummer: Seriennummer des Buches in der Reihe
(defclass buch (literaturbeitrag)
  (verlag
   :reader read-verlag
   :initarg :init-verlag)
  (verlagsort
   :reader read-verlagsort
   :initarg :init-verlagsort)
  (reihe
   :reader read-reihe
   :initarg :init-reihe)
  (seriennummer
   :reader read-seriennummer
   :initarg :init-seriennummer))

;; Schablonenklasse für Sammelbänder
;; Erbt von 'buch' und damit implizit auch von 'literaturbeitrag'
;; @field herausgeber: Name des Herausgebers
;; @field seitenangabe: Seitenangabe zum Artikel im Buch
(defclass sammelband (buch)
  (herausgeber
   :reader read-herausgeber
   :initarg :init-herausgeber)
  (seitenangabe
   :reader read-seitenangabe
   :initarg :init-seitenangabe))

;; Schablonenklasse für Zeitschriftenartikel
;; @field zeitschrift: Name der Zeitschrift
;; @field bandnummer: Nummer des Bandes
;; @field heftnummer: Nummer des Heftes
;; @field erscheinungsmonat: Erscheinungsmonat
(defclass zeitschriftenartikel (literaturbeitrag)
  (zeitschrift
   :reader read-zeitschrift
   :initarg :init-zeitschrift)
  (bandnummer
   :reader read-bandnummer
   :initarg :init-bandnummer)
  (heftnummer
   :reader read-heftnummer
   :initarg :init-heftnummer)
  (erscheinungsmonat
   :reader read-erscheinungsmonat))

;; Erstellt das Objekt Nessie-Buch
(define Beispiel-Buch (make buch
                            :init-key (create-key "Nessie" 1790 "Mein Leben im Loch Ness: Verfolgt als Ungeheuer")
                            :init-autor "Nessie"
                            :init-jahr 1790
                            :init-titel "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
                            :init-verlag "Minority-Verlag"
                            :init-verlagsort "Inverness"
                            :init-reihe "Die besondere Biographie"
                            :init-seriennummer "Band 2"))

;; Erstellt das Objekt Ford-Buch
(define Beispiel-Sammelband (make sammelband
                                  :init-key (create-key "Prefect, F." 1979 "Mostly harmless - some observertions concerning the third planet of the solar system")
                                  :init-autor "Prefect, F."
                                  :init-jahr 1979
                                  :init-titel "Mostly harmless - some observertions concerning the third planet of the solar system"
                                  :init-verlag "Galactic Press"
                                  :init-verlagsort "Vega-System, 3rd planet"
                                  :init-reihe "Travel in Style"
                                  :init-seriennummer "Volume 5"
                                  :init-herausgeber "Adams, D."
                                  :init-seitenangabe 420))

;; Erstellt das Objekt Wells-Buch
(define Beispiel-Zeitschriftenartikel (make zeitschriftenartikel
                                            :init-key (create-key "Wells, H. G." 3200 "Zeitmaschinen leicht gemacht")
                                            :init-autor "Wells, H. G."
                                            :init-jahr 3200
                                            :init-titel "Zeitmaschinen leicht gemacht"
                                            :init-zeitschrift "Heimwerkerpraxis für Anfänger"
                                            :init-bandnummer 3
                                            :init-heftnummer 500))


;;;; Aufgabe 1.2: Generische Funktionen und Methoden
;; Generic 'cite' als Funktionsvorlage.
;; Durch :combination wird an das Ende eines cites der cite des direkten Supertypen angehangen
(defgeneric cite ((literaturbeitrag))
  :combination generic-append-combination)

;; Implementation von 'cite' für einen Literaturbeitrag (Signatur des Beitrages).
;; Form: 'Autor (Jahr): Titel'
(defmethod cite ((lb literaturbeitrag))
  (list (string-append (read-autor lb) " (" (number->string (read-jahr lb)) "): " (read-titel lb))))

;; Implementation von 'cite' für ein Buch.
;; Form: 'Verlag, Verlagsort; Reihe Bandnummer: Signatur'
(defmethod cite ((b buch))
  (list (string-append (read-verlag b) ", " (read-verlagsort b) "; " (read-reihe b) " " (read-seriennummer b) ": ")))

;; Implementation von 'cite' für einen Sammelband.
;; Form: 'Herausgeber, !Seite Nummer, Buch
(defmethod cite ((sb sammelband))
  (list (string-append (read-herausgeber sb)", Seite " (number->string (read-seitenangabe sb)) ", ")))

;; Implementation von 'cite' für einen Zeitschriftenartikel.
;; Form: 'Heft Heftnummer(Bandnummer): Signatur'
(defmethod cite ((za zeitschriftenartikel))
  (list (string-append (read-zeitschrift za) " " (number->string (read-heftnummer za)) "(" (number->string (read-bandnummer za)) "): ")))

;; Druckt in die Konsole die Cites der Beispiel-Literaturen
(displayln (cite Beispiel-Buch))
(displayln (cite Beispiel-Sammelband))
(displayln (cite Beispiel-Zeitschriftenartikel))