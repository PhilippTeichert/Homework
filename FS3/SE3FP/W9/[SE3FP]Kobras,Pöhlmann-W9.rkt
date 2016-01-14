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
(displayln "Zeige die Beispielbücher:")
(displayln (cite Beispiel-Buch))
(displayln (cite Beispiel-Sammelband))
(displayln (cite Beispiel-Zeitschriftenartikel))


;;;; Aufgabe 1.3: Ergänzungsmethoden
#|
Bei Ergänzungsmethoden handelt es sich um Wrapper-Funktionen, die den übergebenen Quellcode
vor bzw. nach bzw. sowohl vor als auch nach (abhängig davon, ob 'before', 'after' oder
'around' übergeben wurde) dem 'super call' ausführen, welcher innerhalb der Erg.m. implizit stattfindet.

Vorteile gegenüber dem 'super call' liegen z.b. darin, dass sichergestellt ist, dass
alle Erg.m. ausgeührt werden und die Ausführung somit nicht z.B. an einer fehlenden
Initialisierung scheitert; außerdem müssen geerbte Methoden nicht mit Modifikaitonen
überlagert werden, sondern nur ergänzt.
|#


;;;;;;;; Aufgabe 2: CLOS und Vererbung
;;;; Aufgabe 2.1: Definition von Klassen

;; Supertyp
;; @field medium: Medium, in dem sich das Fahrzeug bewegen kann
;; @field maximalgeschwindigkeit: Höchstgeschwindigkeit des Fahrzeugs
;; @field zuladung: Maximale Tragfähigkeit des Fahrzeugs
;; @field verbrauch: Verbrauch an Treibstoff pro 100km
;; @field n_passagiere: Maximale Anzahl an Passagieren
(defclass fahrzeug ()
  (medium
   :initarg :medium
   :reader read-medium)
  (maximalgeschwindigkeit
   :initarg :vmax
   :reader read-vmax)
  (zuladung
   :initarg :cmax
   :reader read-cmax)
  (verbrauch
   :initarg :verbrauch
   :reader read-verbrauch)
  (passagieranzahl
   :initarg :passagier
   :reader read-passagier))
;; Erben
(defclass landfahrzeug (fahrzeug)
  (medium
   :initvalue 'land
   :reader read-medium))
(defclass wasserfahrzeug (fahrzeug)
  (medium
   :initvalue 'wasser
   :reader read-medium))
(defclass luftfahrzeug (fahrzeug)
  (medium
   :initvalue 'luft
   :reader read-medium))
;; Erben zweiten Grades
(defclass straßenfahrzeug (landfahrzeug)
  (medium
   :initvalue 'straßen
   :reader read-medium))
(defclass schienenfahrzeug (landfahrzeug)
  (medium
   :initvalue 'schienen
   :reader read-medium))
;; Hybriden
(defclass amphibienfahrzeug (landfahrzeug wasserfahrzeug))
(defclass amphibienflugzeug (luftfahrzeug wasserfahrzeug))
(defclass zweiwegefahrzeug (straßenfahrzeug schienenfahrzeug))
(defclass zeitzug (schienenfahrzeug luftfahrzeug))

;; Da alle Medien gewünscht sind, wird append verwendet, um von einem Typen jeweils das eigene als
;; auch das des Supertypen abzufragen
(defgeneric get-medium ((fahrzeug))
  ; Gibt die Bewegungsmedien des gebenenen Fahrzeuges als Liste zurück
  :combination generic-append-combination)
;; Um die Medien als Liste zu bekommen, wird der reader des Feldes medium der list-Funktion übergeben
(defmethod get-medium ((lf landfahrzeug))
  (list (read-medium lf)))
(defmethod get-medium ((wf wasserfahrzeug))
  (list (read-medium wf)))
(defmethod get-medium ((lf luftfahrzeug))
  (list (read-medium lf)))
(defmethod get-medium ((sf straßenfahrzeug))
  (list (read-medium sf)))
(defmethod get-medium ((sf schienenfahrzeug))
  (list (read-medium sf)))
(defmethod get-medium ((af amphibienfahrzeug))
  (list (read-medium af)))
(defmethod get-medium ((af amphibienflugzeug))
  (list (read-medium af)))
(defmethod get-medium ((zf zweiwegefahrzeug))
  (list (read-medium zf)))
(defmethod get-medium ((zz zeitzug))
  (list (read-medium zz)))

;; Da sich die Geschwindigkeiten möglicherweise unterscheiden können,
;; wird nach dem höchstmöglichen Wert gefragt
(defgeneric get-maximalgeschwindigkeit ((fahrzeug))
  ; gibt die Maximalgeschwindigkeit des Fahrzeuges zurück
  :combination generic-max-combination)
;; Da die maximale Kapazität sich je nach Medium unterscheiden kann
;; (Auftrieb und so) und eine höhere Kapazität aus dem falschen Medium
;; fatal sein kann, sollte das Minimum angegeben werden
(defgeneric get-tragfähigkeit ((fahrzeug))
  ; gibt die Tragfähigkeit des Fahrzeugs zurück
  :combination generic-min-combination)
;; Der Verbrauch kann sich zwar je nach Medium ändern (Reibungswiderstand),
;; da uns aber nicht bewusst ist, wie sich das elegant lösen lässt,
;; wird der Verbrauch höchste Verbrauch zurück gegeben, um so die
;; worst-case-Angabe zu haben
(defgeneric get-verbrauch ((fahrzeug))
  ; gibt den Verbrauch des Fahrzeugs zurück
  :combination generic-max-combination)
;; Die Passagieranzahl kann ebenso wie die Tragfähigkeit je nach
;; Medium leicht variieren. Widerum wird der worst-case-Wert abgefragt
(defgeneric get-passagiere ((fahrzeug))
  :combination generic-min-combination)

;; Erstellt ein Beispiel-Amphibienfahrzeug
(define bsp-amphiauto (make amphibienfahrzeug
                            :init-vmax 50
                            :init-cmax 100
                            :init-verbrauch 12
                            :init-passagiere 4))
;; Erstellt ein Beispiel-Zweiwegeefahrzeug
(define bsp-amphiplane (make amphibienflugzeug
                             :init-vmax 800
                             :init-cmax 300
                             :init-verbrauch 30
                             :init-passagiere 10))
;; Erstellt ein Beispiel-Zweiwegefahrzeug
(define bsp-hybrid (make zweiwegefahrzeug
                         :init-vmax 150
                         :init-cmax 130
                         :init-verbrauch 20
                         :init-passagiere 10))
;; erstellt einen Beispiel-Zeitzug
(define bsp-zug (make zeitzug
                      :init-vmax 141.592 ; 88 MPH in Km/h
                      :init-cmax 99999
                      :init-verbrauch 300
                      :init-passagiere 6))

#|
Zeigt das Ergebnis der exemplarisch implementierten get-medium-Methode
Da sowohl die Klassendefinition als auch die Methodendefinition unsauber sind,
kommt ein komisches Ergebnis zurück
Allerdings ist uns nicht klar, wie genau die Definition angepasst werden müsste,
um das richtige Ergebnis zu erhalten
Die implementierte Funktion gibt das Medium des Fahrzeugs konkateniert mit den Medien
der Supertypen zurück. Durch das Überschreiben beim Erben fällt hierbei der Typ
eines der beiden direkten Supertypen weg. Welcher Wert welchen überschreibt, wird
durch die Klassenpräzedenzliste festgelegt (eine diskrete Datensammlung, welche angibt, in
welcher Reihenfolge die Vererbung ausgewertet wird)
|#
(displayln (list "Medien des Amphibienfahrzeuges:" (get-medium bsp-amphiauto)))
(displayln (list "Medien des Amphibienflugzeuges:" (get-medium bsp-amphiplane)))
(displayln (list "Medien des Zweiwegefahrzeuges:" (get-medium bsp-hybrid)))
(displayln (list "Medien des Zeitzuges:" (get-medium bsp-zug)))