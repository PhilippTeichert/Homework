%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								%
%%	Kobras		6658699		   %%
%%	Pöhlmann	6663579		   %%
%								%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%
%
%1.
%    Y = x,
%    X = y.
%    Da eine Variablenbindung möglich ist.
%    Jeder "Großbuchstabe" kann an einen "Kleinbuchstaben" gebunden werden, ohne dass er an 2 unterschiedliche gebunden werden müsste.
%
%2.
%    false.
%    Da eine variablenbindung nicht möglich ist.
%    Es müsste Z = r und zugleich Z = i gelten (Z ist doppelt gebunden).
%
%3.
%    F = m,
%    H = k.
%    Da eine Variablenbindung möglich ist.
%    Jeder "Großbuchstabe" kann an einen "Kleinbuchstaben" gebunden werden, ohne dass er an 2 unterschiedliche gebunden werden müsste.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variable und Atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%4.
%    false.
%    Das linke m hat 3 Argumetpositionen und das rechte 4.
%    Daher kann keine Unifikation stattfinden.
%
%5.
%    false.
%    Denn "=" wertet nicht aus.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Richtig, was tut es stattdessen?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%6.
%    False = not(true).
%    Denn "False" ist eine Variable, mit der etwas unifiziert werden kann, da es groß geschrieben ist.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%



%%%%% Aufgabe 2.1

% nachfolger(?Peanozahl, ?Nachfolger).
% 'Peanozahl' und 'Nachfolger' sind Argumentpositionen,
% so dass 'Nachfolger', der Nachfolger von 'Peanozahl' ist.

nachfolger(0, s(0)).
nachfolger(s(X), s(Y)) :-
    nachfolger(X, Y).
    
% vorgaenger(?Peanozahl, ?Vorgaenger).
% 'Peanozahl' und 'Vorgaenger' sind Argumentpositionen,
% so dass 'Vorgaenger', der Vorgaenger von 'Peanozahl' ist.

vorgaenger(s(0), 0).
vorgaenger(s(X), s(Y)):-
    vorgaenger(X, Y).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ihr braucht gar kein extra Prädikat dafür, ihr habt doch schon nachfolger ;)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% subtrahiere(?Minuend, ?Subtrahend, ?Differenz).
% 'Minuend', 'Subtrahend' und 'Differenz' sind Argumentpositionen,
% so dass 'Differenz', die Differenz aus 'Minuend' und 'Subtrahend' ist.

subtrahiere(X, Y, G) :-
    Y = 0,
    G = X.
%%%% Kürzere Schreibweise: subtrahiere(X, 0, X).
subtrahiere(s(X), s(Y), G) :-
    subtrahiere(X, Y, G).
    
% verdoppelt(?Peanozahl, ?Doppelt).
% 'Peanozahl' und 'Doppelt' sind Argumentpositionen,
% so dass 'Doppelt', doppelt so groß wie 'Peanozahl' ist.

verdoppelt(X, Y) :-
    X = 0,
    Y = 0.
verdoppelt(s(X), s(s(Y))) :-
    verdoppelt(X, Y).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auch hier kürzer: verdoppelt(0,0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% modulo(?Dividend, ?Divisor, ?Modulo).
% 'Dividend', 'Divisor' und 'Modulo' sind Argumentpositionen,
% so dass 'Modulo', das Ergebnis ein Rechnung von 'Dividend' modulo 'Divisor' ist.

% X mod 1 = 0
% X mod X = 0
% X mod Y = 
modulo(X, Y, G) :-
    Y > 0,
    X < Y,
    G = X.
modulo(X, Y, G) :-
    Y > 0,
    X >= Y,
    X1 is X - Y,
    modulo(X1, Y, G).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modulo soll auch auf Peano-Zahlen agieren!
% - 2 Punkte
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% p2i(?Peanozahl, +Integer).
% 'Peanozahl' und 'Integer' sind Argumentpositionen,
% so dass 'Integer', das Integer-Pendant zu 'Peanozahl' ist.
% Anmerkung: 'Integer' hat eigentlich ein ?, wenn man allerdings eine Variable anstelle einer konkreten Zahl eingibt,
% rechnet das Programm, nachdem es die erste Lösung gefunden hat, unendlich weiter.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bei mir nicht.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p2i(X, Y) :-
    X = 0,
    Y = 0.
%%%% kürzer: p2i(0,0).
p2i(s(X), Y) :-
    p2i(X, Z),
    Y is Z + 1.

	
%%%%% Aufgabe 2.2

% add(?Summand1,?Summand2,?Summe)
% Summand1, Summand2 und Summe sind Peano-Terme,
% so dass gilt: Summand1 + Summand2 = Summe
add(0,X,X). % Rekursionsabschluss
add(s(X),Y,s(R)):-add(X,Y,R). % Rekursionsschritt

% lt(?Term1,?Term2)
% Term1 und Term2 sind Peano-Terme, so dass Term1
% kleiner als Term2
lt(0,s(_)).
lt(s(X),s(Y)):-lt(X,Y).

% peano(+Term)
% Term ist ein Peano-Term
peano(0). % Rekursionsabschluss
peano(s(X)) :- peano(X). % Rekursionsschritt

% addTyptest(?Summand1,?Summand2,?Summe)
% Summand1, Summand2 und Summe sind Peano-Terme,
% so dass gilt: Summand1 + Summand2 = Summe
addTyptest(0,X,X) :-
    peano(X). % Rekursionsabschluss
addTyptest(s(X),Y,s(R)) :-
    peano(X),
    peano(Y),
    addTyptest(X,Y,R). % Rekursionsschritt
% Dieser Typtest sichert zu, dass es sich bei den beiden Summanden um Peano-Terme handelt.
% Ohne sie konnte man "add(0, irgendwas, X)" eingeben und es wurde X mit irgendwas unifiziert.
% Nun müssen alle Eingaben ein Peano-Term oder eine Variable sein, damit nicht grundsätzlich false zurück kommt.

% ltTyptest(?Term1,?Term2)
% Term1 und Term2 sind Peano-Terme, so dass Term1
% kleiner als Term2
ltTyptest(0,s(X)) :-
    peano(X).
ltTyptest(s(X),s(Y)) :-
    peano(X),
    peano(Y),
    ltTyptest(X,Y).
% Dieser Typtest sichert zu, dass es sich bei den beiden Termen um Peano-Terme handelt.
% Ohne sie konnte man "lt(0, s(irgendwas))" eingeben und es wurde "true" ausgegeben.
% Nun müssen beide Eingaben ein Peano-Term oder eine Variable sein, damit nicht grundsätzlich false zurück kommt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Der Typtest instanziiert die Argumente bei Bedarf,
% dadurch werden mögliche Lösungen aufgezählt!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10/12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
% AUFGABE 3 %
%%%%%%%%%%%%%

% Stuff aus Aufgabenblatt 4:

% liegt_stromaufwaerts_von(Fluss,Ort1,Ort2,Distanz)
% ist wahr, wenn fuer zwei an einem Fluss liegende
% Ortschaften die natuerliche Fliessrichtung des Wassers von
% Ort1 zu Ort2 verlaeuft 
% Distanz ist die Entfernung zwischen diesen Orten in Kilometern

stromaufwaerts(moldau,praha,muendung_moldau,38).
stromaufwaerts(elbe,muendung_moldau,usti,70). 
stromaufwaerts(elbe,usti,dresden,93).
stromaufwaerts(elbe,dresden,meissen,26).
stromaufwaerts(elbe,meissen,torgau,73).

stromaufwaerts(elbe,torgau,rosslau,102).
stromaufwaerts(elbe,rosslau,muendung_saale,33).
stromaufwaerts(elbe,muendung_saale,magdeburg,35).
stromaufwaerts(elbe,magdeburg,tangermuende,63).
stromaufwaerts(elbe,tangermuende,muendung_havel,34).

stromaufwaerts(elbe,muendung_havel,wittenberge,31).
stromaufwaerts(elbe,wittenberge,schnackenburg,21).
stromaufwaerts(elbe,schnackenburg,geesthacht,111).
stromaufwaerts(elbe,geesthacht,hamburg,22).
stromaufwaerts(elbe,hamburg,muendung_elbe,125).

stromaufwaerts(saale,calbe,muendung_saale,20).
stromaufwaerts(saale,bernburg,calbe,16).
stromaufwaerts(saale,halle,bernburg,57).
stromaufwaerts(mulde,bitterfeld,rosslau,27).
stromaufwaerts(mulde,wurzen,bitterfeld,47).

stromaufwaerts(havel,havelberg,muendung_havel,3).
stromaufwaerts(havel,rathenow,havelberg,42).
stromaufwaerts(havel,brandenburg,rathenow,47).
stromaufwaerts(havel,muendung_spree,brandenburg,55).
stromaufwaerts(spree,berlin_mitte,muendung_spree,14).

stromaufwaerts(oder,muendung_neisse,eisenhuettenstadt,11).
stromaufwaerts(oder,eisenhuettenstadt,frankfurt_oder,33).
stromaufwaerts(oder,frankfurt_oder,schwedt,111).
stromaufwaerts(oder,schwedt,szczecin,42).
stromaufwaerts(oder,szczecin,swinoujscie,61).

% ist_betroffen_von(?Ort1, ?Ort2)
% 'Ort1' und 'Ort2' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt.
% terminierungssicher, da nach einmaligem Prüfen der Datenbasis die Ausführung beendet ist

ist_betroffen_von(Ort1,Ort2):-
	stromaufwaerts(_,Ort2,Ort1,_).

ist_betroffen_von(Ort1,Ort2):-
	stromaufwaerts(_,OrtX,Ort1,_),
	ist_betroffen_von(OrtX,Ort2).

%%%%% Aufgabe 3.1

% ist_betroffen_von_entfernung(?Ort1, ?Ort2, ?Entfernung)
% 'Ort1', 'Ort2' und 'Entfernung' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt, und 'Entfernung' die Entfernung der beiden Orte ist.

ist_betroffen_von_entfernung(Ort1,Ort2, Entfernung):-
	stromaufwaerts(_,Ort2,Ort1,Entfernung).

ist_betroffen_von_entfernung(Ort1,Ort2, Entfernung):-
	stromaufwaerts(_,OrtX,Ort1,Entfernung1),
	ist_betroffen_von_entfernung(OrtX,Ort2, Entfernung2),
    Entfernung is Entfernung1 + Entfernung2.

%%%%% Aufgabe 3.2

% fließgeschwindigkeit(?Fluss, ?Fließgeschwindigkeit).
% 'Fluss' und 'Fließgeschwindigkeit' sind Argumentpositionen,
% so dass 'Fließgeschwindigkeit' die ungefähre Fließgeschwindigkeit im Fluss 'Fluss' in Kiilometer pro Stunde ist.
fließgeschwindigkeit(elbe, 5).
fließgeschwindigkeit(oder, 4).
fließgeschwindigkeit(moldau, 3).
fließgeschwindigkeit(saale, 2).
fließgeschwindigkeit(mulde, 7).
fließgeschwindigkeit(havel, 0.5).
fließgeschwindigkeit(spree, 0.3).

% ist_betroffen_von_zeit(?Ort1, ?Ort2, ?Zeit)
% 'Ort1', 'Ort2' und 'Zeit' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt, und 'Zeit' die Zeit in Stunden ist, nach der 'Ort1' vom Abwasser von 'Ort2' betroffen ist.

ist_betroffen_von_zeit(Ort1,Ort2, Zeit):-
	ist_betroffen_von_entfernung(Ort1, Ort2, Entfernung),
    ist_betroffen_von_name(Ort1, Ort2, Name),
    fließgeschwindigkeit(Name, Geschwindigkeit),
    Zeit is div(Entfernung, Geschwindigkeit).
    

% ist_betroffen_von_name(?Ort1, ?Ort2, ?Name)
% 'Ort1', 'Ort2' und 'Name' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt, und 'Name' der Name des Flusses ist.
ist_betroffen_von_name(Ort1,Ort2, Name):-
	stromaufwaerts(Name,Ort2,Ort1,_).

ist_betroffen_von_name(Ort1,Ort2, Name):-
	stromaufwaerts(Name,OrtX,Ort1,_),
	ist_betroffen_von(OrtX,Ort2).

%%%%%%%%%%%%%
% AUFGABE 4 %
%%%%%%%%%%%%%

% Stuff aus Aufgabenblatt 3:

% directory(DirId,Name,ParentId,DateCreated,DateModified)

directory(1,root,0,date(2007,5,2),date(2007,5,2)).
directory(2,bilder,1,date(2007,5,2),date(2009,11,2)).
directory(3,musik,1,date(2007,5,2),date(2009,10,4)).
directory(4,dokumente,1,date(2007,5,2),date(2009,11,5)).
directory(5,urlaub,2,date(2008,6,22),date(2009,8,15)).
directory(6,hochzeit,2,date(2008,1,27),date(2008,1,31)).
directory(7,kinder,2,date(2007,5,2),date(2009,9,5)).
directory(8,klassik,3,date(2007,5,2),date(2007,5,2)).
directory(9,pop,3,date(2007,5,2),date(2009,11,5)).
directory(10,urlaub,4,date(2008,5,23),date(2008,11,1)).
directory(11,hochzeit,4,date(2007,12,4),date(2008,1,25)).
directory(12,scheidung,4,date(2009,9,2),date(2009,11,5)).

% file(FileId,DirId,Name,Size,DateCreated,DateModified)

file(1,9,in_the_summertime,56,date(2007,5,2),date(2009,11,5)).
file(2,9,i_am_so_romantic_tonight,31,date(2007,5,2),date(2009,11,5)).
file(3,9,ich_und_du_fuer_immer,67,date(2007,5,2),date(2009,11,5)).
file(4,9,paris,267,date(2008,6,3),date(2008,6,3)).
file(7,10,quartieranfrage,1,date(2007,5,2),date(2009,11,5)).
file(13,5,paris,251,date(2008,6,22),date(2008,6,17)).
file(14,5,dijon,217,date(2008,6,22),date(2008,6,17)).
file(15,5,die_bruecke_von_avignon,191,date(2008,6,22),date(2008,6,17)).
file(21,6,polterabend,238,date(2008,1,27),date(2008,1,31)).
file(22,6,standesamt,244,date(2008,1,27),date(2008,1,31)).
file(23,6,kirche,158,date(2008,1,28),date(2008,1,31)).
file(24,6,festessen,151,date(2008,1,28),date(2008,1,31)).
file(25,11,standesamt,33,date(2007,6,3),date(2007,6,3)).
file(34,12,scheidungsklage,48,date(2009,9,2),date(2009,11,5)).

%%%%% Aufgabe 4.1

% zugriffspfad(?VerzeichnisName1, ?VerzeichnisName2)
% 'VerzeichnisName1' und 'VerzeichnisName2' sind Argumentpositionen,
% so dass 'VerzeichnisName1' der Name des Verzeichnisses ist, welches im Zugriffspfad vom Verzeichnis mit dem Namen 'VerzeichnisName2' liegt.
zugriffspfad(VerzeichnisName1, VerzeichnisName1) :-
    directory(_, VerzeichnisName1, _, _, _).
zugriffspfad(VerzeichnisName1, VerzeichnisName2) :-
    istUnterverzeichnisVon(VerzeichnisName1, VerzeichnisName2);
    istUnterverzeichnisVon(VerzeichnisName2, VerzeichnisName1).

% istUnterverzeichnisVon(?VerzeichnisName1, ?VerzeichnisName2)
% 'VerzeichnisName1' und 'VerzeichnisName2' sind Argumentpositionen,
% so dass 'VerzeichnisName1' der Name des Verzeichnisses ist, welches ein Unterverzeichnis vom Verzeichnis mit dem Namen 'VerzeichnisName2' ist.
istUnterverzeichnisVon(VerzeichnisName1, VerzeichnisName2) :-
    directory(Verzeichnis1, VerzeichnisName1, _, _, _),
    directory(Verzeichnis2, VerzeichnisName2, _, _, _),
    directory(Verzeichnis1, _, Verzeichnis2, _, _).

istUnterverzeichnisVon(VerzeichnisName1, VerzeichnisName2) :-
    directory(Verzeichnis1, VerzeichnisName1, _, _, _),
    directory(Verzeichnis1, _, Verzeichnis3, _, _),
    directory(Verzeichnis3, VerzeichnisName3, _, _, _),
    istUnterverzeichnisVon(VerzeichnisName3, VerzeichnisName2).

%%%%% Aufgabe 4.2

% istErreichbar(?Wurzelverzeichnis, ?DateiID)
% 'Wurzelverzeichnis' und 'DateiID' sind Argumentpositionen,
% so dass die Datei mit der ID 'DateiID' im Zugriffspfad vom Verzeichnis mit der ID 'Wurzelverzeichnis' liegt.
istErreichbar(Wurzelverzeichnis, DateiID) :-
    directory(Wurzelverzeichnis, WurzelverzeichnisName, _, _, _),
    file(DateiID, ParentID, _, _, _, _),
    directory(ParentID, ParentName, _, _, _),
    zugriffspfad(WurzelverzeichnisName, ParentName).
    
%%%%% Aufgabe 4.3

% alleUnterverzeichnisse(?VerzeichnisName, ?Unterverzeichnisse)
% 'VerzeichnisName' und 'Unterverzeichnisse' sind Argumentpositionen,
% so dass 'Unterverzeichnisse' alle Unterverzeichnisse vom Verzeichnis mit dem Namen 'VerzeichnisName' enthält.
alleUnterverzeichnisse(VerzeichnisName, Unterverzeichnisse) :-
    findall(X,
        istUnterverzeichnisVon(X, VerzeichnisName),
        Unterverzeichnisse).

% Alternativlösung für Ausgabe nacheinadner:

% alleUnterverzeichnisse2(?VerzeichnisName, Unterverzeichnis)
% 'VerzeichnisName' und 'Unterverzeichnis' sind Argumentpositionen,
% so dass 'Unterverzeichnis' ein Unterverzeichnis vom Verzeichnis mit dem Namen 'VerzeichnisName' ist.
alleUnterverzeichnisse2(VerzeichnisName, Unterverzeichnis) :-
    istUnterverzeichnisVon(Unterverzeichnis, VerzeichnisName).

%%%%% Aufgabe 4.4

% gesamtgroesse(?OberverzeichnisID, ?Groesse)
% 'OberverzeichnisID' und 'Groesse' sind Argumentpositionen,
% so dass 'Groesse' die Größe aller Dateien im Verzeichnis mit der ID 'OberverzeichnisID' ist.
% Die richtige Lösung ist das erste Ergebnis, welches ausgegeben wird.
gesamtgroesse(OberverzeichnisID, Groesse) :-
    alleUnterverzeichnisseID(OberverzeichnisID, Unterverzeichnisse),
    add_list([OberverzeichnisID], Unterverzeichnisse, Unterverzeichnisse2),
    teileListe(Unterverzeichnisse2, 0, Gesamtgroesse, []),
    Groesse is Gesamtgroesse.


% Aktuelles Problem: Sobald er eine Lösung gefunden hat, kann man sich alle anderen Lösungen ausgeben lassen, die in irgendeiner Weise mögliche Kombinationen der Files sind, was eine ziemlich lange Liste ergibt.
% Wenn die Liste leer ist.
teileListe(Unterverzeichnisse, Groesse, Gesamtgroesse, _) :-
    Unterverzeichnisse = [],
    Gesamtgroesse = Groesse.
% Wenn noch Dateien in dem aktuellen Verzeichns liegen.
teileListe(Unterverzeichnisse, Groesse, Gesamtgroesse, SchonBearbeiteteFiles) :-
    Unterverzeichnisse = [DirID|_],
    file(FileID, DirID, _, Dateigroesse, _, _),
    \+ istEnthalten(FileID, SchonBearbeiteteFiles),
    add_list([FileID], SchonBearbeiteteFiles, SchonBearbeiteteFiles2),
    Groesse2 is Groesse + Dateigroesse,
    teileListe(Unterverzeichnisse, Groesse2, Gesamtgroesse, SchonBearbeiteteFiles2).
% Wenn keine Datei mehr in dem aktuellen Verzeichnis liegt.
teileListe(Unterverzeichnisse, Groesse, Gesamtgroesse, _) :-
    Unterverzeichnisse = [_|Restliste],
    SchonBearbeiteteFiles2 = [],
    teileListe(Restliste, Groesse, Gesamtgroesse, SchonBearbeiteteFiles2).

add2(Element, List, [Element|List]).
add_list([], List, List).
add_list([Head|Tail], List, List2) :-
    add2(Head, List3, List2),
    add_list(Tail, List, List3).

istUnterverzeichnisVonID(VerzeichnisID1, VerzeichnisID2) :-
    directory(VerzeichnisID1, _, VerzeichnisID2, _, _).

istUnterverzeichnisVonID(VerzeichnisID1, VerzeichnisID2) :-
    directory(VerzeichnisID1, _, VerzeichnisID3, _, _),
    istUnterverzeichnisVonID(VerzeichnisID3, VerzeichnisID2).

alleUnterverzeichnisseID(VerzeichnisID, Unterverzeichnisse) :-
    findall(X,
        istUnterverzeichnisVonID(X, VerzeichnisID),
        Unterverzeichnisse).

istEnthalten(Element, List) :-
    List = [Head|_],
    Element = Head.
istEnthalten(Element, List) :-
    List = [_|Tail],
    istEnthalten(Element, Tail).

%%%%%%%%%%%%%
% AUFGABE 5 %
%%%%%%%%%%%%%

%Gemeinsamkeiten:
%    Alle haben eine Datenbasis und Operationen, die auf ihr ausgeführt werden können. Alle Datenbanken arbeiten nach der selben Logik.
    
%Unterschiede:
%    Die Komplexität der Operationen unterscheidet sich mit der Komplexität der Datenbasis. Demzufolge sind mehr unterschiedliche Methoden möglich, je komplexer die Datenbasis ist.

%Bedingungen:
%    Ein terminierungssicherer Prädikatsaufruf muss in jeder Instanziierungsvariante terminierungssicher sein.

%Beispiel:
%    Nicht terminierungssicher. Es kommt bei jeder Eingabe in eine Endlosrekursion.
    einBeispiel(X, Y).
    einBeispiel(X, Y) :-
        einBeispiel(Y, X).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 28/30
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% Testmethoden, die beim Schreiben der anderen aus Versehen entstanden sind.

% natuerlicheZahl(+Zahl)
% Zahl ist eine Natürliche Zahl
natuerlicheZahl(0).
natuerlicheZahl(X) :-
    X > 0,
    X1 is X - 1,
    natuerlicheZahl(X1).

i2p(X, Y) :-
    X = 0,
    Y = 0.
i2p(X, s(Y)) :-
    X1 is X - 1,
    p2i(X1, Y).
    
addiere(X, Y, G) :-
    X = 0,
    G = Y.
addiere(s(X), Y, G) :-
    addiere(X, s(Y), G).