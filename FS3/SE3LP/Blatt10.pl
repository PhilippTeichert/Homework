%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%

%%%% Aufgabe 1.1

% Folgende Informationen sind in der "month_2014_01.json" enthalten:
% "key": der Key um auf die Daten danach gezielt zugreifen zu können
% "color": die Farbe in der die Daten auf der website dargestellt werden
% "date": der Datumswert
% "values": Tupel der Form: [Datumswert, Stromwert]
% Stromwert ist dabei die Information zur elektrischen Energie auf dem Wege "key" in Gigawatt (GW)
% Der Datumswert ist wie folgt verschlüsselt: 3.600.000 entspricht einer Stunde
% => die Zeitangabe ist in Millisekunden
% 1.388.530.800.000 entspricht dem 01.01.2014 00:00 Uhr
% => Der startzeitpunkt sind also ~44 Jahre


%%%% Aufgabe 1.2

% internally, we'll use the following format:
%
% energy_time_series(Type, StartTime, IntegrationPeriod, ListOfValues).
%
% Type is the kind of information stored in this time series. We'll use the 
%   German description of the data as type information.
% StartTime is a UNIX timestamp (i.e., seconds since 1970-01-01T00:00)
%   (this simplifies conversion using stamp_date_time/3 (stamp, date, timezone)).
% IntegrationPeriod is the number of seconds between the values in ListOfValues.
% Each value is the energy counted in this period (in MWh/integration period)

:- dynamic energy_time_series/4.

init(Summe) :- % Testmethode für besonders faule
    liesDateiEin(),
    veraendereZeitraum(),
    gesamtverbrauch(Summe).

:- use_module(library(http/json)).

liesDateiEin() :- % Testmethode für faule
    liesDateiEin('month_2014_01.json').

liesDateiEin(Datei) :- % liest eine .json Datei ein nach dem Format der Seite "energy-charts.de" in die Datenbank ein
    open(Datei, read, Stream),
    json_read(Stream, Inhalt),
    Inhalt = [json([key= [json([_, de=Type1|_])], _, _, _, (values=Werte1)]), json([key=[json([_,de=Type2|_])], _, _, (values=Werte2)]), json([key=[json([_,de=Type3|_])], _, (values=Werte3)]), json([key=[json([_,de=Type4|_])], _, (values=Werte4)]), json([key=[json([_,de=Type5|_])], _, (values=Werte5)]), json([key=[json([_,de=Type6|_])], _, (values=Werte6)]), json([key=[json([_,de=Type7|_])], _, (values=Werte7)]), json([key=[json([_,de=Type8|_])], _, (values=Werte8)]), json([key=[json([_,de=Type9|_])], _, (values=Werte9)]), json([key=[json([_,de=Type10|_])], _, (values=Werte10)]), json([key=[json([_,de=Type11|_])], _, (values=Werte11)]), json([key=[json([_,de=Type12|_])], _, (values=Werte12)]), json([key=[json([_,de=Type13|_])], _, (values=Werte13)]), json([key=[json([_,de=Type14|_])], _, (values=Werte14)])],
    IntegrationPeriod = 3600000,
    sammleWerte(Werte1, ListOfValues1, []),
    Werte1 = [[StartTime1, _]|_],
    sammleWerte(Werte2, ListOfValues2, []),
    Werte2 = [[StartTime2, _]|_],
    sammleWerte(Werte3, ListOfValues3, []),
    Werte3 = [[StartTime3, _]|_],
    sammleWerte(Werte4, ListOfValues4, []),
    Werte4 = [[StartTime4, _]|_],
    sammleWerte(Werte5, ListOfValues5, []),
    Werte5 = [[StartTime5, _]|_],
    sammleWerte(Werte6, ListOfValues6, []),
    Werte6 = [[StartTime6, _]|_],
    sammleWerte(Werte7, ListOfValues7, []),
    Werte7 = [[StartTime7, _]|_],
    sammleWerte(Werte8, ListOfValues8, []),
    Werte8 = [[StartTime8, _]|_],
    sammleWerte(Werte9, ListOfValues9, []),
    Werte9 = [[StartTime9, _]|_],
    sammleWerte(Werte10, ListOfValues10, []),
    Werte10 = [[StartTime10, _]|_],
    sammleWerte(Werte11, ListOfValues11, []),
    Werte11 = [[StartTime11, _]|_],
    sammleWerte(Werte12, ListOfValues12, []),
    Werte12 = [[StartTime12, _]|_],
    sammleWerte(Werte13, ListOfValues13, []),
    Werte13 = [[StartTime13, _]|_],
    sammleWerte(Werte14, ListOfValues14, []),
    Werte14 = [[StartTime14, _]|_],
    asserta(energy_time_series(Type1, StartTime1, IntegrationPeriod, ListOfValues1)),
    asserta(energy_time_series(Type2, StartTime2, IntegrationPeriod, ListOfValues2)),
    asserta(energy_time_series(Type3, StartTime3, IntegrationPeriod, ListOfValues3)),
    asserta(energy_time_series(Type4, StartTime4, IntegrationPeriod, ListOfValues4)),
    asserta(energy_time_series(Type5, StartTime5, IntegrationPeriod, ListOfValues5)),
    asserta(energy_time_series(Type6, StartTime6, IntegrationPeriod, ListOfValues6)),
    asserta(energy_time_series(Type7, StartTime7, IntegrationPeriod, ListOfValues7)),
    asserta(energy_time_series(Type8, StartTime8, IntegrationPeriod, ListOfValues8)),
    asserta(energy_time_series(Type9, StartTime9, IntegrationPeriod, ListOfValues9)),
    asserta(energy_time_series(Type10, StartTime10, IntegrationPeriod, ListOfValues10)),
    asserta(energy_time_series(Type11, StartTime11, IntegrationPeriod, ListOfValues11)),
    asserta(energy_time_series(Type12, StartTime12, IntegrationPeriod, ListOfValues12)),
    asserta(energy_time_series(Type13, StartTime13, IntegrationPeriod, ListOfValues13)),
    asserta(energy_time_series(Type14, StartTime14, IntegrationPeriod, ListOfValues14)),
    !. %dies ist nur dazu da, dass er das "false" nicht mit ausgibt.

% wandelt eine .json Datei nach dem Format der Seite "energy-charts.de" in das Format gegeben in der "2014_01_facts.pl" um
sammleWerte([], Output, Iterator) :-
    reverse(Output, Iterator).

sammleWerte(Input, Output, Iterator) :-
    Input = [[_, Wert]|RestInput],
    Iterator2 = [Wert|Iterator],
    sammleWerte(RestInput, Output, Iterator2).

%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%

%%%% Aufgabe 2.1.a

veraendereZeitraum() :- % Testmethode für faule
    findall(Name, (energy_time_series(Name, _, _, _), veraendereZeitraum(energy_time_series(Name, _, _, _), 86400000)), _).

veraendereZeitraum(energy_time_series(Type, StartTime, IntegrationPeriod, ListOfValues), NeuesZeitintervall) :- % verändert das Intervall eines Eintrags auf ein gegebenes Intervall
    retract(energy_time_series(Type, StartTime, IntegrationPeriod, ListOfValues)),
    length(ListOfValues, Laenge),
    Zeitintervall is Laenge * IntegrationPeriod,
    NeueLaenge is Zeitintervall / NeuesZeitintervall,
    AnzahlZusammenzufassenderWerte is Laenge / NeueLaenge,
    bildeNeueWerte(ListOfValues, AnzahlZusammenzufassenderWerte, NeueWerte, []),
    asserta(energy_time_series(Type, StartTime, NeuesZeitintervall, NeueWerte)).


bildeNeueWerte([], _, NeueWerte, Iterator) :- % bildet eine neue Liste an Werten indem die alten Werte auf die AnzahlZusammenzufassenderWerte (in der aufrufenen Methode: auf das neue Zeitintervall) zusammengefasst werden
    reverse(NeueWerte, Iterator).

bildeNeueWerte(WerteListe, AnzahlZusammenzufassenderWerte, NeueWerte, Iterator) :-
    length(WerteListe, Laenge),
    AnzahlZusammenzufassenderWerte =< Laenge,
    nimmWerteRunter(WerteListe, AnzahlZusammenzufassenderWerte, Restliste, EntfernteWerte),
    listenSumme(EntfernteWerte, Summe),
    length(EntfernteWerte, NeueLaenge),
    NeuerWert is Summe / NeueLaenge,
    Iterator2 = [NeuerWert| Iterator],
    bildeNeueWerte(Restliste, AnzahlZusammenzufassenderWerte, NeueWerte, Iterator2),
    !. % zum Verhindern der "false" Ausgabe am Ende
    

bildeNeueWerte(WerteListe, AnzahlZusammenzufassenderWerte, NeueWerte, Iterator) :-
    length(WerteListe, Laenge),
    AnzahlZusammenzufassenderWerte > Laenge,
    EntfernteWerte = WerteListe, % ein Methodenaufruf wird hier gespaart
    listenSumme(EntfernteWerte, Summe),
    length(EntfernteWerte, NeueLaenge),
    NeuerWert is Summe / NeueLaenge,
    NeueWerte = [NeuerWert| Iterator]. % und hier auch einer


% Für die Argumentpositionen gelten folgende Definitonen:
% WerteListe: Eine AusgangsListe mit Elementen
% AnzahlZusammenzufassenderWerte: Die Anzahl der Elemente, die abgeschnitten werden sollen
% Restliste: die nach dem Abschneiden über bleibende Liste
% EntfernteWerte: die abgeschnittenen Werte
nimmWerteRunter(WerteListe, 0, WerteListe, []).

nimmWerteRunter([H|T], AnzahlZusammenzufassenderWerte, Restliste, EntfernteWerte) :-
    AnzahlZusammenzufassenderWerte > 0,
    AnzahlZusammenzufassenderWerte2 is AnzahlZusammenzufassenderWerte - 1,
    nimmWerteRunter(T, AnzahlZusammenzufassenderWerte2, Restliste, EntfernteWerte2),
    EntfernteWerte = [H|EntfernteWerte2],
    !. % um die Ausgabe des "false" am Ende zu vermeiden
    




listenSumme([Element], Summe) :- % berechnet die Summe der Elemente einer Liste
    Summe is Element,
    !. % um das "false" am Ende abzuschneiden

listenSumme([Element1, Element2| Restliste], Summe) :-
    listenSumme([Element1+Element2| Restliste], Summe).

%%%% Aufgabe 2.1.b
% Type = 'Steinkohle',
% StartTime = 1388530800000,
% IntegrationPeriod = 86400000,
% ListOfValues = [3.751291666666667, 8.170291666666666, 6.847208333333331, 5.804333333333333, 4.685791666666667, 6.425708333333334, 10.517791666666666, 13.693333333333333, 11.53475, 12.286333333333333, 8.040708333333333, 6.132875000000001, 17.287625, 19.407666666666668, 19.268500000000003, 17.549083333333332, 17.263208333333335, 10.280125, 6.336416666666666, 17.215791666666664, 19.279166666666665, 19.60766666666667, 19.55275, 19.409791666666667, 15.19279166666667, 8.859625, 16.698458333333335, 18.89225, 17.540291666666665, 18.08641666666667, 17.902291666666663] 

% Type = 'Wind',
% StartTime = 1388530800000,
% IntegrationPeriod = 86400000,
% ListOfValues = [8.950541666666666, 12.917708333333335, 13.750375, 11.388083333333334, 7.123208333333333, 15.288124999999996, 15.233791666666669, 12.213999999999999, 17.333458333333333, 16.204125, 12.485708333333333, 10.597458333333334, 5.287958333333333, 1.4009166666666666, 3.369208333333333, 7.184916666666669, 5.170999999999999, 6.394500000000001, 12.833041666666665, 3.5096249999999998, 0.6701666666666667, 2.382916666666667, 5.122208333333334, 4.5421249999999995, 5.767333333333333, 7.656333333333332, 7.826499999999999, 6.476916666666668, 8.897833333333333, 7.053833333333334, 4.536541666666666] 

%%%% Aufgabe 2.2.a

% Die Implementierung fand wie folgt statt:
% Erst wurde aus der Datenbank jede Werteliste ausgelesen.
% Diese wurden dann alle zusammen in eine Liste gesteckt.
% Diese Liste wurde nun in eine Matrix umgeformt, indem immer das erste Element on jeder Lsite abgeknüpft und der Matrix hinzugefügt wurde, bis die Listen leer waren.
% PS: Es wäre sinnvoll diese Methode nur mit gleich langen Listen (Listen, bei denen die Zeitintervalle gleich lang sind) auszuführen.

namenInWerte([], []). % baut die Zeilennamen in die Werte um
namenInWerte([ErsterName|Namen], [ErsteWerte|Werte]) :-
    energy_time_series(ErsterName, _, _, ErsteWerte),
    namenInWerte(Namen, Werte).

listeToMatrix(Matrix) :- % Testmethode für faule
    findall(Name, (energy_time_series(Name, _, _, _)), Namen),
    namenInWerte(Namen, Werte),
    listeToMatrix(Werte, Matrix).

listeToMatrix([[]| _], []).

listeToMatrix(Liste, Matrix) :-
    length(Liste, AnzahlListen),
    getErsteElemente(Liste, Restliste, ErsteElemente, [], [], AnzahlListen),
    listeToMatrix(Restliste, Restmatrix),
    !, % um das "false" am Ende abzuschneiden
    Matrix = [ErsteElemente| Restmatrix].
    

% holt sich aus jeder Liste das erste Element
% die Parameter sidn wie folgt Definiert:
% Liste: die Eingabeliste, die aus Listen besteht
% Restliste: die Eingabeliste ohne ihr erstes Element in jeder Liste
% ErsteElemente: die ersten Elmente aus jeder Liste als Liste
% Iterator: der Iterator für "Restliste"
% Iterator2: der Iterator für "ErsteElemente"
% AnzahlListen: die Anzahl der noch vorhandenen Listen in "Liste"
getErsteElemente(_, Restliste, ErsteElemente, Iterator, Iterator2, 0) :-
    reverse(Restliste, Iterator),
    reverse(ErsteElemente, Iterator2).


getErsteElemente([[ErstesElement| T2]| T], Restliste, ErsteElemente, Iterator, Iterator2, AnzahlListen) :-
    AnzahlListen > 0,
    AnzahlListen2 is AnzahlListen - 1,
    Iterator3 = [T2| Iterator],
    Iterator4 = [ErstesElement| Iterator2],
    getErsteElemente(T, Restliste, ErsteElemente, Iterator3, Iterator4, AnzahlListen2).


% Test:
% [1] 22 ?- listeToMatrix([[1, 2, 3, 6], [2, 3, 4, 6], [3, 4, 5, 6]], X).
% X = [[1, 2, 3], [2, 3, 4], [3, 4, 5], [6, 6, 6]].
% [1] 23 ?- transpose([[1, 2, 3, 6], [2, 3, 4, 6], [3, 4, 5, 6]], X).
% X = [[1, 2, 3], [2, 3, 4], [3, 4, 5], [6, 6, 6]]


%%%% Aufgabe 2.2.b

gesamtverbrauch(Summe) :- % Testmethode für faule
    listeToMatrix(Matrix),
    gesamtverbrauch(Matrix, Summe).

gesamtverbrauch([], []). % summiert die einzelnen Spalten einer Matrix auf und gibt das Ergebnis als Liste aus

gesamtverbrauch([ErstesElement| Restliste], Summe) :-
    length(ErstesElement, Divisor),
    listenSumme(ErstesElement, Dividend),
    ErsteSumme is Dividend / Divisor,
    gesamtverbrauch(Restliste, Restsumme),
    Summe = [ErsteSumme| Restsumme].

%%%% Aufgabe 3.1.a

korrelationskoeffizient(Koeffizient) :- % Testmethode für faule
    energy_time_series(Name1, Startzeit, Intervall, Werte1),
    energy_time_series(Name2, Startzeit, Intervall, Werte2),
    Name1 \= Name2, 
    !,
    korrelationskoeffizient(Werte1, Werte2, Koeffizient).

korrelationskoeffizient([], [], 0). % berechnet edn Korrelationskoeffizient

korrelationskoeffizient(Werte1, Werte2, Koeffizient) :-
    length(Werte1, Laenge),
    malListenSumme(Werte1, Werte2, Summe, 0),
    listenSumme(Werte1, Summe2),
    listenSumme(Werte2, Summe3),
    listenSummeQuadrat(Werte1, Summe4),
    listenSummeQuadrat(Werte2, Summe5),
    % sqrt(((Laenge * Summe4) - (Summe2 * Summe2)), Wurzel),
    % sqrt(((Laenge * Summe5) - (Summe3 * Summe3)), Wurzel2),
    % Koeffizient is (((Laenge * Summe) - (Summe2 * Summe3)) / (Wurzel * Wurzel2)).
    % trace,
    Zwischenergebnis1 is Laenge * Summe,
    Zwischenergebnis2 is Summe2 * Summe3,
    Zwischenergebnis3 is Zwischenergebnis1 - Zwischenergebnis2,
    Zwischenergebnis4 is Laenge * Summe4,
    Zwischenergebnis5 is Summe2 * Summe2,
    Zwischenergebnsi6 is Zwischenergebnis4 - Zwischenergebnis5,
    sqrt(Zwischenergebnsi6, Zwischenergebnis7),
    Zwischenergebnis8 is Laenge * Summe5,
    Zwischenergebnis9 is Summe3 * Summe3,
    Zwischenergebnis10 is Zwischenergebnis8 - Zwischenergebnis9,
    sqrt(Zwischenergebnis10, Zwischenergebnis11),
    Zwischenergebnis12 is Zwischenergebnis7 * Zwischenergebnis11,
    Koeffizient is Zwischenergebnis3 / Zwischenergebnis12.



malListenSumme([], [], Summe, Summe) :- % berechnet die Summe der Elemente einer Liste
    !. % um das "false" am Ende abzuschneiden

malListenSumme([Element1| Liste1], [Element2| Liste2], Summe, Iterator) :-
    Iterator2 is (Element1 * Element2) + Iterator,
    malListenSumme(Liste1, Liste2, Summe, Iterator2).

listenSummeQuadrat([], 0) :- % berechnet die Summe der Elemente einer Liste
    !. % um das "false" am Ende abzuschneiden

listenSummeQuadrat([Element| Restliste], Summe) :-
    listenSummeQuadrat(Restliste, Summe2),
    Summe is (Element * Element) + Summe2.


%%%% Aufgabe 2.3.b

% eine Methode, um den höchsten Korellationskoeffizienten zu bestimmen
gib(X) :-
    findall([Art, Werte], energy_time_series(Art, _, _, Werte), Liste),
    gesamtverbrauch(Summe),
    %trace,
    korellKoeffAufList(Liste, X, Summe).
    %berechneMaximum(Liste, [X, _], Summe).

% Sie konnte aus folgenden gGründen leider nicht getestet werden:
% Biomasse erhält ein negatives Ergebnis unter der Wurzel:
% [13] 94 ?- energy_time_series('Biomasse', _, _, Werte), korrelationskoeffizient(Werte, Werte, X).   Call: (173) _G35494 is 31*1008.9577749999991 ? creep
%    Exit: (173) 31277.69102499997 is 31*1008.9577749999991 ? creep
%    Call: (173) _G35500 is 176.85500000000008*176.85500000000008 ? creep
%    Exit: (173) 31277.691025000026 is 176.85500000000008*176.85500000000008 ? creep
%    Call: (173) _G35506 is 31277.69102499997-31277.691025000026 ? creep
%    Exit: (173) -5.4569682106375694e-11 is 31277.69102499997-31277.691025000026 ? creep
%    Call: (173) _G35512 is 31*1008.9577749999991 ? creep
%    Exit: (173) 31277.69102499997 is 31*1008.9577749999991 ? creep
%    Call: (173) _G35518 is 176.85500000000008*176.85500000000008 ? creep
%    Exit: (173) 31277.691025000026 is 176.85500000000008*176.85500000000008 ? creep
%    Call: (173) _G35524 is 31277.69102499997-31277.691025000026 ? creep
%    Exit: (173) -5.4569682106375694e-11 is 31277.69102499997-31277.691025000026 ? creep
%    Call: (173) quintus:sqrt(-5.4569682106375694e-11, _G35528) ? creep
%    Call: (174) _G35529 is sqrt(-5.4569682106375694e-11) ? creep
% ERROR: sqrt/1: Arithmetic: evaluation error: `undefined'
%    Exception: (174) _G35552 is sqrt(-5.4569682106375694e-11) ? creep
%    Exception: (173) quintus:sqrt(-5.4569682106375694e-11, _G35553) ? creep

korellKoeffAufList([[Typ, Werte]], [[Typ, Koeffizient]], Summe) :-
    korrelationskoeffizient(Werte, Summe, Koeffizient).

korellKoeffAufList([[Typ, Werte]| Restliste], [[Typ, Koeffizient]| Restliste2], Summe) :-
    korrelationskoeffizient(Werte, Summe, Koeffizient),
    korellKoeffAufList(Restliste, Restliste2, Summe).

berechneMaximum([[Typ, Werte]], [Typ, Koeffizient], Summe) :- % eine Methode, um den maximalen Korrelationskoeffizienten in einer Liste zu finden, wie sie bei folgender Anfrage zustande kommt: "?- findall([Art, Werte], energy_time_series(Art, _, _, Werte), Liste)."
    korrelationskoeffizient(Werte, Summe, Koeffizient).

berechneMaximum([[Typ, Werte]| Restliste], [Typ, Koeffizient], Summe) :-
    berechneMaximum(Restliste, [_| Maximum], Summe),
    korrelationskoeffizient(Werte, Summe, Koeffizient),
    Koeffizient > Maximum.

berechneMaximum([Werte| Restliste], [Typ, Maximum], Summe) :-
    berechneMaximum(Restliste, [Typ, Maximum], Summe),
    korrelationskoeffizient(Werte, Summe, Koeffizient),
    Koeffizient =< Maximum.


findeMaximum([Element], Element). % eine Methode, um das Meximum in einer Liste zu finden

findeMaximum([Element| Restliste], Element) :-
    findeMaximum(Restliste, Maximum),
    Element > Maximum.

findeMaximum([Element| Restliste], Maximum) :-
    findeMaximum(Restliste, Maximum),
    Element =< Maximum.


%%%% Aufgabe 2.4.a

lastprofil(Zeitintervall, Summe) :-
    liesDateiEin(),
    findall(Name, (energy_time_series(Name, _, _, _), veraendereZeitraum(energy_time_series(Name, _, _, _), Zeitintervall)), Namen),
    namenInWerte(Namen, Werte),
    listeToMatrix(Werte, Matrix),
    gesamtverbrauch(Matrix, Summe).

%%%% Aufgabe 2.4.b

% Es gibt einen Fehler beim Einlesen weil eine Variable in der Datenbank steht.
