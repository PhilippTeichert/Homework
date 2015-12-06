:- dynamic(zuletztModifiziert/2).

% zuletztModifiziert(?Anzahl, ?Liste)
% 'Anzahl' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad zu 'Anzahl' Dateien ist, die zuletzt modifiziert worden sind.

zuletztModifiziert(Anzahl, Liste) :-
    date(Heute),
    zuletztModifiziertRek(Anzahl, [], Heute, [], Liste).


:- dynamic(zuletztModifiziertRek/5).

% zuletztModifiziert(?Anzahl, ?Liste, ?LetztesDatum, ?Ergebnis)
% 'Anzahl', 'Liste', 'LetztesDatum', 'BereitsBekannt' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' der Zugriffspfad zu 'Anzahl' Dateien ist, die zuletzt modifiziert worden sind.
% 'Liste' ist die Zwischenliste aller schon gefundenen Dateien mit 'Liste'.length < 'Anzahl'.
% 'LetztesDatum' ist das letzte überprüfte Datum.

zuletztModifiziertRek(Anzahl, Liste, _, Liste) :-
    length(Liste, Laenge),
	findall([Date, Name, ID, DirID], file(ID,DirID,Name,_,_,Date), FileList),
    (Laenge >= Anzahl;
	Liste = FileList).

zuletztModifiziertRek(Anzahl, Liste, LetztesDatum, Ergebnis) :-
    length(Liste, Laenge),
    Laenge < Anzahl,
    holeNeuestenFile(Liste, Datei, LetztesDatum),
    zuletztModifiziertRek(Anzahl, [Datei|Liste], LetztesDatum, Ergebnis).


:- dynamic(holeNeuestenFile/3).

% holeNeuestenFile(?BereitsBekannt, ?Ergebnis, ?LetztesDatum)
% 'BereitsBekannt', 'Ergebnis' und 'LetztesDatum' sind Argumentpositionen,
% so dass 'Ergebnis' eine Liste aus ID und Name der neusten Datei ab dem Datum 'LetztesDatum' ist.
% 'BereitsBekannt' ist eine Liste aller bereits überprüften Dateien.


holeNeuestenFile(BereitsBekannt, [ID, Dateiname], LetztesDatum) :-
    file(_, ID, Dateiname, _, _, LetztesDatum),
    \+ member([ID, Dateiname], BereitsBekannt).

holeNeuestenFile(BereitsBekannt, Ergebnis, LetztesDatum) :-
    getDatumVor(LetztesDatum, VorletztesDatum),
    VorletztesDatum @> date(1900, 1, 1),
    holeNeuestenFile(BereitsBekannt, Ergebnis, VorletztesDatum).


:- dynamic(getDatumVor/2).

% getDatumVor(?Heute, ?Gestern)
% 'Heute' und 'Gestern' sind Argumentpositionen,
% so dass 'Gestern' der Tag vor 'Heute' ist.


getDatumVor(date(Y, M, D), Gestern) :- % Tageswechsel
    D > 1,
    D2 is D - 1,
    Gestern = date(Y, M, D2).

getDatumVor(date(Y, M, D), Gestern) :- % 31-Tages-Monat
    D = 1,
    M > 1,
    M2 is M - 1,
    (M2 = 1 ; M2 = 3; M2 = 5; M2 = 7; M2 =  8; M2 = 10; M2 = 12),
    Gestern = date(Y, M2, 31).

getDatumVor(date(Y, M, D), Gestern) :- % 30-Tages-Monat
    D = 1,
    M > 1,
    M2 is M - 1,
    (M2 = 4; M2 = 6; M2 = 9; M2 = 11),
    Gestern = date(Y, M2, 30).

getDatumVor(date(Y, M, D), Gestern) :- % kein Schaltjahr
    D = 1,
    M > 1,
    M2 is M - 1,
    M2 = 2,
    Y2 is Y / 4,
    Y3 is Y / 100,
    Y4 is Y / 400,
    ((integer(Y2),
        integer(Y3),
        \+ integer(Y4));
    (\+ integer(Y2))),
    Gestern = date(Y, M2, 28).

getDatumVor(date(Y, M, D), Gestern) :- % Schaltjahr
    D = 1,
    M > 1,
    M2 is M - 1,
    M2 = 2,
    Y2 is Y / 4,
    Y3 is Y / 100,
    Y4 is Y / 400,
    ((integer(Y2),
        \+ integer(Y3));
    (integer(Y2),
        integer(Y3),
        integer(Y4))),
    Gestern = date(Y, M2, 29).

getDatumVor(date(Y, M, D), Gestern) :- % Jahreswechsel
    D = 1,
    M = 1,
    Y2 is Y - 1,
    Gestern = date(Y2, 12, 31).