
:- dynamic(directory/5).
:- dynamic(file/6).

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






%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%

% Aufgabe 1.1

:- dynamic(zugriffspfad/2).

% zugriffspfad(?ID, ?Liste)
% 'ID' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad des Verzeichnsises 'ID' ist.

zugriffspfad(1, Liste) :-
    directory(1, Name, _, _, _),
    Liste = [Name].

zugriffspfad(ID, Liste) :-
    directory(ID, Name, ParentID, _, _),
    zugriffspfad(ParentID, Liste2),
    Liste = [Name|Liste2].

% Aufgabe 1.2

:- dynamic(zugriffspfadDatei/3).

% zugriffspfadDatei(?ID, ?Dateiname, ?Liste)
% 'ID', 'Dateiname' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad der Datei 'Dateiname' im Verzeichnis 'ID' ist.

zugriffspfadDatei(ID, Dateiname, Liste) :-
    zugriffspfad(ID, Liste2),
    Liste = [Dateiname|Liste2].

% Aufgabe 1.3

:- dynamic(zuletztModifiziert/2).

% zuletztModifiziert(?Anzahl, ?Liste)
% 'Anzahl' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad zu 'Anzahl' Dateien ist, die zuletzt modifiziert worden sind.

zuletztModifiziert(Anzahl, Liste) :-
    date(Heute),
    zuletztModifiziertRek(Anzahl, [], Heute, [], Liste).


:- dynamic(zuletztModifiziertRek/5).

% zuletztModifiziert(?Anzahl, ?Liste, ?LetztesDatum, ?BereitsBekannt, ?Ergebnis)
% 'Anzahl', 'Liste', 'LetztesDatum', 'BereitsBekannt' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' der Zugriffspfad zu 'Anzahl' Dateien ist, die zuletzt modifiziert worden sind.
% 'Liste' ist die Zwischenliste aller schon gefundenen Dateien mit 'Liste'.length < 'Anzahl'.
% 'LetztesDatum' ist das letzte überprüfte Datum.
% 'BereitsBekannt' ist eine Liste aller bereits überprüften Dateien.

zuletztModifiziertRek(Anzahl, Liste, _, _, Liste) :-
    length(Liste, Laenge),
    Laenge >= Anzahl.

zuletztModifiziertRek(Anzahl, Liste, LetztesDatum, BereitsBekannt, Ergebnis) :-
    length(Liste, Laenge),
    Laenge < Anzahl,
    holeNeustenFile(BereitsBekannt, Datei, LetztesDatum),
    zuletztModifiziertRek(Anzahl, [Datei|Liste], LetztesDatum, [Datei|BereitsBekannt], Ergebnis).


:- dynamic(holeNeustenFile/3).

% holeNeustenFile(?BereitsBekannt, ?Ergebnis, ?LetztesDatum)
% 'BereitsBekannt', 'Ergebnis' und 'LetztesDatum' sind Argumentpositionen,
% so dass 'Ergebnis' eine Liste aus ID und Name der neusten Datei ab dem Datum 'LetztesDatum' ist.
% 'BereitsBekannt' ist eine Liste aller bereits überprüften Dateien.


holeNeustenFile(BereitsBekannt, [ID, Dateiname], LetztesDatum) :-
    file(_, ID, Dateiname, _, _, LetztesDatum),
    \+ member([ID, Dateiname], BereitsBekannt).

holeNeustenFile(BereitsBekannt, Ergebnis, LetztesDatum) :-
    getDatumVor(LetztesDatum, VorletztesDatum),
    VorletztesDatum @> date(1900, 1, 1),
    holeNeustenFile(BereitsBekannt, Ergebnis, VorletztesDatum).


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
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AUFGABE 1.3 terminiert noch nicht, sondern gibt alle Möglichkeiten aus
    