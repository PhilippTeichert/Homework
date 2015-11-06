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






%%%%%%%%%%%%%%%%%%%%%%%%% datenverzeichnis.pl ^^^^^^
%%%%%%%%%%%%%%%%%%%%%%%%% unser Stuff vvvvvvv



%%%%% Aufgabe 2.1


:- dynamic(nameUndIdFile/2).


% nameUndIdFile(?,?)
% nameUndIdFile(name, id)

nameUndIdFile(X, Y) :- file(Y, _, X, _, _, _), print("Name: "), print(X), print(" ID: "), print(Y).


%%%%% Aufgabe 2.2


:- dynamic(nameUndIdDirectory/2).


% nameUndIdDirectory(?,?)
% nameUndIdDirectory(name, id)

nameUndIdDirectory(X, Y) :- directory(Y, X, _, _, _), print("Name: "), print(X), print(" ID: "), print(Y).


%%%%% Aufgabe 2.3

:- dynamic(dateiToVerzeichnis/1).

% dateiToVerzeichnis(+)
% dateiToVerzeichnis(Dateiname)

dateiToVerzeichnis(X) :- file(_, Y, X, _, _, _), directory(Y, Z, _, _, _), print("Schlussel: "), print(Y), print(" Verzeichnisname: "), print(Z).


%%%%% Aufgabe 2.4

:- dynamic(parentFolder/1).

% parentFolder(+)
% parentFolder(Verzeichnisname)

parentFolder(X) :- file(_, Y, X, _, _, _), directory(Y, Z, _, _, _), print("Schlussel: "), print(Y), print(" Verzeichnisname: "), print(Z).