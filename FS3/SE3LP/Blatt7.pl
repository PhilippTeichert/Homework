
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

%%%%%%%% Aufgabe 1.1

:- dynamic(zugriffspfad/2).

% zugriffspfad(?VerzID, ?Liste)
% 'VerzID' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad des Verzeichnsises 'VerzID' ist.

zugriffspfad(1, Liste) :-
    directory(1, Name, _, _, _),
    Liste = [Name].

zugriffspfad(VerzID, Liste) :-
    directory(VerzID, Name, ParentID, _, _),
    zugriffspfad(ParentID, Liste2),
    Liste = [Name|Liste2].

%%%%%%%% Aufgabe 1.2

:- dynamic(zugriffspfadDatei/3).

% zugriffspfadDatei(?VerzID, ?Dateiname, ?Liste)
% 'VerzID', 'Dateiname' und 'Liste' sind Argumentpositionen,
% so dass 'Liste' der Zugriffspfad der Datei 'Dateiname' im Verzeichnis 'VerzID' ist.

zugriffspfadDatei(VerzID, Dateiname, Liste) :-
    zugriffspfad(VerzID, Liste2),
    Liste = [Dateiname|Liste2].

%%%%%%%% Aufgabe 1.3


:- dynamic (hole_n_elemente/4,
			zuletzt_geaendert/2).
			

% Rekursionsabbruch: Der Zähler ist auf 0 gekommen
hole_n_elemente(0, _, Akku, Akku).
% Wenn mehr Elemente gefragt werden, als vorhanden sind,
% wird die gesamte Liste ausgegeben
hole_n_elemente(Anzahl, Liste, Liste2, Liste3) :-
	length(Liste, Laenge),			% hole die Länge der Liste
	Anzahl > Laenge,				% wenn mehr gefragt wird, als gegeben werden kann
	hole_n_elemente(Laenge, Liste, Liste2, Liste3),	% dann gib die ganze Liste aus
	!.
	
% 'Anzahl', '[Head|Tail]', 'OutputList', 'Akku' sind Argumentpositionen, so dass
% 'OutputList' die 'Anzahl' zuletzt geänderten Elemente aus '[Head|Tail]' beinhält.
% 'Akku' ist ein interner Zwischenspeicher.
hole_n_elemente(Anzahl, [Head|Tail], OutputList, Akku) :-
	Decrement is Anzahl - 1 ,
	Head = [_, Name, FileID, VerzID],
	zugriffspfadDatei(VerzID, Name, Pfad),
	append(Akku, [[FileID, Pfad]], NewAkku),
	hole_n_elemente(Decrement, Tail, OutputList, NewAkku).
	
% 'Anzahl' und 'Liste' sind Argumentpositionen, so dass 'Anzahl' zuletzt geänderter Dateien aus
% 'Liste' zurückgegeben werden.
zuletzt_geaendert(Anzahl, Liste) :-
	findall([Datum, Name, FileID, VerzID], file(FileID, VerzID, Name, _, _, Datum), DateiListe),
	sort(0, @>=,DateiListe, SortierteListe),
	hole_n_elemente(Anzahl, SortierteListe, Liste, []).


%%%% Laufzeit:
%% findall ist linearer Aufwand abhängig von der Anzahl der Dateien (O(n))
%% sort benutzt merge-sort (O(n*log n))
%% hole_n_elemente ist zwischen O(n) und O(Anzahl)
%% Dies ergibt eine Laufzeit in O(n*log n)

%%%% Optimierung:
%% Derzeit wird in der zweiten Abbruchbedingung für hole_n_elemente/4 ein ! verwendet.
%% Dies könnte ggf. noch wegoptimiert werden.
    
    
	
	
	
%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%

%%%%%%%% Aufgabe 2.1

/*
Damit der entstehende Baum balanciert ist, müsste die Eingabeliste
folgendermaßen sortiert sein:

Das erste Element der Eingabeliste müsste in der Mitte der Liste liegen, wäre sie sortiert.
Demzufolge müsste es das Element mit dem Median-Wert sein.
Dies gilt rekursiv für jede beliebige Restliste.
*/


%%%%%%%% Aufgabe 2.2

:- dynamic (split/4,
			list2tree/2).

split([ ],_,[ ],[ ]).
split([E|R],M,[E|VL],HL) :-
	E@=<M, split(R,M,VL,HL).
split([E|R],M,VL,[E|HL]) :-
	E@>M, split(R,M,VL,HL).

list2tree([],end).
list2tree([[E|I]|R],t(v(E,I),VB,HB)) :-
	split(R,E,VL,HL),
	list2tree(VL,VB),
	list2tree(HL,HB).
	
	
%%%%%%%% Aufgabe 2.3

:- dynamic (tree2list/2).

% Aus dem Skript.
% tree2list(+Baum,?Liste)
tree2list(end,[ ]).
tree2list(t(v(E,I),VB,HB),L) :-
	tree2list(VB,VL),
	tree2list(HB,HL),
	append(VL,[[E|I]|HL],L).


:- dynamic (hole_info_aus_liste/3,
			hole_info_aus_baum/3).

% $Liste, $Element und $Info sind Argumentpositionen, so dass
% das $Element aus der Liste $Liste die $Info beinhält.
% hole_info_aus_liste(+Liste,+Element,?Info)
hole_info_aus_liste(Liste, Element, Info) :-
	list2tree(Liste, Baum),
	hole_info_aus_baum(Baum, Element, Info).
	
% $t(...), $Knoten und $Info sind Argumentpositionen, so dass
% der $Knoten aus dem Baum $t(...) die $Info beinhält.
% hole_info_aus_liste(+Baum,+Knoten,?Info)
hole_info_aus_baum(t(v(Knoten,Info),_,_), Knoten, Info).
hole_info_aus_baum(t(v(Knoten2,_),LTB,RTB), Knoten, Info) :-
	Knoten2 \= Knoten,
	hole_info_aus_baum(LTB,Knoten,Info);
	hole_info_aus_baum(RTB,Knoten,Info).
	
	
	
%%%%%%%% Aufgabe 2.4


:- dynamic (balancierter_baum/2).

balancierter_baum([],end).
balancierter_baum(Liste, Baum) :-
	sort(Liste, SortierteListe),
	length(SortierteListe, L),
	Idx is L / 2,
	integer(Idx),
	nth0(Idx, SortierteListe, Elem),
	split(SortierteListe, Elem, LTL, RTL),
	balancierter_baum(LTL, LTB),
	balancierter_baum(RTL, RTB),
	Elem = [E|I],
	Baum = t(v(E,I),LTB, RTB).
	
balancierter_baum(Liste, Baum) :-
	sort(Liste, SortierteListe),
	length(SortierteListe, L),
	Idx is (L - 1) / 2,
	integer(Idx),
	nth0(Idx, SortierteListe, Elem),
	split(SortierteListe, Elem, LTL, RTL),
	balancierter_baum(LTL, LTB),
	balancierter_baum(RTL, RTB),
	Elem = [E|I],
	Baum = t(v(E,I),LTB, RTB).
	

	
	





































































