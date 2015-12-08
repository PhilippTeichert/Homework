
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
% eine Methode, um aus einer Liste aus Listen der Form [[Knotenname1 | Informationen zum Knoten1], .... ,[KnotennameN | Informationen zum KnotenN]]
% einen Rot-Schwarz-Baum (https://de.wikipedia.org/wiki/Rot-Schwarz-Baum) zu machen.
% Liste ist dabei die Liste aus den Listen
% und Baum der Rot-Schwarz-Baum

balancierter_baum([],end).
balancierter_baum(Liste, Baum) :-
	sort(Liste, SortierteListe),
	length(SortierteListe, L),
	Idx is L / 2,
	integer(Idx),
	nth0(Idx, SortierteListe, Elem),
	split2(SortierteListe, Elem, LTL, [Elem|RTL]),
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
	split2(SortierteListe, Elem, LTL, [Elem|RTL]),
	balancierter_baum(LTL, LTB),
	balancierter_baum(RTL, RTB),
	Elem = [E|I],
	Baum = t(v(E,I),LTB, RTB).

:- dynamic (split2/4).

split2([ ],_,[ ],[ ]).
split2([E|R],M,[E|VL],HL) :-
	E@<M, split2(R,M,VL,HL).
split2([E|R],M,VL,[E|HL]) :-
	E@>=M,
    split2(R,M,VL,HL).














%%%%%%%%%%%%%
% AUFGABE 4 %
%%%%%%%%%%%%%

%%%%%%%% Aufgabe 4.1

:- dynamic(hamming_distanz/3).

% hamming_distanz(+Liste1, +Liste2, ?Distanz)
% 'Liste1', 'Liste2' und 'Distanz' sind Argumentpositionen, so dass
% 'Distanz' die Hamming-Distanz von der Liste 'Liste1' und der Liste 'Liste2' ist.
% Die Listen müssen gleich lang sein.

hamming_distanz([], [], 0).

hamming_distanz([Element|Liste1], [Element|Liste2], Distanz) :-
    hamming_distanz(Liste1, Liste2, Distanz).
    
hamming_distanz([Element1|Liste1], [Element2|Liste2], Distanz) :-
    Element1 \= Element2,
    hamming_distanz(Liste1, Liste2, Distanz2),
    Distanz is Distanz2 + 1.


	
%%%%%%%% Aufgabe 4.2

:- dynamic(hamming_distanz2/3).

% hamming_distanz2(+Liste1, +Liste2, ?Distanz)
% 'Liste1', 'Liste2' und 'Distanz' sind Argumentpositionen, so dass
% 'Distanz' die Hamming-Distanz von der Liste 'Liste1' und der Liste 'Liste2' ist.
% Die Listen dürfen verschieden lang sein.

hamming_distanz2([], Liste2, Distanz) :-
    length(Liste2, Distanz).

hamming_distanz2(Liste1, [], Distanz) :-
    length(Liste1, Distanz).

hamming_distanz2([Element|Liste1], [Element|Liste2], Distanz) :-
    hamming_distanz2(Liste1, Liste2, Distanz).
    
hamming_distanz2([Element1|Liste1], [Element2|Liste2], Distanz) :-
    Element1 \= Element2,
    hamming_distanz2(Liste1, Liste2, Distanz2),
    Distanz is Distanz2 + 1.

	
	
%%%%%%%% Aufgabe 4.3

:- dynamic(hamming_distanz3/4).

% hamming_distanz3(+Liste1, +Liste2, ?Distanz, ?Zuordnung)
% 'Liste1', 'Liste2', 'Distanz' und 'Zuordnung' sind Argumentpositionen, so dass
% 'Distanz' die Hamming-Distanz von der Liste 'Liste1' und der Liste 'Liste2' ist
% und 'Zuordnung' eine Liste aus Listen von je 2 Elementen, wo bei die Zuordnung wie folgt erfolgt:
% List1 = [1, 2, 3], List2 = [a, b, c, d], Zuordnung = [[1, a], [2, b], [3, c], [*, d]].
% Die Listen dürfen verschieden lang sein.

hamming_distanz3([], [], 0, []).

hamming_distanz3([], [Element|Liste2], Distanz, [[*, Element]|Restliste]) :-
    hamming_distanz3([], Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.

hamming_distanz3([Element|Liste1], [], Distanz, [[Element, *]|Restliste]) :-
    hamming_distanz3(Liste1, [], Distanz2, Restliste),
    Distanz is Distanz2 + 1.

hamming_distanz3([Element|Liste1], [Element|Liste2], Distanz, [[Element, Element]|Restliste]) :-
    hamming_distanz3(Liste1, Liste2, Distanz, Restliste).
    
hamming_distanz3([Element1|Liste1], [Element2|Liste2], Distanz, [[Element1, Element2]|Restliste]) :-
    Element1 \= Element2,
    hamming_distanz3(Liste1, Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.


	
%%%%%%%% Aufgabe 4.4

:- dynamic(alignment/4).

% alignment(+Liste1, +Liste2, ?Distanz, ?Zuordnung)
% 'Liste1', 'Liste2', 'Distanz' und 'Zuordnung' sind Argumentpositionen, so dass
% 'Distanz' die Hamming-Distanz von der Liste 'Liste1' und der Liste 'Liste2' ist
% und 'Zuordnung' eine Liste aus Listen von je 2 Elementen, wo bei die Zuordnung wie folgt erfolgt:
% List1 = [1, 2, 3], List2 = [a, b, c, d], Zuordnung = [[1, a], [2, b], [3, c], [*, d]].
% Die Listen dürfen verschieden lang sein.
% Je an beliebigen Stellen in beiden Listen kann ein "*"-Element in der Berechnung hinzu gefügt werden.


alignment([], [], 0, []).
/*
%alignment([], [Element|Liste2], Distanz, [[*, Element]|Restliste]) :-
    alignment([], Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.

alignment([Element|Liste1], [], Distanz, [[Element, *]|Restliste]) :-
    alignment(Liste1, [], Distanz2, Restliste),
    Distanz is Distanz2 + 1.
*/
alignment([Element|Liste1], [Element|Liste2], Distanz, [[Element, Element]|Restliste]) :-
    alignment(Liste1, Liste2, Distanz, Restliste).
    
alignment([Element1|Liste1], [Element2|Liste2], Distanz, [[Element1, Element2]|Restliste]) :-
    Element1 \= Element2,
    alignment(Liste1, Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.

alignment(Liste1, [Element|Liste2], Distanz, [[*, Element]|Restliste]) :-
    alignment(Liste1, Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.

alignment([Element|Liste1], Liste2, Distanz, [[Element, *]|Restliste]) :-
    alignment(Liste1, Liste2, Distanz2, Restliste),
    Distanz is Distanz2 + 1.


/*
Die Menge der generierten Zuordnungen ist endlich, da beide Listen endlich sind und so nur Ausgabemenge.length <= Liste1.length + Liste2.length sein kann. 
Das Prädikat berechnet keine Doppelergebnisse mehr.
Beispiel:
?- alignment([1],[1, 2],Distanz, Endliste).
Distanz = 1,
Endliste = [[1, 1], [*, 2]] ;
Distanz = 2,
Endliste = [[*, 1], [1, 2]] ;
Distanz = 3,
Endliste = [[*, 1], [*, 2], [1, *]] ;
Distanz = 3,
Endliste = [[*, 1], [1, *], [*, 2]] ;
Distanz = 3,
Endliste = [[1, *], [*, 1], [*, 2]] ;
false.

*/



%%%%%%%% Aufgabe 4.5

% levenstein(+Liste1,+Liste2,?Levenshtein_Distanz)
levenstein(L1,L2,LDistanz) :-
  findall(Distanz, alignment(L1, L2, Distanz,_),Distanzen),
  min_list(Distanzen,LDistanz).

/*
Sie ist sehr ineffizient, da sie erst alle möglichen Kombinationen durchgehen muss, um alle möglichen Distanzen zu kennen und so eine Aussage über die minimale Distanz treffen zu können.
Sie läuft daher in O(List1.length + List2.length).


Ein Verbesserungsvorschlag wäre, dass man die Elemente in jeder Liste identifizieren könnte und dann schaut, wie viele in beiden Listen drin sind.
Die übrig gebliebenen Elemente geben den Mindestabstand an.
Die Levenshtein_Distanz liegt dann irgendwozwischen dem Mindestabstand und max(List1.length, List2.length).

Nun hört man mit dem Suchen auf, sobald mehr Paare mit einem "*" gebildet werden, als die Maximaldistanz - min(List1.length, List2.length) groß ist.
Denn größer, als die Länge der größeren lsite wird es niemals werden und sobald mehr "*"-Paare gebildet werden, als die kleinere Liste lang ist, ist die Levenshtein_Distanz kleiner als jede Lösung, die noch kommt.
*/