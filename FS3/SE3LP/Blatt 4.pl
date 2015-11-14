%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AUFGABE 1
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. ist...
%% symmetrisch: nein (A ist ein Jahr, B ist eine Person)
%% reflexiv: nö (ein Jahr kann nicht die Person sein, die in dem Jahr geboren wurde)
%% transitiv: nope (B ist eine Person und kann damit nicht das Jahr von C sein)
%% funktional: joa (1:m, wenn X das Geburtsjahr von Y ist und Z das Geburtsjahr von Z ist, dann X=Y, weil man nur in einem Jahr geboren werden kann )
% 2. ist...
%% symmetrisch: ja (wenn A neben B ist, ist B auch neben A, zumindest nach der Definition im 3-dimensionalen Raum)
%% reflexiv: nö (A nimmt den selben Platz ein wie A, kann also nicht neben A sein)
%% transitiv: nope (A neben B, B neben C, C nicht neben A weil B im Weg)
%% funktional: nah (n:m, A Nachbar von B und C Nachbar von B, dann nicht A=C, weil B zwei Seiten hat)
% 3. ist...
%% symmetrisch: nein (wenn A leichter B, muss B schwerer A sein, somit asymetrisch)
%% reflexiv: nö (A kann nicht leichter als A sein)
%% transitiv: jo (wenn A.Masse kleiner B.Masse und B.Masse kleiner C.Masse, dann A.Masse kleiner C.Masse, weil Algebra)
%% funktional: nah (n:m, da wir vom Gewicht keine Gleicheit ableiten können)
% 4. ist...
%% symmetrisch: nein (siehe oben)
%% reflexiv: japp (A ist kleiner gleich A)
%% transitiv: jo (wenn A kleiner gleich B und B kleiner gleich C, dann A kleiner gleich C, weil Algebra)
%% funktional: nah (n:m, da wir von der Größe keine Gleicheit ableiten können)
% 5. ist...
%% symmetrisch: ja (wenn A schonmal mit B gespielt hat, hat B auch schonmal mit A gespielt)
%% reflexiv: japp (A hat schon das ein oder andere Mal mit sich selber in einer Mannschaft gespielt)
%% transitiv: nope (A und B spielten bei Leverkusen, B und C bei Pauli, A guckt Pauli nicht mit dem Hintern an)
%% funktional: nah (n:m, weil es mehrere Spieler pro Mannschaft gibt)
% 6. ist...
%% symmetrisch: ja (wenn A kongruent B, dann B auch kongruent A)
%% reflexiv: japp (A ist kongruent zu sich selber)
%% transitiv: jo (A kongruent B und B kongruent C dann A kongruent B)
%% funktional: nah (kongruent ja, gleich nicht)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AUFGABE 2
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic obj/5.

% obj(Objektnummer, Objekttyp, Strassenname, Hausnummer, Baujahr).
obj(1,efh,gaertnerstr,15,2005).
obj(2,efh,bahnhofsstr,27,1943).
obj(3,efh,bahnhofsstr,29,1997).
obj(4,mfh,bahnhofsstr,28,1989).
obj(5,bahnhof,bahnhofsstr,30,1901).
obj(6,kaufhaus,bahnhofsstr,26,1997).
obj(7,efh,gaertnerstr,17,1982).

:- dynamic bew/6.

% bew(Vorgangsnr, Objektnr, Verkaeufer, Kaeufer, Preis, Verkaufsdatum)
% Datumsangaben haben die Struktur JJJJMMTT 
bew(1,1,mueller,meier,250000,20120401).
bew(2,3,schulze,schneider,260000,19881213).
bew(3,3,schneider,mueller,315000,20011201).
bew(4,5,bund,piepenbrink,1500000,19980601).
bew(5,7,greta,peter,1,19990101).
bew(6,7,peter,steve,1,20000101).
bew(7,7,steve,dieter,1,20010101).
bew(8,7,dieter,trudi,1,20020101).
bew(9,7,trudi,holger,1,20030101).
bew(10,7,holger,ingrid,1,20040101).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 2.1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic neuereigentuemer/3.

% neuereigentuemer(?Eigentümer, ?Straße, ?Hausnummer)
% 'Eigentümer', 'Straße' und 'Hausnummer' sind Argumentpositionen,
% so dass 'Eigentümer' der Eigentümer vom Haus in der 'Straße' mit der 'Hausnummer' ist.

neuereigentuemer(Eigentuemer, Strasse, Hausnummer) :-
    obj(Objektnummer, _, Strasse, Hausnummer, _),
        bew(_, Objektnummer, _, Eigentuemer, _, Datum),
        (Datum > 20071231),
        \+ (bew(_, Objektnummer, Eigentuemer, _, _, Datum2), Datum2 > Datum).
        
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 2.2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a)
% Diese Variante gibt alle Vorbesitzer aus, angefangen mit dem, der das Haus, als letztes besessen hat.

:- dynamic vorbesitzerAll/3.

% vorbesitzerAll(?ObjektID, ?Besitzer, ?Vorbesitzer)
% 'ObjektID' (Objektnummer), 'Besitzer' und 'Vorbesitzer' sind Argumentpositionen,
% so dass 'Vorbesitzer' der Vorbesitzer vor 'Besitzer' vom Haus 'ObjektID' war.

vorbesitzerAll(ObjektID, Besitzer, Vorbesitzer) :-
    bew(_, ObjektID, Vorbesitzer, Besitzer, _, _),
    print(Vorbesitzer),
    nl,
    vorbesitzerAll(ObjektID, Vorbesitzer, _).

% b)
% Diese Variante gibt alle Vorbesitzer aus, angefangen mit dem, der das Haus zuerst besessen hat.

:- dynamic vorbesitzer/3.

% vorbesitzer(?ObjektID, ?Besitzer, ?Vorbesitzer)
% 'ObjektID' (Objektnummer), 'Besitzer' und 'Vorbesitzer' sind Argumentpositionen,
% so dass 'Vorbesitzer' der Vorbesitzer vor 'Besitzer' vom Haus 'ObjektID' war.

vorbesitzer(ObjektID, Besitzer, Vorbesitzer) :-
    if(bew(_, ObjektID, Vorbesitzer, Besitzer, _, _),
        (vorbesitzer(ObjektID, Vorbesitzer, _),
        nl,
        print(Vorbesitzer)),
        print("Vorbesitzer:")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AUFGABE 3
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 3.1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic ist_betroffen_von/2.

% ist_betroffen_von(?Ort1, ?Ort2)
% 'Ort1' und 'Ort2' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt.

ist_betroffen_von(Ort1, Ort2) :-
    stromaufwaerts(_, Ort2, Ort1, _).

ist_betroffen_von(Ort1, Ort2) :-
    ist_betroffen_von(Ort1, Ort3),
    ist_betroffen_von(Ort3, Ort2).

% Diese Funktion ist...
%% symmetrisch: nein (Wenn Ort1 stromaufwärts von Ort2 liegt, kann Ort2 nicht gleichzeitig stromaufwärts von Ort1 liegen)
%% reflexiv: nö (Ort1 kann nicht stromaufwärts von sich selber liegen)
%% transitiv: jo (wenn Ort2 stromaufwärts von Ort1 und Ort3 stromaufwärts von Ort2, dann liegt auch Ort3 stromaufwärts von Ort1)
%% funktional: nah (n:m, da es mehr als 2 Orte an einem Fluss geben kann)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 3.2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic trinkwasserverbot/2.

% trinkwasserverbot(?Orte)
% 'Orte' ist eine Argumentposition,
% so dass 'Orte' die Liste aller Orte ist, die von einem Störfall im Chemiewerk Bitterfeld betroffen sind,
% die also stromabwärts von Bitterfeld liegen.

trinkwasserverbot(Orte) :-
    findall(Ort,
    ist_betroffen_von(Ort, bitterfeld),
    BetroffeneOrte).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 3.3
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic ist_erreichbar_von/2.

% ist_erreichbar_von(?Ort1, ?Ort2)
% 'Ort1' und 'Ort2' sind Argumentpositionen,
% so dass 'Ort1' von 'Ort2' aus über Flüsse erreichbar ist.

% 'Ort2' liegt stromaufwärts von 'Ort1'.
ist_erreichbar_von(Ort1, Ort2) :-
    ist_betroffen_von(Ort1, Ort2).
    
% 'Ort2' liegt stromabwärts von 'Ort1'.
ist_erreichbar_von(Ort1, Ort2) :-
    ist_betroffen_von(Ort2, Ort1).

% 'Ort2' liegt erst stromaufwärts, dann stromabwärts von 'Ort1'.
ist_erreichbar_von(Ort1, Ort2) :-
    ist_betroffen_von(Ort1, Ort3),
    ist_betroffen_von(Ort2, Ort3).

% 'Ort2' liegt erst stromabwärts, dann stromaufwärts von 'Ort1'.
ist_erreichbar_von(Ort1, Ort2) :-
    ist_betroffen_von(Ort3, Ort1),
    ist_betroffen_von(Ort3, Ort2).

% Diese Funktion ist...
%% symmetrisch: ja (Wenn Ort1 von Ort2 aus erreichbar ist, ist Ort2 auch von Ort1 aus erreichbar)
%% reflexiv: japp (Ort1 ist von sich selber aus erreichbar)
%% transitiv: jo (wenn Ort2 von Ort1 aus erreichbar ist und Ort3 von Ort2 aus erreichbar ist, dann ist Ort3 auch von Ort1 aus erreichbar)
%% funktional: nah (n:m, da es mehr als 2 Orte an einem Fluss geben kann)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Aufgabe 3.4
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Möglichkeit 1

%%%% Ergänzugen  der Datenbasis

:- dynamic kanal/5.

% kanal(?Fluss1, ?Ort_am_Fluss1, ?Fluss2, ?Ort_am_Fluss2, ?Länge)
% 'Fluss1', 'Ort_am_Fluss1', 'Fluss2', 'Ort_am_Fluss2' und 'Länge' sind Argumentpositionen,
% so dass der Kanal 'Ort_am_Fluss1' am 'Fluss1' mit 'Ort_am_Fluss2' am 'Fluss2' verbindet und 'Länge' Kilometer lang ist.

% unnötige Kanäle für diese Repräsentation
% kanal(elbe, muendung_moldau, moldau, muendung_moldau, 10). %Moldaukanal
% kanal(elbe, muendung_saale, saale, muendung_saale, 15). %Saalekanal
% kanal(elbe, oder, ). %Donau-Oder-Elbe-Kanal

% sinnvolle Kanäle für diese Repräsentation
kanal(elbe, magdeburg, havel, brandenburg, 55). %Elbe-Havel-Kanal
kanal(spree, berlin_mitte, oder, eisenhuettenstadt, 41). %Oder-Spree-Kanal
kanal(oder, schwedt, havel, brandenburg, 54). %Oder-Havel-Kanal

%%%% Prädikatverhalten

% ist_betroffen_von sollte sich nicht verändern, da die Kanäle selber keine Stromrichtung und oftmals sogar Schleusen besitzen.
% Dies tut es auch nicht.

% ist_erreichbar_von sollte sich verändern, da nun mehr Orte von unterschiedlichen anderern Orten erreichbar sind.
% Bisher schaut ist_erreichbar_von nur von Fluss zu Fluss. Nun wurden allerdings Kanäle hinzugefügt. Diese müsste nun auch noch geprüft werden.

:- dynamic ist_erreichbar_von2/2.

% ist_erreichbar_von2(?Ort1, ?Ort2)
% 'Ort1' und 'Ort2' sind Argumentpositionen,
% so dass 'Ort1' von 'Ort2' aus über Flüsse und Kanäle erreichbar ist.

% 'Ort2' liegt stromaufwärts von 'Ort1'.
ist_erreichbar_von2(Ort1, Ort2) :-
    ist_betroffen_von(Ort1, Ort2).

% 'Ort1' besitzt einen Kanal zu 'Ort2'.
ist_erreichbar_von2(Ort1, Ort2) :-
    kanal(_, Ort1, _, Ort2, _).
    
% Wenn 'Ort1' von 'Ort2' aus erreichbar ist, sollte es auch andersherum der Fall sein.
ist_erreichbar_von2(Ort1, Ort2) :-
    ist_erreichbar_von2(Ort2, Ort1).

% 'Ort1' ist auch von 'Ort2' aus erreichbar, wenn es einen Ort gibt, der von beiden Orten aus erreichbar ist.
ist_erreichbar_von2(Ort1, Ort2) :-
    ist_erreichbar_von2(Ort1, Ort3),
    ist_erreichbar_von2(Ort2, Ort3).

%%%%% Möglichkeit 2

%%%% Ergänzen der Datenbasis

stromaufwaerts(elbe_havel_kanal, magdeburg, brandenburg, 55).
stromaufwaerts(oder_spree_kanal, berlin_mitte, eisenhuettenstadt, 41).
stromaufwaerts(oder_havel_kanal, schwedt, brandenburg, 54).

%%%% Prädikatverhalten

% ist_betroffen_von sollte sich nicht verändern, da die Kanäle selber keine Stromrichtung und oftmals sogar Schleusen besitzen.
% Dies tut es trotzdem, da die Kanäle eine Fließrichtung bekommen durch die Art, wie sie in die Datenbank eingetragen wurden. Dies müsste geändert werden.

:- dynamic ist_betroffen_von2/2.

% ist_betroffen_von2(?Ort1, ?Ort2)
% 'Ort1' und 'Ort2' sind Argumentpositionen,
% so dass 'Ort1' vom Abwasser von 'Ort2' betroffen ist,
% also 'Ort2' stromaufwärts von 'Ort1' liegt.

% Wenn der Fluss kein Kanal ist, dann arbeite wie bisher,
% sonst gib false aus.
ist_betroffen_von2(Ort1, Ort2) :-
    if((stromaufwaerts(Fluss_oder_kanal, Ort2, Ort1, _),
        (Fluss_oder_kanal = elbe_havel_kanal;
        Fluss_oder_kanal = oder_havel_kanal;
        Fluss_oder_kanal = oder_spree_kanal)),
        1=0,
        stromaufwaerts(_ , Ort2, Ort1, _)).

ist_betroffen_von2(Ort1, Ort2) :-
    ist_betroffen_von(Ort1, Ort3),
    ist_betroffen_von(Ort3, Ort2).

% ist_erreichbar_von sollte sich verändern, da nun mehr Orte von unterschiedlichen anderern Orten erreichbar sind.
% Dies tut es automatisch, da die Kanäle nach dem selben Prinzip wie Flüsse in der Datenbank stehen.




















%%%%%%%% Das funktioniert. Die Methode unten aber nicht. Warum auch immer... O.o            
            
        ObjektID = 7,
        Besitzer = ingrid,
        AnzahlRekursionen = 1,
        if(AnzahlRekursionen > 0,
            (bew(_, ObjektID, Vorbesitzer, Besitzer, _, _),
            bew(_, ObjektID, Vorvorbesitzer, Vorbesitzer, _, _),
            AnzahlRekursionen2 is AnzahlRekursionen - 1,
            if(AnzahlRekursionen2 > 0,
                (bew(_, ObjektID, Vorvorbesitzer, Vorbesitzer, _, _),
                bew(_, ObjektID, Vorvorvorbesitzer, Vorvorbesitzer, _, _),
                AnzahlRekursionen3 is AnzahlRekursionen2 - 1),
                Rueckgabewert = Vorvorbesitzer)),
            Rueckgabewert = Vorbesitzer).   
            
            
            
            
            
:- dynamic tuEtwas/5.            
        % ObjektID = 7
        % Besitzer = ingrid
        % AnzahlRekursionen = 1
tuEtwas(ObjektID, Besitzer, Vorbesitzer, AnzahlRekursionen, Rueckgabewert) :-
    if(AnzahlRekursionen > 0,
        (bew(_, ObjektID, Vorbesitzer, Besitzer, _, _),
        bew(_, ObjektID, Vorvorbesitzer, Vorbesitzer, _, _),
        AnzahlRekursionen2 is AnzahlRekursionen - 1,
        tuEtwas(ObjektID, Vorbesitzer, Vorvorbesitzer, AnzahlRekursionen2, Rueckgabewert).
        Rueckgabewert = Vorbesitzer).

% Würde es funktionieren, wäre das ein etwas umfangreicherer Lösungsansatzfür 2.2:

:- dynamic vorbesitzer2/3.
:- dynamic vorbesitzer2_rek/4.
:- dynamic vorbesitzer2_for/5. 
        
vorbesitzer2(ObjektID, Besitzer, Vorbesitzer) :-
    bew(_, ObjektID, Vorbesitzer, Besitzer, _, _).
	
vorbesitzer2(ObjektID, Besitzer, Vorbesitzer) :-
    vorbesitzer2_rek(ObjektID, Besitzer, Vorbesitzer, 1).

vorbesitzer2_rek(ObjektID, Besitzer, Vorbesitzer, AnzahlRekursionen) :-
    vorbesitzer2_for(ObjektID, Besitzer, Vorbesitzer, AnzahlRekursionen, Rueckgabewert),
    vorbesitzer2(ObjektID, Vorbesitzer, Rueckgabewert),
    Vorbesitzer = Rueckgabewert.
    
vorbesitzer2_for(ObjektID, Besitzer, Vorbesitzer, AnzahlRekursionen, Rueckgabewert) :-
    if(AnzahlRekursionen > 0,
        (bew(_, ObjektID, Vorbesitzer, Besitzer, _, _),
        bew(_, ObjektID, Vorvorbesitzer, Vorbesitzer, _, _),
        AnzahlRekursionen2 is AnzahlRekursionen - 1,
        vorbesitzer2_for(ObjektID, Vorbesitzer, Vorvorbesitzer, AnzahlRekursionen2, Rueckgabewert),
    Rueckgabewert = Vorbesitzer).






    
    
    
    
    







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% stuff für das if



/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sicstus,
	  [ (block)/1,			% +Heads

	    if/3,			% :If, :Then, :Else

	    use_module/3,		% ?Module, ?File, +Imports

	    bb_put/2,			% :Key, +Value
	    bb_get/2,			% :Key, -Value
	    bb_delete/2,		% :Key, -Value
	    bb_update/3,		% :Key, -Old, +New

	    create_mutable/2,		% ?Value, -Mutable
	    get_mutable/2,		% ?Value, +Mutable
	    update_mutable/2,		% ?Value, !Mutable

	    read_line/1,		% -Codes
	    read_line/2,		% +Stream, -Codes

	    trimcore/0,

%	    call_residue/2,		% :Goal, -Residue

	    prolog_flag/3,		% +Flag, -Old, +New
	    prolog_flag/2,		% +Flag, -Value

	    op(1150, fx, (block))
	  ]).

:- use_module(sicstus/block).
:- use_module(library(occurs)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).


/** <module> SICStus compatibility library

This library is intended to be activated   using  the directive below in
files that are designed for use with  SICStus Prolog. The changes are in
effect until the end of the file and in each file loaded from this file.

    ==
    :- expects_dialect(sicstus).
    ==

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package.
*/

:- multifile
	system:goal_expansion/2.


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_sicstus_library
%
%	Pushes searching for dialect/sicstus in   front of every library
%	directory that contains such as sub-directory.

push_sicstus_library :-
	(   absolute_file_name(library(dialect/sicstus), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, sicstus))),
	    fail
	;   true
	).


:- push_sicstus_library.


		 /*******************************
		 *	      OPERATORS		*
		 *******************************/

%	declare all operators globally

system:goal_expansion(op(Pri,Ass,Name),
		      op(Pri,Ass,user:Name)) :-
	\+ qualified(Name),
	prolog_load_context(dialect, sicstus).

qualified(Var) :- var(Var), !, fail.
qualified(_:_).


%%	setup_dialect
%
%	Further dialect initialization.

setup_dialect.


		 /*******************************
		 *	      CONTROL		*
		 *******************************/

:- meta_predicate
	if(0,0,0).

system:goal_expansion(if(If,Then,Else),
		      (If *-> Then ; Else)) :-
	prolog_load_context(dialect, sicstus),
	\+ (sub_term(X, [If,Then,Else]), X == !).

%%	if(:If, :Then, :Else)
%
%	Same  as  SWI-Prolog  soft-cut  construct.   Normally,  this  is
%	translated using goal-expansion. If either term contains a !, we
%	use meta-calling for full compatibility (i.e., scoping the cut).

if(If, Then, Else) :-
	(   If
	*-> Then
	;   Else
	).


		 /*******************************
		 *	  LIBRARY MODULES	*
		 *******************************/

%%	rename_module(?SICStusModule, ?RenamedSICSTusModule) is nondet.
%
%	True if RenamedSICSTusModule is the  name   that  we use for the
%	SICStus native module SICStusModule. We do  this in places where
%	the module-name conflicts. All explicitely   qualified goals are
%	mapped to the SICStus equivalent of the module.

:- multifile
	rename_module/2.

system:goal_expansion(M:Goal, SicstusM:Goal) :-
	atom(M),
	rename_module(M, SicstusM),
	prolog_load_context(dialect, sicstus).


		 /*******************************
		 *	     MODULES		*
		 *******************************/

% SICStus use_module/1 does not require the target to be a module.

system:goal_expansion(use_module(File), load_files(File, [if(changed)])).

%%	use_module(+Module, -File, +Imports) is det.
%%	use_module(-Module, +File, +Imports) is det.
%
%	This predicate can be used to import   from a named module while
%	the file-location of the module is unknown   or to get access to
%	the module-name loaded from a file.
%
%	If both Module and File are  given,   we  use  Module and try to
%	unify File with the absolute  canonical   path  to the file from
%	which Module was loaded. However, we   succeed regardless of the
%	success of this unification.

use_module(Module, File, Imports) :-
	atom(Module), !,
	module_property(Module, file(Path)),
	use_module(Path, Imports),
	ignore(File = Path).
use_module(Module, File, Imports) :-
	ground(File), !,
	absolute_file_name(File, Path,
			   [ file_type(prolog),
			     access(read)
			   ]),
	use_module(Path, Imports),
	module_property(Module, file(Path)).
use_module(Module, _, _Imports) :-
	instantiation_error(Module).


		 /*******************************
		 *	 FOREIGN RESOURCES      *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SICStus uses foreign_resource(Name, Functions) and predicate definitions
similar to Quintus. qpforeign can generate  the   glue  code that can be
linked with swipl-ld. This  part  of   the  emulation  merely  skips the
declarations and Maps load_foreign_resource   to load_foreign_resource/2
from library(qpforeign).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

system:term_expansion(
	   (:- load_foreign_resource(Base)),
	   (:- initialization(load_foreign_resource(M:Base, Source), now))) :-
	prolog_load_context(source, Source),
	prolog_load_context(module, M).
system:term_expansion(
	   (:- module(Name, Exports, Options)),
	   [ (:- module(Name, Exports))
	   | Declarations
	   ]) :-
	prolog_load_context(dialect, sicstus),
	phrase(sicstus_module_decls(Options), Declarations).

sicstus_module_decls([]) --> [].
sicstus_module_decls([H|T]) -->
	sicstus_module_decl(H),
	sicstus_module_decls(T).

sicstus_module_decl(hidden(true)) --> !,
	[(:- set_prolog_flag(generate_debug_info, false))].
sicstus_module_decl(_) -->
	[].


		 /*******************************
		 *	       BB_*		*
		 *******************************/

:- meta_predicate
	bb_put(:, +),
	bb_get(:, -),
	bb_delete(:, -),
	bb_update(:, -, +).

system:goal_expansion(bb_put(Key, Value), nb_setval(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_get(Key, Value), nb_current(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_delete(Key, Value),
		      (	  nb_current(Atom, Value),
			  nb_delete(Atom)
		      )) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_update(Key, Old, New),
		      (	  nb_current(Atom, Old),
			  nb_setval(Atom, New)
		      )) :-
	bb_key(Key, Atom).

bb_key(Module:Key, Atom) :-
	atom(Module), !,
	atomic(Key),
	atomic_list_concat([Module, Key], :, Atom).
bb_key(Key, Atom) :-
	atomic(Key),
	prolog_load_context(module, Module),
	atomic_list_concat([Module, Key], :, Atom).

%%	bb_put(:Name, +Value) is det.
%%	bb_get(:Name, -Value) is semidet.
%%	bb_delete(:Name, -Value) is semidet.
%%	bb_update(:Name, -Old, +New) is semidet.
%
%	SICStus compatible blackboard routines. The implementations only
%	deal with cases where the module-sensitive   key  is unknown and
%	meta-calling. Simple cases are  directly   mapped  to SWI-Prolog
%	non-backtrackable global variables.

bb_put(Key, Value) :-
	bb_key(Key, Name),
	nb_setval(Name, Value).
bb_get(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value).
bb_delete(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value),
	nb_delete(Name).
bb_update(Key, Old, New) :-
	bb_key(Key, Name),
	nb_current(Name, Old),
	nb_setval(Name, New).


		 /*******************************
		 *	     MUTABLES		*
		 *******************************/

%%	create_mutable(?Value, -Mutable) is det.
%
%	Create a mutable term with the given initial Value.
%
%	@compat sicstus

create_mutable(Value, '$mutable'(Value,_)).

%%	get_mutable(?Value, +Mutable) is semidet.
%
%	True if Value unifies with the current value of Mutable.
%
%	@compat sicstus

get_mutable(Value, '$mutable'(Value,_)).

%%	update_mutable(?Value, !Mutable) is det.
%
%	Set the value of Mutable to Value.  The old binding is
%	restored on backtracking.
%
%	@see setarg/3.
%	@compat sicstus

update_mutable(Value, Mutable) :-
	functor(Mutable, '$mutable', 2), !,
	setarg(1, Mutable, Value).
update_mutable(_, Mutable) :-
	type_error(mutable, Mutable).


		 /*******************************
		 *	   LINE READING		*
		 *******************************/

%%	read_line(-Codes) is det.
%%	read_line(+Stream, -Codes) is det.
%
%	Read a line from the given or  current input. The line read does
%	_not_ include the line-termination character. Unifies Codes with
%	=end_of_file= if the end of the input is reached.
%
%	@compat sicstus
%	@see	The SWI-Prolog primitive is read_line_to_codes/2.

read_line(Codes) :-
    read_line_to_codes(current_input, Codes).

read_line(Stream, Codes) :-
    read_line_to_codes(Stream, Codes).


		 /*******************************
		 *  COROUTINING & CONSTRAINTS	*
		 *******************************/

/* This is more complicated.  Gertjan van Noord decided to use
   copy_term/3 in Alpino.

%%	call_residue(:Goal, -Residue) is nondet.
%
%	Residue is a list of VarSet-Goal.  Note that this implementation
%	is   incomplete.   Please   consult     the   documentation   of
%	call_residue_vars/2 for known issues.

:- meta_predicate
	call_residue(0, -).

call_residue(Goal, Residue) :-
	call_residue_vars(Goal, Vars),
	(   Vars == []
	->  Residue = []
	;   copy_term(Vars, _AllVars, Goals),
	    phrase(vars_by_goal(Goals), Residue)
	).

vars_by_goal((A,B)) --> !,
	vars_by_goal(A),
	vars_by_goal(B).
vars_by_goal(Goal) -->
	{ term_attvars(Goal, AttVars),
	  sort(AttVars, VarSet)
	},
	[ VarSet-Goal ].
*/

%%	trimcore
%
%	Trims the stacks.  Other tasks of the SICStus trimcore/0 are
%	automatically scheduled by SWI-Prolog.

trimcore :-
	trim_stacks.


		 /*******************************
		 *	       FLAGS		*
		 *******************************/

%%	prolog_flag(+Flag, -Old, +New) is semidet.
%
%	Query and set a Prolog flag. Use the debug/1 topic =prolog_flag=
%	to find the flags accessed using this predicate.

prolog_flag(Flag, Old, New) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q, ~q)', [Flag, Old, New]),
	current_prolog_flag(Flag, Old),
	set_prolog_flag(Flag, New).

%%	prolog_flag(+Flag, -Value) is semidet.
%
%	Query a Prolog flag, mapping SICSTus flags to SWI-Prolog flags

prolog_flag(Flag, Value) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q)', [Flag, Value]),
	sicstus_flag(Flag, Value).

sicstus_flag(system_type, Type) :- !,
	(   current_prolog_flag(saved_program, true)
	->  Type = runtime
	;   Type = development
	).
sicstus_flag(Name, Value) :-
	current_prolog_flag(Name, Value).


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

% Provide (#)/2 as arithmetic function.  Ideally, we should be able to
% bind multiple names to built-in functions.  This is rather slow.  We
% could also consider adding # internally, but not turning it into an
% operator.

:- op(500, yfx, #).

:- arithmetic_function(user:(#)/2).
:- arithmetic_function(user:(\)/2).

user:(#(X,Y,R)) :-				% SICStus 3
	R is xor(X,Y).
user:(\(X,Y,R)) :-				% SICStus 4
	R is xor(X,Y).


		 /*******************************
		 *	       HACKS		*
		 *******************************/

%%	prolog:'$breaklevel'(-BreakLevel, Unknown)
%
%	Query the current break-level

prolog:'$breaklevel'(BreakLevel, _) :-
	current_prolog_flag(break_level, BreakLevel), !.
prolog:'$breaklevel'(0, _).
