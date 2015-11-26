%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%

%%%%% Aufgabe 1.1

:- dynamic(zins/4).

% zins(+Anlagebetrag, +Zinsfaktor, +Anlagedauer, ?Endguthaben)
% 'Anlagebetrag', 'Zinsfaktor', 'Anlagedauer' und 'Endguthaben' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'Anlagebetrag' nach der Dauer in Jahren 'Anlagedauer' mit dem Zinsfaktor in Prozent 'Zinsfaktor' ist.

zins(Anlagebetrag, _, 0, Anlagebetrag).

zins(Anlagebetrag, Zinsfaktor, Anlagedauer, Endguthaben) :-
    Anlagedauer2 is Anlagedauer - 1,
    zins(Anlagebetrag, Zinsfaktor, Anlagedauer2, Endguthaben2),
    Endguthaben is (1 + Zinsfaktor / 100) * Endguthaben2.

/** 
 *  Testwerte und Ergebnisse:
 *  
 *  zins(2000, 5, 1, Endguthaben)
 *  Endguthaben = 2100.0 ;
 *  
 *  zins(2000, 5, 3, Endguthaben).
 *  Endguthaben = 2315.25 
 *  
 *  zins(2000, 1, 10, Endguthaben).
 *  Endguthaben = 2209.244250822409
 *  
 *  Diese Testwerte sind zufällig generiert und deshalb aussagekräftig.
 *  
 *  zins(2000, 0, 10, Endguthaben).
 *  Endguthaben = 2000
 *
 *  zins(2000, 100, 0, Endguthaben).
 *  Endguthaben = 2000
 *
 *  zins(0, 100, 100, Endguthaben).
 *  Endguthaben = 0
 *  
 *  Diese Testfälle sind Extremfälle und wurden daher geprüft.
 */ 
 
 %%%%% Aufgabe 1.2
 
 :- dynamic(zins_nr/4).

% zins_nr(+Anlagebetrag, +Zinsfaktor, +Anlagedauer, ?Endguthaben)
% 'Anlagebetrag', 'Zinsfaktor', 'Anlagedauer' und 'Endguthaben' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'Anlagebetrag' nach der Dauer in Jahren 'Anlagedauer' mit dem Zinsfaktor in Prozent 'Zinsfaktor' ist.

zins_nr(Anlagebetrag, Zinsfaktor, Anlagedauer, Endguthaben) :-
    Endguthaben is Anlagebetrag * ((1 + (Zinsfaktor / 100)) ** Anlagedauer).

%   zins_nr (steht für Nicht Rekursiv) ist um eine Nachkommastelle genauer. Auf die gleiche Stellenzahl gerundet, geben sie allerdings das gleiche Ergebnis aus. Zudem bricht zins_nr nach einem Ergebnis ab, zins gibt als zweites Ergebnis immer ein:
%   ;
%   ERROR: Out of local stack

%   zurück.

%%%%% Aufgabe 1.3

:- dynamic(zins_er/4).

% zins_er(+Anlagebetrag, +Zinsfaktor, +Anlagedauer, ?Endguthaben)
% 'Anlagebetrag', 'Zinsfaktor', 'Anlagedauer' und 'Endguthaben' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'Anlagebetrag' nach der Dauer in Jahren 'Anlagedauer' mit dem Zinsfaktor in Prozent 'Zinsfaktor' ist.

:- dynamic(zins_er/5).

% zins_er(_, +Zinsfaktor, +Anlagedauer, ?Endguthaben, +AktuellerBetrag)
% '_', 'Zinsfaktor', 'Anlagedauer', 'Endguthaben' und 'AktuellerBetrag' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'AktuellerBetrag' nach der Dauer in Jahren 'Anlagedauer' mit dem Zinsfaktor in Prozent 'Zinsfaktor' ist.

zins_er(Anlagebetrag, Zinsfaktor, Anlagedauer, Endguthaben) :-
    zins_er(Anlagebetrag, Zinsfaktor, Anlagedauer, Endguthaben, Anlagebetrag).

zins_er(_, _, 0, Endguthaben, AktuellerBetrag) :-
    Endguthaben is AktuellerBetrag.

zins_er(_, Zinsfaktor, Anlagedauer, Endguthaben, AktuellerBetrag) :-
    Anlagedauer2 is Anlagedauer - 1,
    AktuellerBetrag2 = (1 + Zinsfaktor / 100) * AktuellerBetrag,
    zins_er(_, Zinsfaktor, Anlagedauer2, Endguthaben, AktuellerBetrag2).

%%%%% Aufgabe 1.4

:- dynamic(zins_alt/3).

% zins_alt(+Anlagebetrag, +Anlagedauer, ?Endguthaben)
% 'Anlagebetrag', 'Anlagedauer' und 'Endguthaben' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'Anlagebetrag' nach der Dauer in Jahren 'Anlagedauer' ist.

:- dynamic(zins_alt/4).

% zins_alt(+Anlagebetrag, +Anlagedauer, ?Endguthaben, ?VorherigerZins)
% 'Anlagebetrag', 'Anlagedauer', 'Endguthaben' und 'VorherigerZins' sind Argumentpositionen,
% so dass 'Endguthaben' das Endguthaben in Euro eines Anlagebetrages in Euro 'Anlagebetrag' nach der Dauer in Jahren 'Anlagedauer' und 'VorherigerZins' der Zinssatz des letzten Jahres ist.

zins_alt(Anlagebetrag, Anlagedauer, Endguthaben) :-
    zins_alt(Anlagebetrag, Anlagedauer, Endguthaben, _).

zins_alt(Anlagebetrag, 0, Anlagebetrag, 1).

zins_alt(Anlagebetrag, Anlagedauer, Endguthaben, VorherigerZins) :-
    Anlagedauer2 is Anlagedauer - 1,
    zins_alt(Anlagebetrag, Anlagedauer2, Endguthaben2, VorherigerZins2),
    VorherigerZins is VorherigerZins2 + 1/(2 ** (Anlagedauer - 2)),
    Endguthaben is (1 + VorherigerZins / 100) * Endguthaben2.

% Das nicht Endrekursive Verfahren ist geeigneter, da es nur einen:
% ERROR: Out of local stack

% und keinen:

% ERROR: Out of global stack

% produziert.
% Und außerdem ist es intuitiver.

%%%%% Aufgabe 1.5

% zins_nr(Anlagebetrag, 4, Anlagedauer, Endguthaben1), zins_alt(Anlagebetrag, Anlagedauer, Endguthaben2), Endguthaben1 < Endguthaben2.
% obige Methode gibt ab einer Anlagedauer von 4 Jahren true aus. Demzufolge ist die alternative Methode ab 4 Jahren für den Kunden profitabler.

%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%

%%%%% Aufgabe 2.1

