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

:- dynamic(pi/2).

% pi(+Rekursionsschritte, ?Ergebnis)
% 'Rekursionsschritte' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

:- dynamic(pi_rek/3).

% pi_rek(+Rekursionsschritte, +Zaehler, ?Ergebnis)
% 'Rekursionsschritte', 'Zaehler' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

pi(Rekursionsschritte, Ergebnis) :-
    pi_rek(Rekursionsschritte, 1, Ergebnis2),
    Ergebnis is 4 * Ergebnis2.

pi_rek(0, _, 0).

pi_rek(Rekursionsschritte, Zaehler, Ergebnis) :-
    Rekursionsschritte > 0,
    Zaehler2 is Zaehler + 1,
    Rekursionsschritte2 is Rekursionsschritte - 1,
    pi_rek(Rekursionsschritte2, Zaehler2, Ergebnis2),
    Ergebnis is ((-1)**(Zaehler + 1)) / (2 * Zaehler - 1) + Ergebnis2.

:- dynamic(pi2/2).

% pi2(+Rekursionsschritte, ?Ergebnis)
% 'Rekursionsschritte' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

:- dynamic(pi_rek2/3).

% pi_rek2(+Rekursionsschritte, +Zaehler, ?Ergebnis)
% 'Rekursionsschritte', 'Zaehler' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

pi2(Rekursionsschritte, Ergebnis) :-
    pi_rek2(Rekursionsschritte, 1, 0, Ergebnis2),
    Ergebnis is 4 * Ergebnis2.

pi_rek2(0, _, Zwischenergebnis, Zwischenergebnis).

pi_rek2(Rekursionsschritte, Zaehler, Zwischenergebnis, Ergebnis) :-
    Rekursionsschritte > 0,
    Zaehler2 is Zaehler + 1,
    Rekursionsschritte2 is Rekursionsschritte - 1,
    Zwischenergebnis2 is Zwischenergebnis + (((-1)**(Zaehler + 1)) / (2 * Zaehler - 1)),
    pi_rek2(Rekursionsschritte2, Zaehler2, Zwischenergebnis2, Ergebnis).

% Endrekursion liegt bei pi_rek2 vor, wenn das Ergebnis beim rekursiven Abstieg berechnet wird.

%%%%% Aufgabe 2.2

% Der nicht Endrekursive ist verständlicher, weil er mit nur 3 Argumenten auskommt, wohingegen der Endrekursive 4 benötigt, um das Zwischenergebnis mitzuführen.

% Das Berechnungsverhalten ist bei beiden identisch, da sie die gleiche Rechnung durchführen (partiell (unter-)spezifiziert).

%%%%% Aufgabe 2.3

:- dynamic(pi3/2).

% pi3(+Rekursionsschritte, ?Ergebnis)
% 'Rekursionsschritte' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

:- dynamic(pi_rek3/3).

% pi_rek3(+Rekursionsschritte, +Zaehler, ?Ergebnis)
% 'Rekursionsschritte', 'Zaehler' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.

pi3(Rekursionsschritte, Ergebnis) :-
    pi_rek3(Rekursionsschritte, 1, Ergebnis2),
    Ergebnis is 2 * Ergebnis2.

pi_rek3(0, _, 1).

pi_rek3(Rekursionsschritte, Zaehler, Ergebnis) :-
    Rekursionsschritte > 0,
    Zaehler2 is Zaehler + 1,
    Rekursionsschritte2 is Rekursionsschritte - 1,
    pi_rek3(Rekursionsschritte2, Zaehler2, Ergebnis2),
    Ergebnis is (((2 * Zaehler) / (2 * Zaehler - 1)) * ((2 * Zaehler)/(2 * Zaehler + 1))) * Ergebnis2.

% Diese Methode (pi_rek3) konvergiert schneller.

%%%%%%%%%%%%%
% AUFGABE 3 %
%%%%%%%%%%%%%

%%%%% Aufgabe 3.1

%pi(1, X) :- X is 4 * ( ((-1)**(1 + 1)) / (2 * 1 - 1) ).
%pi(2, X) :- X is 4 * ( ((-1)**(1 + 1)) / (2 * 1 - 1) + ((-1)**(2 + 1)) / (2 * 2 - 1) ).
%pi(3, X) :- X is 4 * ( ((-1)**(1 + 1)) / (2 * 1 - 1) + ((-1)**(2 + 1)) / (2 * 2 - 1) + ((-1)**(3 + 1)) / (2 * 3 - 1) ).
%pi(4, X) :- X is 4 * ( ((-1)**(1 + 1)) / (2 * 1 - 1) + ((-1)**(2 + 1)) / (2 * 2 - 1) + ((-1)**(3 + 1)) / (2 * 3 - 1) + ((-1)**(4 + 1)) / (2 * 4 - 1) ).

:- dynamic(pi3/2).

% pi_incr(+Rekursionsschritte, ?Ergebnis)
% 'Rekursionsschritte' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.
% Es werden alle Ergebnisse bis zum Rekusrionsschritt 'Rekursionsschritte' ausgegeben.

:- dynamic(pi_rek4/3).

% pi_rek4(+Rekursionsschritte, +Zaehler, ?Ergebnis)
% 'Rekursionsschritte', 'Zaehler' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' Pi auf 'Rekursionsschritte' Rekursionen genau ist.
% Es werden alle Ergebnisse bis zum Rekusrionsschritt 'Rekursionsschritte' ausgegeben.


pi_incr(Rekursionsschritte, Ergebnis) :-
    pi_rek4(Rekursionsschritte, 1, Ergebnis).

pi_rek4(Rekursionsschritte, Zaehler, Ergebnis) :-
    Zaehler =< Rekursionsschritte,
    pi(Zaehler, Ergebnis).

pi_rek4(Rekursionsschritte, Zaehler, Ergebnis) :-
    Zaehler =< Rekursionsschritte,
    Zaehler2 is Zaehler + 1,
    pi_rek4(Rekursionsschritte, Zaehler2, Ergebnis).

%%%%% Aufgabe 3.2

% Es sind genau 118 Approximationsschritte erforderlich, damit bei der gegebenen Auflösung der Approximationsfehler nicht mehr darstellbar ist.

%%%%%%%%%%%%%
% AUFGABE 4 %
%%%%%%%%%%%%%

%%%%% Aufgabe 4.1

:- dynamic(binomialkoeffizient/3).

% binomialkoeffizient(+N, +K, ?Ergebnis)
% 'N', 'K' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' 'N' über 'K' ist.

binomialkoeffizient(N, K, Ergebnis) :-
    N > 0,
    K > 0,
    N2 is N - 1,
    K2 is K - 1,
    binomialkoeffizient(N2, K2, Ergebnis2),
    binomialkoeffizient(N2, K, Ergebnis3),
    Ergebnis is Ergebnis2 + Ergebnis3.

binomialkoeffizient(_, 0, 1).

binomialkoeffizient(N, N, 1).

% Diese Definition ist nicht Endrekursiv, da es so intuitiver war.

%%%%% Aufgabe 4.Bonus1

% Diese Methode arbeitet das Pascalsche Dreieck durch:
%                                         1                                               n = 0
%                                     1       1                                           n = 1
%                                 1       2       1                                       n = 2
%                             1       3       3       1                                   n = 3
%                         1       4       6       4       1                               n = 4
%                     1       5       10      10      5       1                           n = 5
%                 1       6       15      20      15      6       1                       n = 6
%             1       7       21      35      35      21      7       1                   n = 7
%         1       8       28      56      70      56      28      8       1               n = 8
%     1       9       36      84      126     126     84      36      9       1           n = 9
% 1       10      45      120     210     252     210     120     45      10      1       n = 10

% k=0     k=1     k=2     k=3     k=4     k=5     k=6     k=7     k=8     k=9     k=10

% Wobei jede Zahl (n über k) die Summe der Zahl links über ihr (n-1 über k-1) und der Zahl rechts über ihr (n-1 über k) ist.
% Für jeden rekursiven Aufruf der Methode wird also das gesamte obere Teilfeld für gegebenes n und k rekonstruiert.
% Beispiel: 3 über 2:
%                 1                       n = 0
%             1       1                   n = 1
%         1       2       1               n = 2
%     1       3       3       1           n = 3
% 1       4       6       4       1       n = 4

% k=0     k=1     k=2     k=3     k=4

% Rekursion 0 (Die Felder zeigen die Anzahl ihrr Aufrufe an):
%                 0                       n = 0
%             0       0                   n = 1
%         0       0       0               n = 2
%     0       0       0       0           n = 3
% 0       0       1       0       0       n = 4

% Rekursion 1:
%                 0                       n = 0
%             0       0                   n = 1
%         0       0       0               n = 2
%     0       1       1       0           n = 3
% 0       0       1       0       0       n = 4

% Rekursion 2:
%                 0                       n = 0
%             0       0                   n = 1
%         1       2       1               n = 2
%     0       1       1       0           n = 3
% 0       0       1       0       0       n = 4

% Rekursion 3:
%                 0                       n = 0
%             3       3                   n = 1
%         1       2       1               n = 2
%     0       1       1       0           n = 3
% 0       0       1       0       0       n = 4

% Rekursion 4:
%                 6                       n = 0
%             3       3                   n = 1
%         1       2       1               n = 2
%     0       1       1       0           n = 3
% 0       0       1       0       0       n = 4

% Somit wird ein umgekehrtes Pascalsches Dreieck konstruiert.
% Dieses könnte man auch einfach einmal von oben herunter konstruieren ohne rekursive Aufrufe, sondern indem man sich die Zahlen merkt.
% Damit müsste nur die letzte Zahl unten an der Spitze ausgegeben werden und  der Rechenaufwand wäre minimiert.

%%%%% Aufgabe 4.Bonus2

:- dynamic(binomialkoeffizient2/3).

% binomialkoeffizient2(+N, +K, ?Ergebnis)
% 'N', 'K' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' 'N' über 'K' ist.

binomialkoeffizient2(N, K, Ergebnis) :-
    fak(N, ErgebnisN),
    fak(K, ErgebnisK),
    NK is N - K,
    fak(NK, ErgebnisNK),
    Ergebnis is (ErgebnisN  / (ErgebnisK * ErgebnisNK)).

:- dynamic(binomialkoeffizient3/3).

% binomialkoeffizient3(+N, +K, ?Ergebnis)
% 'N', 'K' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' 'N' über 'K' ist.

:- dynamic(binomialkoeffizient_rek3/4).

% binomialkoeffizient3_rek3(+N, +K, +Zaehler, ?Ergebnis)
% 'N', 'K', 'Zaehler' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' 'N' über 'K' ist.

binomialkoeffizient3(N, K, Ergebnis) :-
    binomialkoeffizient_rek3(N, K, 0, Ergebnis).

binomialkoeffizient_rek3(N, K, Zaehler, Ergebnis) :-
    K > Zaehler,
    Zaehler2 is Zaehler + 1,
    binomialkoeffizient_rek3(N, K, Zaehler2, Ergebnis2),
    Ergebnis is ((N + 1 - Zaehler2) / Zaehler2) * Ergebnis2.

binomialkoeffizient_rek3(_, K, K, 1).



:- dynamic(fak/2).

% fak(+Fakultaet, ?Ergebnis)
% 'Fakultaet' und 'Ergebnis' sind Argumentpositionen,
% so dass 'Ergebnis' 'Fakultaet' Fakultät ist.

fak(Fakultaet, Ergebnis) :-
    Fakultaet > 0,
    Fakultaet2 is Fakultaet - 1,
    fak(Fakultaet2, Ergebnis2),
    Ergebnis is Fakultaet * Ergebnis2.

fak(0, 1).


% binomialkoeffizient:
% nicht endrekursiv
% Berechnungsergebnisse exakt; Berechnungsdauer: langsam; Rekursion kann mehrere Ergebnisse ausgeben, da das Endergebnis irgendwo mitten im Baum gefunden wird, jedoch ist das zweite Ergebnis immer "false"
% partiell (unter-)spezifiziert

% binomialkoeffizient2:
% nicht Endrekursiv (Rekursion ausgelagert in fak/2)
% Berechnungsergebnis exakt; Berechnungsdauer: schnell; Rekursion terminiert immer mit genau einem Ergebnis, weil das Endergebnis erst im letzten Teibaum gefunden wird, daher nur ein Ergebnis
% partiell (unter-)spezifiziert

% binomialkoeffizient3:
% nicht Endrekursiv (Rekursion in Hilfsmethode binomialkoeffizient_rek3/4) 
% Berechnungsergebnis inexakt und als Gleitkommazahl; Berechnungsdauer: schnell; Rekursion kann mehrere Ergebnisse ausgeben, da das Endergebnis irgendwo mitten im Baum gefunden wird, jedoch ist das zweite Ergebnis immer "false"
% partiell (unter-)spezifiziert














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% display.pl

% init_display(Display-Objekt,Fenster-Ueberschrift,Fensterbreite,Fensterhoehe)
init_display(Name,Label,Width,Height):-
   free(Name),
   retractall(display_parameters(_,_,_)),
   assert(display_parameters(Name,Width,Height)),
   new(Name,picture(Label,size(Width,Height))),
   send(Name,open).
   
% display_sequence(Displayname,Liste,Skalierung-x, Skalierung-y)
display_sequence(Name,L,Sx,Sy):-
  ds(Name,L,10,1,Sx,Sy).  % y-offset muss an das Problem angepasst werden 

%ds(Displayname,Liste,Offset-x,Offset-y,Skalierung-x,Skalierung-y)
ds(_,[ ],_,_,_,_).
ds(_,[_],_,_,_,_).
ds(Name,[E1,E2|T],Ox,Oy,Sx,Sy):-
  Ox1 is Ox+Sx,
  display_parameters(Name,_,Height),
  E1s is Height - (E1 + Oy) * Sy,
  E2s is Height - (E2 + Oy) * Sy,
  send(Name,display,new(_,line(Ox,E1s,Ox1,E2s,none))),
%  new(Line, line(Ox,E1s,Ox1,E2s,none) ),
%  new(C,colour(red)),
%  send(Line , colour(C)),	
%  send(Name,display,Line),
  send(Name,flush),
  ds(Name,[E2|T],Ox1,Oy,Sx,Sy).

%display(Fenster-Ueberschrift,Liste)
display(Label,L):-
   init_display(@d,Label,800,300),  % Breite und Hoehe des Fensters muss 
                                    % an das Problem angepasst werden 
   Y is 300 - (pi+1) * 50,  
   new(Pi_line,line(10,Y,800,Y,none)),   % Referenzlinie fÃ¼r pi
   new(C,colour(red)),
   send(Pi_line,colour(C)),
   send(@d,display,Pi_line),
%   send(@d,flush),
   display_sequence(@d,L,5,50).    % Skalierungsfaktoren in x- und y-Richtung
                                    % muessen an das Problem angepasst werden