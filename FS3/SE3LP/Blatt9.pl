%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%

%%%% Aufgabe 1

% Gebrauchsanweisung:
% Eingabe in die Konsole: eliza("%EIGENER SATZ%", "%ELIZAS ANTWORT%").
% eliza(+Eingabe, -Ausgabe).
% Keine Satzzeichen, Großbuchstaben oder Umlaute im eigenen Satz.
% Eigener Satz in Anführungszichen: "ein eigener satz".
% Eliza antwortet anch einem bestimmten Muster und sucht Schlüsselwörter in dem eigegebenen Satz und wählt dementsprechend ihre Antworten aus.

% Beispieldialog:
%?- eliza("hallo", Antwort).
%Antwort = "guten tag . ich bin eliza . was hast du fuer ein problem ? ".
%?- eliza("ich bin traurig", Antwort).
%Antwort = "seit wann bist du traurig ? ".
%?- eliza("seit gestern", Antwort).
%Antwort = "was ist gestern passiert ? ".
%?- eliza("ich habe meinen vater getroffen", Antwort).
%Antwort = "erzaehl mir mehr . ".
%?- eliza("mein vater hasst mich", Antwort).
%Antwort = "wie kommst du darauf , dass dein vater dich hasst ? ".
%?- eliza("weil ich drogen genommen habe", Antwort).
%Antwort = "warum hast du drogen genommen ? ".
%?- eliza("weil es mir schlecht ging", Antwort).
%Antwort = "warum ging es dir schlecht ? ".
%?- eliza("ich hab in der uni verkackt", Antwort).
%Antwort = "wie war es als du in der uni verkackt hast ? ".
%?- eliza("es war schlimm", Antwort).
%Antwort = "wie kam es dazu ? ".
%?- eliza("ich hatte nicht gelernt", Antwort).
%Antwort = "erzaehl mir mehr . ".
%?- eliza("das war es danke ich werde in zukunft mehr lernen", Antwort).
%Antwort = "es freut mich wenn ich helfen konnte . "
%?- eliza("tschuess", Antwort).
%Antwort = "tschuess . ".





:- dynamic(rule/2).

% rule(Pattern, Response)
% 'Pattern' und 'Response' sind Argumentpositionen,
% so dass 'Response' die Ausgabe auf eine Eingabe 'Pattern' ist.
    
rule2(Pattern, Response) :-
    findall(Patterns2, rule(Patterns2, _), Liste),
    member(Patterns, Liste),
    member(Pattern, Patterns),
    rule(Patterns, Response).

rule2(Pattern, Response) :-
    findall(Patterns2, rule(Patterns2, _), Liste),
    member(Pattern, Liste),
    rule(Pattern, Response).

member(E,[E|_]).
member(E,[_|R]) :- member(E,R).

rule([[hallo]], [guten, tag, ., ich, bin, eliza, ., was, hast, du, fuer, ein, problem, ?]).
rule([[ich, bin| Gefuehl]], [seit, wann, bist, du| Gefuehl3]) :-
    ichNachdu(Gefuehl, Gefuehl2),
    append(Gefuehl2, [?], Gefuehl3).
rule([[ich, fuehle, mich| Zustand]], [wie, lange, fuehlst, du, dich, schon, sehr| Zustand3]) :-
    ichNachdu(Zustand, Zustand2),
    append(Zustand2, [?], Zustand3).
rule([[es, geht, mir| Zustand]], [warum, geht, es, dir| Zustand3]) :-
    ichNachdu(Zustand, Zustand2),
    append(Zustand2, [?], Zustand3).
rule([[seit, Zeit| _]], [was, ist, Zeit, passiert, ?]).

rule([[weil, ich, Art, Verb, habe| _]], [warum, hast, du, Art, Verb, ?]).

rule([[weil, es, Pronomen, Adjektiv, Verb| _]], [warum, Verb, es, Pronomen2, Adjektiv, ?]) :-
    ichNachdu([Pronomen], [Pronomen2]),
    istAdjektiv(Adjektiv),
    istPronomen(Pronomen).

rule([[es, war, Adjektiv| _]], [wie, kam, es, dazu, ?]) :-
    istAdjektiv(Adjektiv).

rule([[ich, habe| Tat]], [wie, war, es, als, du| Tat3]) :-
    ichNachdu(Tat, Tat2),
    append(Tat2, [?], Tat3).
rule([[ich, hab| Tat]], [wie, war, es, als, du| Tat3]) :-
    ichNachdu(Tat, Tat2),
    append(Tat2, [hast, ?], Tat3).

rule([[ich, habe, ein, problem, mit| Problemausloeser]], [erzähl, mir, doch, von| Problemausloeser3]) :-
    ichNachdu(Problemausloeser, Problemausloeser2),
    append(Problemausloeser2, [.], Problemausloeser3).
rule([[tschuess]], [tschuess, .]) :-
    halt.
rule([[ich, habe, das, gefuehl, Pronomen, Mensch, Gefuehl, Pronomen2| _]], [wie, kommst, du, darauf, ,, dass, Pronomen3, Mensch, Pronomen4, Gefuehl, ?]) :-
    ichNachdu([Pronomen], [Pronomen3]),
    ichNachdu([Pronomen2], [Pronomen4]).
rule([[Pronomen, Mensch, Gefuehl, Pronomen2| _]], [wie, kommst, du, darauf, ,, dass, Pronomen3, Mensch, Pronomen4, Gefuehl, ?]) :-
    ichNachdu([Pronomen], [Pronomen3]),
    ichNachdu([Pronomen2], [Pronomen4]),
    istPronomen(Pronomen),
    istPronomen(Pronomen2).

rule([[das, war, es| _]], [es, freut, mich, wenn , ich, helfen, konnte, .]).
rule([[danke| _]], [gern, geschehen]).
rule([[_| _]], [erzaehl, mir, mehr, .]).

% macht ichs zu dus und ähnliches und schneited satzzeichen am ende ab.

ichNachdu([.], []).
ichNachdu([!], []).
ichNachdu([?], []).
ichNachdu([], []).
ichNachdu([ich| Eingabe], [du| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([mir| Eingabe], [dir| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([mein| Eingabe], [dein| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([meins| Eingabe], [deins| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([mich| Eingabe], [dich| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([meine| Eingabe], [deine| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([meiner| Eingabe], [deiner| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([uns| Eingabe], [ihr| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([unser| Eingabe], [euer| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([wir| Eingabe], [ihr| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([unserem| Eingabe], [eurem| Ausgabe]) :-
    !,
    ichNachdu(Eingabe, Ausgabe).
ichNachdu([ErstesElement| Eingabe], [ErstesElement| Ausgabe]) :-
    ichNachdu(Eingabe, Ausgabe).

% prüft, ob eine Eingabe ein Pronomen ist.

istPronomen(ich).
istPronomen(mir).
istPronomen(mein).
istPronomen(meins).
istPronomen(mich).
istPronomen(meine).
istPronomen(meiner).
istPronomen(uns).
istPronomen(unser).
istPronomen(wir).
istPronomen(unserem).

% prüft, ob eine Eingabe ein Pronomen ist.

istAdjektiv(gut).
istAdjektiv(schlecht).
istAdjektiv(super).
istAdjektiv(toll).
istAdjektiv(miserabel).
istAdjektiv(blöd).
istAdjektiv(perfekt).
istAdjektiv(außergewöhnlich).
istAdjektiv(schlimm).

% wandelt einen String in eine Liste um

wandleEingabeUm(String, Ausgabe) :-
    string_to_list(String, Liste),
    wandleEingabeUm(Liste, Ausgabe, []),
    !.

wandleEingabeUm([], [Ausgabe], Puffer) :-
    atom_codes(Ausgabe, Puffer).

wandleEingabeUm([ErstesElement| Restliste], Ausgabe, Puffer) :-
    ErstesElement \= 32,
    append(Puffer, [ErstesElement], Puffer2),
    wandleEingabeUm(Restliste, Ausgabe, Puffer2).

wandleEingabeUm([32| Restliste], Ausgabe, Puffer) :-
    Puffer \= [],
    atom_codes(Atom, Puffer),
    wandleEingabeUm(Restliste, Ausgabe2, []),
    append([Atom], Ausgabe2, Ausgabe).

% wandelt eine Liste in einen String um

wandleAusgabeUm(Liste, Ausgabe) :-
    wandleAusgabeUm2(Liste, Ausgabe2),
    atom_string(Ausgabe2, Ausgabe).

wandleAusgabeUm2([], '') :-
    !.
    
wandleAusgabeUm2([ErstesElement|Restliste], Ausgabe) :-
    wandleAusgabeUm2(Restliste, Ausgabe2),
    atom_concat(ErstesElement, ' ', Atom),
    atom_concat(Atom, Ausgabe2, Ausgabe).

% gibt die kleinste Zahl zwischen Start und Ende aus.
% bei fragen nach neuen Ergebnissen werden anch udn anch alle ausgegeben.

iterator(Start, Ende, Start) :-
    Start =< Ende.

iterator(Start, Ende, Ergebnis) :-
    Start2 is Start + 1,
    Start2 =< Ende,
    iterator(Start2, Ende, Ergebnis).

term(Term, In, Tail) :-
        with_output_to(codes(In, Tail), write(Term)).



% Startprädikat

eliza(Eingabe, Ausgabe) :-
    wandleEingabeUm(Eingabe, Eingabe2),
    rule([Eingabe2], Ausgabe2),
    wandleAusgabeUm(Ausgabe2, Ausgabe),
    !.


%%% Aufgabe 1.6
%% funktioniert nicht

% eliza(Eingabe, Ausgabe) :-
%    wandleEingabeUm(Eingabe, Eingabe2),
%    findall(Ausgabe2, rule2(Eingabe2, Ausgabe2), MoeglicheAusgaben),
%    length(MoeglicheAusgaben, Laenge),
%    Laenge \= 0,
%    Laenge2 is Laenge - 1,
%    random_between(0, Laenge2, Index),
%    nth0(Index, MoeglicheAusgaben, Ausgabe).

% eliza(Eingabe, Ausgabe) :-
%    wandleEingabeUm(Eingabe, Eingabe2),
%    findall(Ausgabe2, rule2(Eingabe2, Ausgabe2), MoeglicheAusgaben),
%    length(MoeglicheAusgaben, Laenge),
%    Laenge = 0,
%    Ausgabe = [erzaehl, mir, mehr, .].





%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%

%%%% Aufgabe 2