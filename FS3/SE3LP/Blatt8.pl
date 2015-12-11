%%%%%%%%%%%%%
% AUFGABE 1 %
%%%%%%%%%%%%%

%%%% Aufgabe 1.1

:- dynamic(muenze/1).

% alleUnterverzeichnisse(?Wert)
% 'Wert' ist eine Argumentposition,
% so dass 'Wert' der Wert einer Münze ist.

muenze(200).
muenze(100).
muenze(50).
muenze(20).
muenze(10).
muenze(5).
muenze(2).
muenze(1).



:- dynamic(wechselgeld/2).

% wechselgeld(?Wert, ?Zerlegung)
% 'Wert' und 'Zerlegung' sind Argumentpositionen,
% so dass 'Zerlegung' eine mögliche Zerlegung eines Wertes 'Wert' in Münzen ist.

wechselgeld(0, []).
wechselgeld(Wert, Liste) :-
    muenze(Muenzwert),
    Wert2 is Wert - Muenzwert,
    Wert2 >= 0,
    wechselgeld(Wert2, Liste2),
    Liste = [Muenzwert|Liste2].

%%%% Aufgabe 1.2

:- dynamic(wechselgeld2/2).

% wechselgeld2(?Wert, ?Zerlegung)
% 'Wert' und 'Zerlegung' sind Argumentpositionen,
% so dass 'Zerlegung' eine mögliche Zerlegung eines Wertes 'Wert' in Münzen ist.

wechselgeld2(Wert, Zerlegung) :-
    findall(X, muenze(X), AlleMuenzen),
    sort(0, @>, AlleMuenzen, AlleMuenzenSortiert),
    wechselgeld2_rek(Wert, Zerlegung, [], AlleMuenzenSortiert).


:- dynamic(wechselgeld2_rek/3).

% wechselgeld2_rek(?Wert, ?Zerlegung, +Teilliste, +NochZulaessigeMuenzen)
% 'Wert', 'Zerlegung', 'Teilliste' und 'NochZulaessigeMuenzen' sind Argumentpositionen,
% so dass 'Zerlegung' eine mögliche Zerlegung eines Wertes 'Wert' in Münzen ist.
% 'Teilliste' ist dabei die Liste an Münzen deren Wert 'Wert' entspricht.
% 'NochZulaessigeMuenzen' sind dabei die Münzen, die noch benutzt werden dürfen.

wechselgeld2_rek(0, Ergebnis, Liste, _) :-
    sort(0, @>=, Liste, Ergebnis).
wechselgeld2_rek(Wert, Ergebnis, Liste, NochZulaessigeMuenzen) :-
    NochZulaessigeMuenzen = [GroessteMuenze|_],
    Wert2 is Wert - GroessteMuenze,
    Wert2 >= 0,
    wechselgeld2_rek(Wert2, Ergebnis, [GroessteMuenze|Liste], NochZulaessigeMuenzen).
wechselgeld2_rek(Wert, Ergebnis, Liste, [_|KleinereMuenzen]) :-
    Wert > 0,
    wechselgeld2_rek(Wert, Ergebnis, Liste, KleinereMuenzen).


% Es wurde dieser Lösungsansatz gewählt, da sich an den Hinweis auf dem Aufgabenzettel gehalten wurde:
% Dazu wurde erste das Prädikat aus Aufgabe 1 Endrekursiv gemacht
% und dann ein weiterer Parameter für die noch zulässigen Münzen eingebaut.



%%%% Aufgabe 1.3


:- dynamic(minListe/2).

% minListe(?Wert, ?Zerlegung)
% 'Wert' und 'Zerlegung' sind Argumentpositionen,
% so dass 'Zerlegung' eine mögliche Zerlegung eines Wertes 'Wert' in Münzen ist.

minListe(Wert, Ergebnis) :-
    findall(Ergebnis2, wechselgeld2(Wert, Ergebnis2), Ergebnisse),
    findeKuerzestesElement(Ergebnisse, Ergebnis).

:- dynamic(findeKuerzestesElement/2).

% findeKuerzestesElement(?Liste, ?KuerzesteTeilliste)
% 'Liste' und 'KuerzesteTeilliste' sind Argumentpositionen,
% so dass 'KuerzesteTeilliste' die kürzeste Teilliste der Liste 'Liste' ist.

findeKuerzestesElement([EinzigesElement], EinzigesElement).
findeKuerzestesElement(Liste, KuerzesteTeilliste) :-
    Liste = [ErstesElement|Restliste],
    findeKuerzestesElement_rek(Restliste, KuerzesteTeilliste, ErstesElement).

:- dynamic(findeKuerzestesElement_rek/3).

% findeKuerzestesElement_rek(?Liste, ?Ergebnis, +AktuellKuerzestesElement)
% 'Liste', 'KuerzesteTeilliste' und 'AktuellKuerzestesElement' sind Argumentpositionen,
% so dass 'Ergebnis' die kürzeste Teilliste der Liste 'Liste' ist.
% 'AktuellKuerzestesElement' ist dabei das aktuell kürzeste gefundene Element.

findeKuerzestesElement_rek([], Ergebnis, Ergebnis).
findeKuerzestesElement_rek([ErstesElement|Restliste], Ergebnis, AktuellKuerzestesElement):-
    length(ErstesElement, LaengeNeu),
    length(AktuellKuerzestesElement, LaengeAktuell),
    LaengeNeu >= LaengeAktuell,
    findeKuerzestesElement_rek(Restliste, Ergebnis, AktuellKuerzestesElement).
findeKuerzestesElement_rek([ErstesElement|Restliste], Ergebnis, AktuellKuerzestesElement):-
    length(ErstesElement, LaengeNeu),
    length(AktuellKuerzestesElement, LaengeAktuell),
    LaengeNeu < LaengeAktuell,
    findeKuerzestesElement_rek(Restliste, Ergebnis, ErstesElement).
    

%%%% Aufgabe 1.4

% Das Prädikat aus Teilaufgabe 3 müsste dann immer noch funktionieren.
% Es wurde keine Bedingung gefunden, die das Münzsystem erfüllen müsste.
% Mit dem Prädikat minListe/2 wurde die Optimalität bereits für beliebige Münzsysteme garantiert.

%%%% Aufgabe 1.5

:- dynamic(muenzspeicher/2).

% muenzspeicher(?muenze(Wert), ?Anzahl)
% 'muenze(Wert)' und 'Anzahl' sind Argumentpositionen,
% so dass 'Anzahl' die Anzahl der Münzen der Art 'muenze(Wert)' im Münzspeicher ist.

muenzspeicher(muenze(200), 50).
muenzspeicher(muenze(100), 50).
muenzspeicher(muenze(50), 50).
muenzspeicher(muenze(20), 50).
muenzspeicher(muenze(10), 50).
muenzspeicher(muenze(5), 50).
muenzspeicher(muenze(2), 50).
muenzspeicher(muenze(1), 50).


:- dynamic(minListe2/4).

% minListe2(+Wert, ?Zerlegung, +Geldspeicher, ?AktualisierterGeldspeicher)
% 'Wert', 'Zerlegung' und 'Geldspeicher' sind Argumentpositionen,
% so dass 'Zerlegung' eine mögliche Zerlegung eines Wertes 'Wert' in Münzen ist.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

minListe2(Wert, Ergebnis, Geldspeicher, AktualisierterGeldspeicher) :-
    findall(Ergebnis2, wechselgeld2(Wert, Ergebnis2), Ergebnisse),
    findeKuerzestesElement2(Ergebnisse, Ergebnis, Geldspeicher),
    alleMuenzenVorhanden(Ergebnis, Geldspeicher, AktualisierterGeldspeicher),
    berechneGesamtwert(Ergebnis, Wert).

:- dynamic(generiereGeldspeicher/1).

% generiereGeldspeicher(?Geldspeicher)
% 'Geldspeicher' ist eine Argumentposition,
% so dass 'Geldspeicher' ein generierter Geldspeicher aus der internen Dateinbank (muenzspeicher/2) ist.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

generiereGeldspeicher(Geldspeicher) :-
    findall([Wert, Anzahl], muenzspeicher(muenze(Wert), Anzahl), Geldspeicher).

:- dynamic(findeKuerzestesElement2/3).

% findeKuerzestesElement2(+Liste, ?KuerzesteTeilliste, ?Geldspeicher)
% 'Liste', 'KuerzesteTeilliste' und 'Geldspeicher' sind Argumentpositionen,
% so dass 'KuerzesteTeilliste' die kürzeste Teilliste der Liste 'Liste' ist.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

findeKuerzestesElement2(Liste, KuerzesteTeilliste, Geldspeicher) :-
    findeLaengsteTeilliste(Liste, LaengsteTeilliste, Geldspeicher),
    findeKuerzestesElement_rek2(Liste, KuerzesteTeilliste, LaengsteTeilliste, Geldspeicher).

:- dynamic(findeLaengsteTeilliste/3).

% findeLaengsteTeilliste(+Liste, ?LaengsteTeilliste, +Geldspeicher)
% 'Liste', 'LaengsteTeilliste' und 'Geldspeicher' sind Argumentpositionen,
% so dass 'LaengsteTeilliste' die längste Teilliste der Liste 'Liste' ist.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

findeLaengsteTeilliste([], [], _).
findeLaengsteTeilliste(Liste, LaengsteTeilliste, Geldspeicher) :-
    findeLaengsteTeilliste_rek(Liste, LaengsteTeilliste, [], Geldspeicher).

:- dynamic(findeLaengsteTeilliste_rek/4).

% findeLaengsteTeilliste_rek(+Liste, ?LaengsteTeilliste, +AktuellLaengsteTeilliste, +Geldspeicher)
% 'Liste', 'LaengsteTeilliste', 'AktuellLaengsteTeilliste' und 'Geldspeicher' sind Argumentpositionen,
% so dass 'LaengsteTeilliste' die längste Teilliste der Liste 'Liste' ist.
% 'AktuellLaengsteTeilliste' ist dabei die aktuell längste gefundene Teilliste. Sie ist zu Anfang die leere Liste (möglichst kurz).
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

findeLaengsteTeilliste_rek([], LaengsteTeilliste, LaengsteTeilliste, _).
findeLaengsteTeilliste_rek([H|T], LaengsteTeilliste, AktuellLaengsteTeilliste, Geldspeicher) :-
    length(H, LaengeNeu),
    length(AktuellLaengsteTeilliste, LaengeAktuell),
    LaengeNeu >= LaengeAktuell,
    alleMuenzenVorhanden(H, Geldspeicher, _),
    findeLaengsteTeilliste_rek(T, LaengsteTeilliste, H, Geldspeicher).
findeLaengsteTeilliste_rek([H|T], LaengsteTeilliste, AktuellLaengsteTeilliste, Geldspeicher) :-
    length(H, LaengeNeu),
    length(AktuellLaengsteTeilliste, LaengeAktuell),
    LaengeNeu >= LaengeAktuell,
    \+ alleMuenzenVorhanden(H, Geldspeicher, _),
    findeLaengsteTeilliste_rek(T, LaengsteTeilliste, AktuellLaengsteTeilliste, Geldspeicher).
findeLaengsteTeilliste_rek([H|T], LaengsteTeilliste, AktuellLaengsteTeilliste, Geldspeicher) :-
    length(H, LaengeNeu),
    length(AktuellLaengsteTeilliste, LaengeAktuell),
    LaengeNeu < LaengeAktuell,
    findeLaengsteTeilliste_rek(T, LaengsteTeilliste, AktuellLaengsteTeilliste, Geldspeicher).

:- dynamic(findeKuerzestesElement_rek2/4).

% findeKuerzestesElement_rek2(+Liste, ?Ergebnis, +AktuellKuerzestesElement, +Geldspeicher)
% 'Liste', 'KuerzesteTeilliste', 'AktuellKuerzestesElement' und 'Geldspeicher' sind Argumentpositionen,
% so dass 'Ergebnis' die kürzeste Teilliste der Liste 'Liste' ist.
% 'AktuellKuerzestesElement' ist dabei das aktuell kürzeste gefundene Element.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

findeKuerzestesElement_rek2([], Ergebnis, Ergebnis, _).
findeKuerzestesElement_rek2([ErstesElement|Restliste], Ergebnis, AktuellKuerzestesElement, Geldspeicher):-
    length(ErstesElement, LaengeNeu),
    length(AktuellKuerzestesElement, LaengeAktuell),
    LaengeNeu > LaengeAktuell,
    findeKuerzestesElement_rek2(Restliste, Ergebnis, AktuellKuerzestesElement, Geldspeicher).
findeKuerzestesElement_rek2([ErstesElement|Restliste], Ergebnis, AktuellKuerzestesElement, Geldspeicher):-
    length(ErstesElement, LaengeNeu),
    length(AktuellKuerzestesElement, LaengeAktuell),
    LaengeNeu =< LaengeAktuell,
    alleMuenzenVorhanden(ErstesElement, Geldspeicher, _),
    findeKuerzestesElement_rek2(Restliste, Ergebnis, ErstesElement, Geldspeicher).
findeKuerzestesElement_rek2([ErstesElement|Restliste], Ergebnis, AktuellKuerzestesElement, Geldspeicher):-
    length(ErstesElement, LaengeNeu),
    length(AktuellKuerzestesElement, LaengeAktuell),
    LaengeNeu =< LaengeAktuell,
    \+ alleMuenzenVorhanden(ErstesElement, Geldspeicher, _),
    findeKuerzestesElement_rek2(Restliste, Ergebnis, AktuellKuerzestesElement, Geldspeicher).

:- dynamic(alleMuenzenVorhanden/3).

% alleMuenzenVorhanden(+Liste, +Geldspeicher, ?AktualisierterGeldspeicher)
% 'Liste', 'Geldspeicher' und 'AktualisierterGeldspeicher' sind Argumentpositionen,
% so dass geprüft wird, ob jede Müenze aus der Liste 'Liste' im Münzspeicher 'Geldspeicher' vorhanden ist.
% 'Geldspeicher' ist dabei die Menge an zur verfügung stehenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].
% 'AktualisierterGeldspeicher' ist dabei die Menge an nach Abzug der Liste übrig bleibenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

alleMuenzenVorhanden([], Geldspeicher, Geldspeicher).
alleMuenzenVorhanden([H|T], Geldspeicher, AktualisierterGeldspeicher2):-
    entferneAusGeldspeicher(H, Geldspeicher, AktualisierterGeldspeicher),
    alleMuenzenVorhanden(T, AktualisierterGeldspeicher, AktualisierterGeldspeicher2).
    


:- dynamic(entferneAusGeldspeicher/3).

% entferneAusGeldspeicher(+Element, +Liste, ?AktualisierterGeldspeicher)
% 'Element', 'Liste' und 'AktualisierterGeldspeicher' sind Argumentpositionen,
% so dass die Liste 'Liste' ein Münzspeicher ist und eine Münze vom Wert 'Element' aus ihm entfernt wird.
% 'AktualisierterGeldspeicher' ist dabei die Menge an nach Abzug des Elements übrig bleibenden Münzen als Liste von Listen der Form [[Münzwert1, Anzahl1], [Münzwert2, Anzahl2]...[MünzwertN, AnzahlN]].

entferneAusGeldspeicher(Element, [[Element, Anzahl]|T], [[Element, Anzahl2]|T]) :-
    Anzahl2 is Anzahl - 1,
    Anzahl2 >= 0.
entferneAusGeldspeicher(Element, [[Element2, Anzahl]|T], AktualisierterGeldspeicher) :-
    Element \= Element2,
    entferneAusGeldspeicher(Element, T, AktualisierterGeldspeicher2),
    AktualisierterGeldspeicher = [[Element2, Anzahl]|AktualisierterGeldspeicher2].






%%%%% Aufgabe 1.Bonus

:- dynamic(automat/4).

% automat(+BezahlteMuenzen, ?Wechselgeld, +Preis, +Geldspeicher, ?AktualisierterGeldspeicher)
% 'BezahlteMuenzen', 'Wechselgeld', 'Preis', 'Geldspeicher' und 'AktualisierterGeldspeicher' sind Argumentpositionen,
% so dass 'Preis' der Preis ist, der bezahlt werden soll,
% 'BezahlteMuenzen' die Liste an Münzen, die zum bezahlen eingesteckt wurden,
% 'Wechselgeld' das rausgegebene Wechselgeld in Münzen,
% 'Geldspeicher' das dem Automaten zur verfügung stehenden Geld und
% 'AktualisierterGeldspeicher' der Geldspeicher nach dem Bezahlvorgang.

% Idee: Differenz zwischen Preis und den eingeworfenen Münzen berechnen.
% Wenn >= 0, dann Münzen zum Geldspeicher hinzufügen,
% Wechselgeld aus Geldspeicher berechnen und
% Wechselgeld aus Geldspeicher abziehen.


automat(BezahlteMuenzen, Wechselgeld, Preis, Geldspeicher, AktualisierterGeldspeicher) :-
    berechneGesamtwert(BezahlteMuenzen, Gesamtwert),
    Differenz is Gesamtwert - Preis,
    Differenz >= 0,
    sort(0, @>=, BezahlteMuenzen, SortierteMuenzen),
    fuegeMuenzenHinzu(SortierteMuenzen, Geldspeicher, NeuerGeldspeicher),
    minListe2(Differenz, Wechselgeld, NeuerGeldspeicher, AktualisierterGeldspeicher).

:- dynamic(berechneGesamtwert/2).

% berechneGesamtwert(+Liste, ?Gesamtwert)
% 'Liste' und 'Gesamtwert' sind Argumentpositionen,
% so dass 'Gesamtwert' der Wert aller Werte einer Liste aus Zahlen 'Liste' addiert ist.

berechneGesamtwert([], 0).
berechneGesamtwert([H|T], Gesamtwert) :-
    berechneGesamtwert(T, Wert),
    Gesamtwert is Wert + H.

:- dynamic(fuegeMuenzenHinzu/3).

% fuegeMuenzenHinzu(+BezahlteMuenzen, +Geldspeicher, ?NeuerGeldspeicher)
% 'BezahlteMuenzen', 'Geldspeicher' und 'NeuerGeldspeicher' sind Argumentpositionen,
% so dass 'NeuerGeldspeicher' der Geldspeicher 'Geldspeicher' nach Hinzufügen der Münzen 'BezahlteMuenzen' ist.

fuegeMuenzenHinzu([], Geldspeicher, Geldspeicher).
fuegeMuenzenHinzu([ErsterMuenzwert|RestlicheMuenzwerte], [[Wert, Anzahl]|Restliste], NeuerGeldspeicher) :-
    ErsterMuenzwert > Wert,
    fuegeMuenzenHinzu(RestlicheMuenzwerte, [[ErsterMuenzwert, 1]|[[Wert, Anzahl]|Restliste]], NeuerGeldspeicher).
    %NeuerGeldspeicher = [[ErsterMuenzwert, 1]|AktuellerGeldspeicher].
fuegeMuenzenHinzu([ErsterMuenzwert|RestlicheMuenzwerte], [[ErsterMuenzwert, Anzahl]|Restliste], NeuerGeldspeicher) :-
    Anzahl2 is Anzahl + 1,
    fuegeMuenzenHinzu(RestlicheMuenzwerte, [[ErsterMuenzwert, Anzahl2]|Restliste], NeuerGeldspeicher).
    %NeuerGeldspeicher = [[ErsterMuenzwert, Anzahl2]|AktuellerGeldspeicher].
fuegeMuenzenHinzu([ErsterMuenzwert|RestlicheMuenzwerte], [[Wert, Anzahl]|Restliste], NeuerGeldspeicher) :-
    ErsterMuenzwert < Wert,
    fuegeMuenzenHinzu([ErsterMuenzwert|RestlicheMuenzwerte], Restliste, AktuellerGeldspeicher),
    NeuerGeldspeicher = [[Wert, Anzahl]|AktuellerGeldspeicher].
fuegeMuenzenHinzu([H|T], [], NeuerGeldspeicher) :-
    fuegeMuenzenHinzu(T, [], AktualisierterGeldspeicher),
    NeuerGeldspeicher = [[H, 1]|AktualisierterGeldspeicher].


%%%%%%%%%%%%%
% AUFGABE 2 %
%%%%%%%%%%%%%

%%%% Aufgabe 2.1

% Da wir mit Texten arbeiten wurden die orthografischen Schlüssel gewählt.

%%%% Aufgabe 2.2

% CFG:
% Terminale: S(Startsymbol), U(Unterbaum),
% Nichtterminale: 
% S -> U|SU
% U ->










































%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% woerterbuch.pl


dictionary(Dictionary) :-
	findall(Entry, (Entry=entry(_, _), Entry), Dictionary).
entry('Aachen', ['a:', x, @, n]).
entry(abartig, [a, p, 'a:', r, t, 'I', 'C']).
entry(abbrechen, [a, p, b, r, 'E', 'C', @, n]).
entry('Abbruch', [a, p, b, r, 'U', x]).
entry(abchecken, [a, p, tS, 'E', k, @, n]).
entry('Aachen', ['a:', x, @, n]).
entry(abartig, [a, p, 'a:', r, t, 'I', 'C']).
entry(abbrechen, [a, p, b, r, 'E', 'C', @, n]).
entry('Abbruch', [a, p, b, r, 'U', x]).
entry(abchecken, [a, p, tS, 'E', k, @, n]).
entry('Abendbrot', ['a:', b, @, n, t, b, r, 'o:', t]).
entry('Abenden', ['a:', b, @, n, d, @, n]).
entry(abende, ['a:', b, @, n, d, @]).
entry('Abendeßchen', ['a:', b, @, n, t, 'E', s, 'C', @, n]).
entry(abendessen, ['a:', b, @, n, t, 'E', s, @, n]).
entry('Abendessens', ['a:', b, @, n, t, 'E', s, @, n, s]).
entry('Abendflüge', ['a:', b, @, n, t, f, l, 'y:', g, @]).
entry('Abendflug', ['a:', b, @, n, t, f, l, 'u:', k]).
entry('Abendgestaltung', ['a:', b, @, n, t, g, @, 'S', t, a, l, t, 'U', 'N']).
entry('Abendkasse', ['a:', b, @, n, t, k, a, s, @]).
entry(abendlich, ['a:', b, @, n, t, l, 'I', 'C']).
entry('Abendöffnung', ['a:', b, @, n, t, '9', f, n, 'U', 'N']).
entry('Abendplanung', ['a:', b, @, n, t, p, l, 'a:', n, 'U', 'N']).
entry('Abendprogramme', ['a:', b, @, n, t, p, r, o, g, r, a, m, @]).
entry('Abendprogramm', ['a:', b, @, n, t, p, r, o, g, r, a, m]).
entry('Abendprogramms', ['a:', b, @, n, t, p, r, o, g, r, a, m, s]).
entry(abend, ['a:', b, @, n, t]).
entry(abends, ['a:', b, @, n, ts]).
entry('Abendstunden', ['a:', b, @, n, tS, t, 'U', n, d, @, n]).
entry('Abendtermine', ['a:', b, @, n, t, t, 'E', '6', m, 'i:', n, @]).
entry('Abendtermin', ['a:', b, @, n, t, t, 'E', '6', m, 'i:', n]).
entry('Abendtreffen', ['a:', b, @, n, t, t, r, 'E', f, @, n]).
entry('Abendtr', ['a:', b, @, n, t, t, r]).
entry('Abendunternehmungen', ['a:', b, @, n, t, 'U', n, t, '6', n, 'e:', m, 'U', 'N', @, n]).
entry('Abendzug', ['a:', b, @, n, t, ts, 'u:', k]).
entry('Aben', ['a:', b, @, n]).
entry(abenschließenden, ['a:', b, @, n, 'S', l, 'i:', s, @, n, d, @, n]).
entry('Aberglaubens', ['a:', b, '6', g, l, aU, b, @, n, s]).
entry(abergläubisch, ['a:', b, '6', g, l, 'OY', b, 'I', 'S']).
entry(aber, ['a:', b, '6']).
entry(abfahren, [a, pf, 'a:', r, @, n]).
entry('Abfahr', [a, pf, 'a:', r]).
entry('Abfahrt', [a, pf, 'a:', r, t]).
entry(abfährt, [a, pf, 'E:', '6', t]).
entry('Abfahrtstermin', [a, pf, 'a:', r, ts, t, 'E', '6', m, 'i:', n]).
entry('Abfahrtszeiten', [a, pf, 'a:', r, ts, ts, aI, t, @, n]).
entry('Abfahrtszeit', [a, pf, 'a:', r, ts, ts, aI, t]).
entry(abfassen, [a, pf, a, s, @, n]).
entry('Abfassung', [a, pf, a, s, 'U', 'N']).
entry('Abfertigungshalle', [a, pf, 'E', '6', t, 'I', g, 'U', 'N', s, h, a, l, @]).
entry('Abfertigungs', [a, pf, 'E', '6', t, 'I', g, 'U', 'N', s]).
entry(abfliegen, [a, pf, l, 'i:', g, @, n]).
entry('Abflüge', [a, pf, l, 'y:', g, @]).
entry('Abflugmöglichkeit', [a, pf, l, 'u:', k, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Abflug', [a, pf, l, 'u:', k]).
entry('Abflüg', [a, pf, l, 'y:', k]).
entry('Abflugszeiten', [a, pf, l, 'u:', k, s, ts, aI, t, @, n]).
entry('Abflugterminal', [a, pf, l, 'u:', k, t, '9', '6', m, 'I', n, @, l]).
entry('Abflugtermin', [a, pf, l, 'u:', k, t, 'E', '6', m, 'i:', n]).
entry('Abflugzeiten', [a, pf, l, 'u:', k, ts, aI, t, @, n]).
entry('Abflugzeit', [a, pf, l, 'u:', k, ts, aI, t]).
entry(abf, [a, pf]).
entry(abgearbeitet, [a, p, g, @, a, r, b, aI, t, @, t]).
entry(abgeben, [a, p, g, 'e:', b, @, n]).
entry(abgeblieben, [a, p, g, @, b, l, 'i:', b, @, n]).
entry(abgedeckt, [a, p, g, @, d, 'E', k, t]).
entry(abgefunden, [a, p, g, @, f, 'U', n, d, @, n]).
entry(abgegangen, [a, p, g, @, g, a, 'N', @, n]).
entry(abgegeben, [a, p, g, @, g, 'e:', b, @, n]).
entry(abgehakt, [a, p, g, @, h, 'a:', k, t]).
entry(abgehandelt, [a, p, g, @, h, a, n, d, @, l, t]).
entry(abgehen, [a, p, g, 'e:', @, n]).
entry(abgeholt, [a, p, g, @, h, 'o:', l, t]).
entry(abgeht, [a, p, g, 'e:', h, t]).
entry(abgeklappert, [a, p, g, @, k, l, a, p, '6', t]).
entry(abgeklär, [a, p, g, @, k, l, 'E:', '6']).
entry(abgeklärt, [a, p, g, @, k, l, 'E:', '6', t]).
entry(abgelaufen, [a, p, g, @, l, aU, f, @, n]).
entry(abgelegen, [a, p, g, @, l, 'e:', g, @, n]).
entry(abgelehnt, [a, p, g, @, l, 'e:', n, t]).
entry(abgelenkt, [a, p, g, @, l, 'E', 'N', k, t]).
entry(abgemachte, [a, p, g, @, m, a, x, t, @]).
entry(abgemacht, [a, p, g, @, m, a, x, t]).
entry(abgeneigt, [a, p, g, @, n, aI, k, t]).
entry(abgenommen, [a, p, g, @, n, 'O', m, @, n]).
entry(abge, [a, p, g, @]).
entry(abgereist, [a, p, g, @, r, aI, s, t]).
entry(abgeschafft, [a, p, g, @, 'S', a, f, t]).
entry(abgeschlossen, [a, p, g, @, 'S', l, 'O', s, @, n]).
entry(abgesehen, [a, p, g, @, z, 'e:', @, n]).
entry(abgespannt, [a, p, g, @, 'S', p, a, n, t]).
entry(abgesprochen, [a, p, g, @, 'S', p, r, 'O', x, @, n]).
entry(abgestiegen, [a, p, g, @, 'S', t, 'i:', g, @, n]).
entry(abgestimmt, [a, p, g, @, 'S', t, 'I', m, t]).
entry(abgestürzt, [a, p, g, @, 'S', t, 'Y', '6', ts, t]).
entry(abgewickelt, [a, p, g, @, v, 'I', k, @, l, t]).
entry(abgleichen, [a, p, g, l, aI, 'C', @, n]).
entry(abhaken, [a, p, h, 'a:', k, @, n]).
entry(abhalten, [a, p, h, a, l, t, @, n]).
entry(abhandeln, [a, p, h, a, n, d, @, l, n]).
entry(abhängen, [a, p, h, 'E', 'N', @, n]).
entry(abhängig, [a, p, h, 'E', 'N', 'I', 'C']).
entry(abhetzen, [a, p, h, 'E', ts, @, n]).
entry(abholen, [a, p, h, 'o:', l, @, n]).
entry(abhole, [a, p, h, 'o:', l, @]).
entry(abhol, [a, p, h, 'o:', l]).
entry(abkapseln, [a, p, k, a, p, s, @, l, n]).
entry(abklären, [a, p, k, l, 'E:', r, @, n]).
entry(abkläre, [a, p, k, l, 'E:', r, @]).
entry(abkömmlich, [a, p, k, '9', m, l, 'I', 'C']).
entry(ablaufen, [a, p, l, aU, f, @, n]).
entry('Ablauf', [a, p, l, aU, f]).
entry(ablehnen, [a, p, l, 'e:', n, @, n]).
entry(ablehne, [a, p, l, 'e:', n, @]).
entry('Ableitner', [a, p, l, aI, t, n, '6']).
entry('Ablenkung', [a, p, l, 'E', 'N', k, 'U', 'N']).
entry(abmachen, [a, p, m, a, x, @, n]).
entry(abmache, [a, p, m, a, x, @]).
entry('Abmachung', [a, p, m, a, x, 'U', 'N']).
entry('Abom', [a, b, 'O', m]).
entry('Abonnements', [a, b, 'O', n, @, m, 'a~:', s]).
entry(ab, [a, p]).
entry(abraten, [a, p, r, 'a:', t, @, n]).
entry(abrechnen, [a, p, r, 'E', 'C', n, @, n]).
entry('Abrechnungs', [a, p, r, 'E', 'C', n, 'U', 'N', s]).
entry(abreisebereit, [a, p, r, aI, z, @, b, @, r, aI, t]).
entry(abreisen, [a, p, r, aI, z, @, n]).
entry('Abreise', [a, p, r, aI, z, @]).
entry('Abreisetermin', [a, p, r, aI, z, @, t, 'E', '6', m, 'i:', n]).
entry('Abresch', [a, p, r, 'E', 'S']).
entry(abrunden, [a, p, r, 'U', n, d, @, n]).
entry('Absacker', [a, p, z, a, k, '6']).
entry(absagen, [a, p, z, 'a:', g, @, n]).
entry(absa, [a, p, z, a]).
entry(abschätzen, [a, p, 'S', 'E', ts, @, n]).
entry('Abschied', [a, p, 'S', 'i:', t]).
entry(abschlagen, [a, p, 'S', l, 'a:', g, @, n]).
entry(abschlägigen, [a, p, 'S', l, 'E:', g, 'I', g, @, n]).
entry(abschließendes, [a, p, 'S', l, 'i:', s, @, n, d, @, s]).
entry(abschließend, [a, p, 'S', l, 'i:', s, @, n, t]).
entry(abschließen, [a, p, 'S', l, 'i:', s, @, n]).
entry('Abschlußbesprech', [a, p, 'S', l, 'U', s, b, @, 'S', p, r, 'E', 'C']).
entry('Abschluß', [a, p, 'S', l, 'U', s]).
entry('Abschlußsitzung', [a, p, 'S', l, 'U', s, z, 'I', ts, 'U', 'N']).
entry('Abschnitte', [a, p, 'S', n, 'I', t, @]).
entry('Abschnitt', [a, p, 'S', n, 'I', t]).
entry(absch, [a, p, 'S']).
entry(abseilen, [a, p, z, aI, l, @, n]).
entry(absetzen, [a, p, z, 'E', ts, @, n]).
entry(absolute, [a, p, z, o, l, 'u:', t, @]).
entry(absolut, [a, p, z, o, l, 'u:', t]).
entry(absolvieren, [a, p, z, 'O', l, v, 'i:', r, @, n]).
entry('Absprache', [a, p, 'S', p, r, 'a:', x, @]).
entry(absprechen, [a, p, 'S', p, r, 'E', 'C', @, n]).
entry(abspringen, [a, p, 'S', p, r, 'I', 'N', @, n]).
entry('Abständen', [a, p, 'S', t, 'E', n, d, @, n]).
entry('Abstand', [a, p, 'S', t, a, n, t]).
entry(abstatten, [a, p, 'S', t, a, t, @, n]).
entry(abstecken, [a, p, 'S', t, 'E', k, @, n]).
entry(absteigen, [a, p, 'S', t, aI, g, @, n]).
entry(absteige, [a, p, 'S', t, aI, g, @]).
entry(absteigt, [a, p, 'S', t, aI, k, t]).
entry(abstei, [a, p, 'S', t, aI]).
entry(abstellen, [a, p, 'S', t, 'E', l, @, n]).
entry(abstimmen, [a, p, 'S', t, 'I', m, @, n]).
entry('Abstriche', [a, p, 'S', t, r, 'I', 'C', @]).
entry(absurderweise, [a, p, z, 'U', '6', d, '6', v, aI, z, @]).
entry('Abteilen', [a, p, t, aI, l, @, n]).
entry('Abteile', [a, p, t, aI, l, @]).
entry('Abteil', [a, p, t, aI, l]).
entry('Abteilungen', [a, p, t, aI, l, 'U', 'N', @, n]).
entry('Abteilung', [a, p, t, aI, l, 'U', 'N']).
entry('Abteilungs-Geschichte', [a, p, t, aI, l, 'U', 'N', s, g, @, 'S', 'I', 'C', t, @]).
entry('Abteilungsleiter', [a, p, t, aI, l, 'U', 'N', s, l, aI, t, '6']).
entry(abwarten, [a, p, v, a, r, t, @, n]).
entry(abwechselnd, [a, p, v, 'E', k, s, @, l, n, t]).
entry(abwechseln, [a, p, v, 'E', k, s, @, l, n]).
entry('Abwechslung', [a, p, v, 'E', k, s, l, 'U', 'N']).
entry(abwesend, [a, p, v, 'e:', z, @, n, t]).
entry(abwickeln, [a, p, v, 'I', k, @, l, n]).
entry('Abwicklung', [a, p, v, 'I', k, l, 'U', 'N']).
entry(abzubesprechen, [a, p, ts, u, b, @, 'S', p, r, 'E', 'C', @, n]).
entry(abzudampfen, [a, p, ts, u, d, a, m, pf, @, n]).
entry(abzufahren, [a, p, ts, u, f, 'a:', r, @, n]).
entry(abzufassen, [a, p, ts, u, f, a, s, @, n]).
entry(abzüglich, [a, p, ts, 'y:', k, l, 'I', 'C']).
entry(abzuhalten, [a, p, ts, u, h, a, l, t, @, n]).
entry(abzuholen, [a, p, ts, u, h, 'o:', l, @, n]).
entry(abzuklären, [a, p, ts, u, k, l, 'E:', r, @, n]).
entry(abzulenken, [a, p, ts, u, l, 'E', 'N', k, @, n]).
entry(abzumachen, [a, p, ts, u, m, a, x, @, n]).
entry(abzureisen, [a, p, ts, u, r, aI, z, @, n]).
entry(abzurunden, [a, p, ts, u, r, 'U', n, d, @, n]).
entry(abzusprechen, [a, p, ts, u, 'S', p, r, 'E', 'C', @, n]).
entry(abzustecken, [a, p, ts, u, 'S', t, 'E', k, @, n]).
entry(abzusteigen, [a, p, ts, u, 'S', t, aI, g, @, n]).
entry(abzustimmen, [a, p, ts, u, 'S', t, 'I', m, @, n]).
entry(abzuwickeln, [a, p, ts, u, v, 'I', k, @, l, n]).
entry(abzwicken, [a, p, ts, v, 'I', k, @, n]).
entry(ach, [a, x]).
entry(achtem, [a, x, t, @, m]).
entry(achten, [a, x, t, @, n]).
entry(achte, [a, x, t, @]).
entry(achter, [a, x, t, '6']).
entry(achthundert, [a, x, t, h, 'U', n, d, '6', t]).
entry(acht, [a, x, t]).
entry('Acht-Uhr-fünf-Flug', [a, x, t, 'u:', '6', f, 'Y', n, f, f, l, 'u:', k]).
entry('Acht-Uhr-Maschine', [a, x, t, 'u:', '6', m, a, 'S', 'i:', n, @]).
entry('Acht-Uhr-Termin', [a, x, t, 'u:', '6', t, 'E', '6', m, 'i:', n]).
entry('Acht-Uhr-Zug', [a, x, t, 'u:', '6', ts, 'u:', k]).
entry(achtunddreißig, [a, x, t, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(achtunddreißigsten, [a, x, t, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(achtunddreißigste, [a, x, t, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @]).
entry(achtundfünfzig, [a, x, t, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(achtundneun, [a, x, t, 'U', n, t, n, 'OY', n]).
entry(achtundneunzig, [a, x, t, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(achtund, [a, x, t, 'U', n, t]).
entry(achtundsechzig, [a, x, t, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(achtundsiebzig, [a, x, t, 'U', n, t, z, 'i:', p, ts, 'I', 'C']).
entry(achtundvierzig, [a, x, t, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(achtundzwan, [a, x, t, 'U', n, t, ts, v, a, n]).
entry(achtundzwanzig, [a, x, t, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(achtundzwanzigstem, [a, x, t, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, m]).
entry(achtundzwanzigsten, [a, x, t, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(achtundzwanzigste, [a, x, t, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(achtundzwanzigster, [a, x, t, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(achtzehn, [a, x, t, ts, 'e:', n]).
entry(achtzehntem, [a, x, ts, 'e:', n, t, @, m]).
entry(achtzehnten, [a, x, ts, 'e:', n, t, @, n]).
entry(achtzehnte, [a, x, ts, 'e:', n, t, @]).
entry(achtzehnter, [a, x, ts, 'e:', n, t, '6']).
entry(achtzig, [a, x, ts, 'I', 'C']).
entry('Action', ['E', k, 'S', @, n]).
entry('Adalbert', ['a:', d, a, l, b, 'E', '6', t]).
entry(adelig, ['a:', d, @, l, 'I', 'C']).
entry(ade, [a, d, 'e:']).
entry('Adolmy', [a, d, 'O', l, m, i]).
entry('Ado', [a, d, 'O']).
entry(ad, [a, t]).
entry('Adressen', [a, d, r, 'E', s, @, n]).
entry('Adresse', [a, d, r, 'E', s, @]).
entry('Advent', [a, t, v, 'E', n, t]).
entry('Adventsnach', [a, t, v, 'E', n, ts, n, a, x]).
entry('Adventswochenende', [a, t, v, 'E', n, ts, v, 'O', x, @, n, 'E', n, d, @]).
entry('Adventszeit', [a, t, v, 'E', n, ts, ts, aI, t]).
entry('Adventure-Tours', [@, d, v, 'E', n, tS, '6', t, 'U', r, z]).
entry(affeetrinken, [a, f, e, t, r, 'I', 'N', k, @, n]).
entry(af, [a, f]).
entry(age, ['a:', g, @]).
entry(ags, ['a:', k, s]).
entry(aha, [a, h, a]).
entry(ahnen, ['a:', n, @, n]).
entry(ahne, ['a:', n, @]).
entry('Ähnlichem', ['E:', n, l, 'I', 'C', @, m]).
entry(ähnlichen, ['E:', n, l, 'I', 'C', @, n]).
entry(ähnliche, ['E:', n, l, 'I', 'C', @]).
entry(ähnliches, ['E:', n, l, 'I', 'C', @, s]).
entry(ähnlich, ['E:', n, l, 'I', 'C']).
entry('Ahnung', ['a:', n, 'U', 'N']).
entry(ah, ['a:']).
entry(ahrt, ['a:', r, t]).
entry('Aigner', [aI, g, n, '6']).
entry('Aktionärsversammlung', [a, k, ts, j, o, n, 'E:', '6', s, f, 'E', '6', s, a, m, l, 'U', 'N']).
entry('Aktion', [a, k, ts, j, 'o:', n]).
entry('Aktivitäten', [a, k, t, 'I', v, 'I', t, 'E:', t, @, n]).
entry('Aktivität', [a, k, t, 'I', v, 'I', t, 'E:', t]).
entry(aktualisieren, [a, k, t, u, a, l, i, z, 'i:', r, @, n]).
entry(aktuellem, [a, k, t, u, 'E', l, @, m]).
entry(aktuellen, [a, k, t, u, 'E', l, @, n]).
entry(aktuell, [a, k, t, u, 'E', l]).
entry(akut, [a, k, 'u:', t]).
entry(akzeptabel, [a, k, ts, 'E', p, t, 'a:', b, @, l]).
entry('Akzeptanz', [a, k, ts, 'E', p, t, a, n, ts]).
entry(akzeptieren, [a, k, ts, 'E', p, t, 'i:', r, @, n]).
entry(albern, [a, l, b, '6', n]).
entry(alder, [a, l, d, '6']).
entry('Algorithmen-Technik-Praktikum', [a, l, g, o, r, 'I', t, m, @, n, t, 'E', 'C', n, 'I', k, p, r, a, k, t, 'I', k, 'U', m]).
entry('Algorithmen-Technik', [a, l, g, o, r, 'I', t, m, @, n, t, 'E', 'C', n, 'I', k]).
entry('Algorithmus', [a, l, g, o, r, 'I', t, m, 'U', s]).
entry('Alkohol', [a, l, k, o, h, 'o:', l]).
entry('Allalouf', [a, l, a, l, 'u:', f]).
entry(alldieweil, [a, l, d, 'i:', v, aI, l]).
entry(alleine, [a, l, aI, n, @]).
entry(allein, [a, l, aI, n]).
entry(allem, [a, l, @, m]).
entry(allenfalls, [a, l, @, n, f, a, l, s]).
entry(allen, [a, l, @, n]).
entry(alle, [a, l, @]).
entry('Allerbeste', [a, l, '6', b, 'E', s, t, @]).
entry(allerdings, [a, l, '6', d, 'I', 'N', s]).
entry(allerersten, [a, l, '6', 'e:', '6', s, t, @, n]).
entry(allerfrühestens, [a, l, '6', f, r, 'y:', @, s, t, @, n, s]).
entry(allergelegensten, [a, l, '6', g, @, l, 'e:', g, @, n, s, t, @, n]).
entry(allergisch, [a, l, 'E', '6', g, 'I', 'S']).
entry(allergünstigsten, [a, l, '6', g, 'Y', n, s, t, 'I', 'C', s, t, @, n]).
entry('Allerheiligen', [a, l, '6', h, aI, l, 'I', g, @, n]).
entry(allerletzte, [a, l, '6', l, 'E', ts, t, @]).
entry(allerliebsten, [a, l, '6', l, 'i:', p, s, t, @, n]).
entry('Allerliebste', [a, l, '6', l, 'i:', p, s, t, @]).
entry(allerneueste, [a, l, '6', n, 'OY', @, s, t, @]).
entry(aller, [a, l, '6']).
entry(allerspätestens, [a, l, '6', 'S', p, 'E:', t, @, s, t, @, n, s]).
entry('Allerwichtigste', [a, l, '6', v, 'I', 'C', t, 'I', 'C', s, t, @]).
entry(alles, [a, l, @, s]).
entry(allgemeinen, [a, l, g, @, m, aI, n, @, n]).
entry(allgemeine, [a, l, g, @, m, aI, n, @]).
entry(allgemein, [a, l, g, @, m, aI, n]).
entry('Allison', ['E', l, 'I', s, @, n]).
entry(all, [a, l]).
entry(ällt, ['E', l, t]).
entry(allzulange, [a, l, ts, u, l, a, 'N', @]).
entry(allzu, [a, l, ts, u]).
entry(allzuviel, [a, l, ts, 'U', f, 'i:', l]).
entry(allzuweit, [a, l, ts, u, v, aI, t]).
entry('Alphabet', [a, l, f, a, b, 'e:', t]).
entry('Alpha-Tirol', [a, l, f, a, t, i, r, 'o:', l]).
entry(also, [a, l, z, o]).
entry(als, [a, l, s]).
entry('Altenholz', [a, l, t, @, n, h, 'O', l, ts]).
entry(alten, [a, l, t, @, n]).
entry(alte, [a, l, t, @]).
entry(alternativen, [a, l, t, 'E', '6', n, a, t, 'i:', v, @, n]).
entry('Alternative', [a, l, t, 'E', '6', n, a, t, 'i:', v, @]).
entry(alternativ, [a, l, t, 'E', '6', n, a, t, 'i:', f]).
entry('Alternativtermin', [a, l, t, 'E', '6', n, a, t, 'i:', f, t, 'E', '6', m, 'i:', n]).
entry('Alternativvorschlag', [a, l, t, 'E', '6', n, a, t, 'i:', f, f, 'o:', '6', 'S', l, 'a:', k]).
entry(alternierend, [a, l, t, 'E', '6', n, 'i:', r, @, n, t]).
entry('Altheimer', [a, l, t, h, aI, m, '6']).
entry('Altmeier', [a, l, t, m, aI, '6']).
entry('Altstadt', [a, l, tS, t, a, t]).
entry('Amber', ['E', m, b, '6']).
entry('Ambiente', [a, m, b, j, 'E', n, t, @]).
entry('American-Express', ['E', m, e, r, i, k, @, n, 'I', k, s, p, r, e, s]).
entry(amerikanische, [a, m, e, r, i, k, 'a:', n, 'I', 'S', @]).
entry('Amerika', [a, m, 'e:', r, i, k, a]).
entry('Am-Funkturm', [a, m, f, 'U', 'N', k, t, 'U', '6', m]).
entry('Am-Georgengarten', [a, m, g, e, 'O', '6', g, @, n, g, a, r, t, @, n]).
entry('Am-Georgen', [a, m, g, @, 'O', '6', g, @, n]).
entry('Am-Markt', [a, m, m, a, r, k, t]).
entry(am, [a, m]).
entry('Am-Rathaus', [a, m, r, 'a:', t, h, aU, s]).
entry('Am-Stadtgarten', [a, m, 'S', t, a, t, g, a, r, t, @, n]).
entry('Am-Stadtpark', [a, m, 'S', t, a, t, p, a, r, k]).
entry('Amsterdam', [a, m, s, t, '6', d, a, m]).
entry('Amt', [a, m, t]).
entry('Am-Turm', [a, m, t, 'U', '6', m]).
entry(amüsieren, [a, m, y, z, 'i:', r, @, n]).
entry(anand, [a, n, a, n, t]).
entry(anbelangt, [a, n, b, @, l, a, 'N', t]).
entry(anberaumen, [a, n, b, @, r, aU, m, @, n]).
entry('Anbetracht', [a, n, b, @, t, r, a, x, t]).
entry(anbetrifft, [a, n, b, @, t, r, 'I', f, t]).
entry(anbieten, [a, n, b, 'i:', t, @, n]).
entry(anbietet, [a, n, b, 'i:', t, @, t]).
entry(anbiet, [a, n, b, 'i:', t]).
entry('Anbindung', [a, n, b, 'I', n, d, 'U', 'N']).
entry(andauernd, [a, n, d, aU, '6', n, t]).
entry(andauern, [a, n, d, aU, '6', n]).
entry(anderem, [a, n, d, @, r, @, m]).
entry(anderen, [a, n, d, @, r, @, n]).
entry(andere, [a, n, d, @, r, @]).
entry(anderer, [a, n, d, @, r, '6']).
entry(andererseits, [a, n, d, @, r, '6', z, aI, ts]).
entry(anderes, [a, n, d, @, r, @, s]).
entry('Andermahr', [a, n, d, '6', m, 'a:', r]).
entry(andermal, [a, n, d, '6', m, 'a:', l]).
entry('An-der-Marktkirche', [a, n, d, 'e:', '6', m, a, r, k, t, k, 'I', '6', 'C', @]).
entry(andern, [a, n, d, '6', n]).
entry(ändern, ['E', n, d, '6', n]).
entry(andersherum, [a, n, d, '6', s, h, 'E', r, 'U', m]).
entry(anders, [a, n, d, '6', s]).
entry(andersrum, [a, n, d, '6', s, r, 'U', m]).
entry(anderthalb, [a, n, d, '6', t, h, a, l, p]).
entry(anderthalbstündigen, [a, n, d, '6', t, h, a, l, p, 'S', t, 'Y', n, d, 'I', g, @, n]).
entry(anderthalbtägigen, [a, n, d, '6', t, h, a, l, p, t, 'E:', g, 'I', g, @, n]).
entry(anderthalbtägige, [a, n, d, '6', t, h, a, l, p, t, 'E:', g, 'I', g, @]).
entry(anderthalbtägiges, [a, n, d, '6', t, h, a, l, p, t, 'E:', g, 'I', g, @, s]).
entry(anderthalbtägig, [a, n, d, '6', t, h, a, l, p, t, 'E:', g, 'I', 'C']).
entry(ändert, ['E', n, d, '6', t]).
entry('Änderungen', ['E', n, d, @, r, 'U', 'N', @, n]).
entry('Änderung', ['E', n, d, @, r, 'U', 'N']).
entry(anderweitigen, [a, n, d, '6', v, aI, t, 'I', g, @, n]).
entry(anderweitige, [a, n, d, '6', v, aI, t, 'I', g, @]).
entry(anderweitig, [a, n, d, '6', v, aI, t, 'I', 'C']).
entry('Andjelko', [a, n, d, j, 'E', l, k, o]).
entry(and, ['E', n, d]).
entry('Andrea', [a, n, d, r, 'e:', a]).
entry('Andreas', [a, n, d, r, 'e:', a, s]).
entry(andren, [a, n, d, r, @, n]).
entry(andre, [a, n, d, r, @]).
entry('Andre', [a, n, d, r, 'e:']).
entry(andrer, [a, n, d, r, '6']).
entry(andres, [a, n, d, r, @, s]).
entry('Andrew', ['E', n, d, r, 'u:']).
entry(aneinanderfolgenden, [a, n, aI, n, a, n, d, '6', f, 'O', l, g, @, n, d, @, n]).
entry(aneinanderfolgende, [a, n, aI, n, a, n, d, '6', f, 'O', l, g, @, n, d, @]).
entry(aneinanderhängende, [a, n, aI, n, a, n, d, '6', h, 'E', 'N', @, n, d, @]).
entry(aneinanderhängen, [a, n, aI, n, a, n, d, '6', h, 'E', 'N', @, n]).
entry(aneinanderliegt, [a, n, aI, n, a, n, d, '6', l, 'i:', k, t]).
entry(aneinander, [a, n, aI, n, a, n, d, '6']).
entry(anfahren, [a, n, f, 'a:', r, @, n]).
entry(anfahre, [a, n, f, 'a:', r, @]).
entry('Anfahrt', [a, n, f, 'a:', r, t]).
entry('Anfahrtszeit', [a, n, f, 'a:', r, ts, ts, aI, t]).
entry(anfallen, [a, n, f, a, l, @, n]).
entry(anfällt, [a, n, f, 'E', l, t]).
entry(anfangen, [a, n, f, a, 'N', @, n]).
entry(anfange, [a, n, f, a, 'N', @]).
entry('Anfang', [a, n, f, a, 'N']).
entry(anfangs, [a, n, f, a, 'N', s]).
entry('Anfangstermine', [a, n, f, a, 'N', s, t, 'E', '6', m, 'i:', n, @]).
entry(anfängt, [a, n, f, 'E', 'N', t]).
entry('Anfa', [a, n, f, a]).
entry(anfingen, [a, n, f, 'I', 'N', @, n]).
entry(anfliegen, [a, n, f, l, 'i:', g, @, n]).
entry('Anf', [a, n, f]).
entry(anfragen, [a, n, f, r, 'a:', g, @, n]).
entry('Anfrage', [a, n, f, r, 'a:', g, @]).
entry(anfreunden, [a, n, f, r, 'OY', n, d, @, n]).
entry(anführten, [a, n, f, 'y:', '6', t, @, n]).
entry('Angaben', [a, n, g, 'a:', b, @, n]).
entry('Angabe', [a, n, g, 'a:', b, @]).
entry(angeben, [a, n, g, 'e:', b, @, n]).
entry(angeboten, [a, n, g, @, b, 'o:', t, @, n]).
entry(angebote, [a, n, g, @, b, 'o:', t, @]).
entry('Angebot', [a, n, g, @, b, 'o:', t]).
entry(angebrachter, [a, n, g, @, b, r, a, x, t, '6']).
entry(angebracht, [a, n, g, @, b, r, a, x, t]).
entry(angebunden, [a, n, g, @, b, 'U', n, d, @, n]).
entry(angedeutet, [a, n, g, @, d, 'OY', t, @, t]).
entry(angefangen, [a, n, g, @, f, a, 'N', @, n]).
entry('Angeforderte', [a, n, g, @, f, 'O', '6', d, '6', t, @]).
entry(angefordert, [a, n, g, @, f, 'O', '6', d, '6', t]).
entry(angefragten, [a, n, g, @, f, r, 'a:', k, t, @, n]).
entry(angefüllt, [a, n, g, @, f, 'Y', l, t]).
entry(angegebenen, [a, n, g, @, g, 'e:', b, @, n, @, n]).
entry(angegeben, [a, n, g, @, g, 'e:', b, @, n]).
entry(angehalten, [a, n, g, @, h, a, l, t, @, n]).
entry(angehen, [a, n, g, 'e:', @, n]).
entry(angeht, [a, n, g, 'e:', t]).
entry(angekommen, [a, n, g, @, k, 'O', m, @, n]).
entry(angekreuzt, [a, n, g, @, k, r, 'OY', ts, t]).
entry(angekündigte, [a, n, g, @, k, 'Y', n, d, 'I', 'C', t, @]).
entry(angekündigt, [a, n, g, @, k, 'Y', n, d, 'I', 'C', t]).
entry('Angelegenheiten', [a, n, g, @, l, 'e:', g, @, n, h, aI, t, @, n]).
entry('Angelegenheit', [a, n, g, @, l, 'e:', g, @, n, h, aI, t]).
entry('Angeln', [a, 'N', @, l, n]).
entry(angemessenen, [a, n, g, @, m, 'E', s, @, n, @, n]).
entry(angemessene, [a, n, g, @, m, 'E', s, @, n, @]).
entry(angemessener, [a, n, g, @, m, 'E', s, @, n, '6']).
entry(angemessen, [a, n, g, @, m, 'E', s, @, n]).
entry(angenehmen, [a, n, g, @, n, 'e:', m]).
entry('Angenehmen', [a, n, g, @, n, 'e:', m, @, n]).
entry(angenehme, [a, n, g, @, n, 'e:', m, @]).
entry(angenehmere, [a, n, g, @, n, 'e:', m, @, r, @]).
entry(angenehmer, [a, n, g, @, n, 'e:', m, '6']).
entry(angenehmes, [a, n, g, @, n, 'e:', m, @, s]).
entry(angenehmsten, [a, n, g, @, n, 'e:', m, s, t, @, n]).
entry(angenehmste, [a, n, g, @, n, 'e:', m, s, t, @]).
entry(angenommen, [a, n, g, @, n, 'O', m, @, n]).
entry(ange, [a, 'N', @]).
entry(angerannt, [a, n, g, @, r, a, n, t]).
entry(angerufen, [a, n, g, @, r, 'u:', f, @, n]).
entry(angesagt, [a, n, g, @, z, 'a:', k, t]).
entry(angesammelt, [a, n, g, @, z, a, m, @, l, t]).
entry(angeschaut, [a, n, g, @, 'S', aU, t]).
entry(angeschlossen, [a, n, g, @, 'S', l, 'O', s, @, n]).
entry(angesetzt, [a, n, g, @, z, 'E', ts, t]).
entry('Angesicht', [a, n, g, @, z, 'I', 'C', t]).
entry(angesichts, [a, n, g, @, z, 'I', 'C', ts]).
entry(angesiedelt, [a, n, g, @, z, 'i:', d, @, l, t]).
entry(angesprochen, [a, n, g, @, 'S', p, r, 'O', x, @, n]).
entry('Angestellten', [a, n, g, @, 'S', t, 'E', l, t, @, n]).
entry(angestrichen, [a, n, g, @, 'S', t, r, 'I', 'C', @, n]).
entry(angetastet, [a, n, g, @, t, a, s, t, @, t]).
entry(angetreten, [a, n, g, @, t, r, 'e:', t, @, n]).
entry(angewandten, [a, n, g, @, v, a, n, t, @, n]).
entry(angewandte, [a, n, g, @, v, a, n, t, @]).
entry(angewiesen, [a, n, g, @, v, 'i:', z, @, n]).
entry(anging, [a, n, g, 'I', 'N']).
entry(ang, [a, 'N']).
entry('Ang', [a, n, g]).
entry('Angriff', [a, n, g, r, 'I', f]).
entry(angst, [a, 'N', s, t]).
entry(angucken, [a, n, g, 'U', k, @, n]).
entry(angun, [a, n, g, 'U', n]).
entry(anhand, [a, n, h, a, n, t]).
entry(anhängen, [a, n, h, 'E', 'N', @, n]).
entry(anhören, [a, n, h, '2:', r, @, n]).
entry(anhört, [a, n, h, '2:', '6', t]).
entry('Animationsprogramm', [a, n, i, m, a, ts, j, 'o:', n, s, p, r, o, g, r, a, m]).
entry(anklingen, [a, n, k, l, 'I', 'N', @, n]).
entry(ankommen, [a, n, k, 'O', m, @, n]).
entry(ankomme, [a, n, k, 'O', m, @]).
entry(ankommt, [a, n, k, 'O', m, t]).
entry(ankucken, [a, n, k, 'U', k, @, n]).
entry('Ankunft', [a, n, k, 'U', n, f, t]).
entry('Ankunftszeit', [a, n, k, 'U', n, f, ts, ts, aI, t]).
entry('Ankunftsz', [a, n, k, 'U', n, f, ts, ts]).
entry('Anlaß', [a, n, l, a, s]).
entry(anläuft, [a, n, l, 'OY', f, t]).
entry('Anlaufzeit', [a, n, l, aU, f, ts, aI, t]).
entry(anlernen, [a, n, l, 'E', '6', n, @, n]).
entry('Anliegen', [a, n, l, 'i:', g, @, n]).
entry(anliegt, [a, n, l, 'i:', k, t]).
entry(anmelden, [a, n, m, 'E', l, d, @, n]).
entry(anmerken, [a, n, m, 'E', '6', k, @, n]).
entry('Annahme', [a, n, n, 'a:', m, @]).
entry('Anna', [a, n, a]).
entry(annehmbar, [a, n, n, 'e:', m, b, 'a:', r]).
entry(annehmen, [a, n, n, 'e:', m, @, n]).
entry('Anne', ['E', n]).
entry(ann, [a, n]).
entry(anpacken, [a, n, p, a, k, @, n]).
entry(anpassen, [a, n, p, a, s, @, n]).
entry(anpeilen, [a, n, p, aI, l, @, n]).
entry('Anraten', [a, n, r, 'a:', t, @, n]).
entry(anrechnen, [a, n, r, 'E', 'C', n, @, n]).
entry(anregen, [a, n, r, 'e:', g, @, n]).
entry('Anreisemodalitäten', [a, n, r, aI, z, @, m, o, d, a, l, i, t, 'E:', t, @, n]).
entry('Anreisemöglichkeit', [a, n, r, aI, z, @, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry(anreisen, [a, n, r, aI, z, @, n]).
entry('Anreise', [a, n, r, aI, z, @]).
entry('Anreisetag', [a, n, r, aI, z, @, t, 'a:', k]).
entry('Anreiseweg', [a, n, r, aI, z, @, v, 'e:', k]).
entry('Anreisezeiten', [a, n, r, aI, z, @, ts, aI, t, @, n]).
entry('Anreisezeit', [a, n, r, aI, z, @, ts, aI, t]).
entry(anreist, [a, n, r, aI, s, t]).
entry(anr, [a, n, r]).
entry('Anrufbeantworter', [a, n, r, 'u:', f, b, @, a, n, t, v, 'O', '6', t, '6']).
entry(anrufen, [a, n, r, 'u:', f, @, n]).
entry(anrufe, [a, n, r, 'u:', f, @]).
entry('Anruf', [a, n, r, 'u:', f]).
entry(anschauen, [a, n, 'S', aU, @, n]).
entry(anschaue, [a, n, 'S', aU, @]).
entry(anschaut, [a, n, 'S', aU, t]).
entry(anscheinend, [a, n, 'S', aI, n, @, n, t]).
entry('Anschein', [a, n, 'S', aI, n]).
entry(anschließenden, [a, n, 'S', l, 'i:', s, @, n, d, @, n]).
entry(anschließende, [a, n, 'S', l, 'i:', s, @, n, d, @]).
entry(anschließend, [a, n, 'S', l, 'i:', s, @, n, t]).
entry(anschließen, [a, n, 'S', l, 'i:', s, @, n]).
entry('Anschlußflug', [a, n, 'S', l, 'U', s, f, l, 'u:', k]).
entry('Anschlußmöglichkeiten', [a, n, 'S', l, 'U', s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Anschluß', [a, n, 'S', l, 'U', s]).
entry('Anschlußtermine', [a, n, 'S', l, 'U', s, t, 'E', '6', m, 'i:', n, @]).
entry('Anschlußzug', [a, n, 'S', l, 'U', s, ts, 'u:', k]).
entry(ansch, [a, n, 'S']).
entry('Anschriften', [a, n, 'S', r, 'I', f, t, @, n]).
entry(ansehen, [a, n, z, 'e:', @, n]).
entry(ansehe, [a, n, z, 'e:', @]).
entry(ansetzen, [a, n, z, 'E', ts, @, n]).
entry('Ansicht', [a, n, z, 'I', 'C', t]).
entry(ansieht, [a, n, z, 'i:', t]).
entry('Anslik', [a, n, s, l, 'I', k]).
entry(anson, [a, n, z, 'O', n]).
entry(ansonsten, [a, n, z, 'O', n, s, t, @, n]).
entry(ansonst, [a, n, z, 'O', n, s, t]).
entry(ansprachen, [a, n, 'S', p, r, 'a:', x, @, n]).
entry(ansprechendes, [a, n, 'S', p, r, 'E', 'C', @, n, d, @, s]).
entry(ansprechen, [a, n, 'S', p, r, 'E', 'C', @, n]).
entry(anspreche, [a, n, 'S', p, r, 'E', 'C', @]).
entry(ansprichst, [a, n, 'S', p, r, 'I', 'C', s, t]).
entry('Ansprüchen', [a, n, 'S', p, r, 'Y', 'C', @, n]).
entry('Ansprüche', [a, n, 'S', p, r, 'Y', 'C', @]).
entry('Anspruch', [a, n, 'S', p, r, 'U', x]).
entry(anspruchsvoll, [a, n, 'S', p, r, 'U', x, s, f, 'O', l]).
entry(ans, [a, n, s]).
entry(anstaltung, [a, n, 'S', t, a, l, t, 'U', 'N']).
entry(anstatt, [a, n, 'S', t, a, t]).
entry(anstehenden, [a, n, 'S', t, 'e:', @, n, d, @, n]).
entry(anstehende, [a, n, 'S', t, 'e:', @, n, d, @]).
entry(anstehen, [a, n, 'S', t, 'e:', @, n]).
entry(ansteh, [a, n, 'S', t, 'e:']).
entry(ansteht, [a, n, 'S', t, 'e:', t]).
entry(anstelle, [a, n, 'S', t, 'E', l, @]).
entry(anstreben, [a, n, 'S', t, r, 'e:', b, @, n]).
entry(anstrengenden, [a, n, 'S', t, r, 'E', 'N', @, n, d, @, n]).
entry(anstrengende, [a, n, 'S', t, r, 'E', 'N', @, n, d, @]).
entry(anstrengender, [a, n, 'S', t, r, 'E', 'N', @, n, d, '6']).
entry(anstrengend, [a, n, 'S', t, r, 'E', 'N', @, n, t]).
entry(anstrengen, [a, n, 'S', t, r, 'E', 'N', @, n]).
entry(antanzen, [a, n, t, a, n, ts, @, n]).
entry('Anton', [a, n, t, 'o:', n]).
entry(ant, [a, n, t]).
entry('Antrag', [a, n, t, r, 'a:', k]).
entry(antreffen, [a, n, t, r, 'E', f, @, n]).
entry(antreffe, [a, n, t, r, 'E', f, @]).
entry(antreten, [a, n, t, r, 'e:', t, @, n]).
entry('Antritt', [a, n, t, r, 'I', t]).
entry(antun, [a, n, t, 'u:', n]).
entry(antworten, [a, n, t, v, 'O', '6', t, @, n]).
entry(anvisieren, [a, n, v, i, z, 'i:', r, @, n]).
entry(anvisiert, [a, n, v, i, z, 'i:', '6', t]).
entry(anweisen, [a, n, v, aI, z, @, n]).
entry('Anweisungen', [a, n, v, aI, z, 'U', 'N', @, n]).
entry('Anweisung', [a, n, v, aI, z, 'U', 'N']).
entry(anwesend, [a, n, v, 'e:', z, @, n, t]).
entry('Anwesenheit', [a, n, v, 'e:', z, @, n, h, aI, t]).
entry(anzahlen, [a, n, ts, 'a:', l, @, n]).
entry('Anzahlung', [a, n, ts, 'a:', l, 'U', 'N']).
entry(anzigsten, [a, n, ts, 'I', 'C', s, t, @, n]).
entry(anzuberaumen, [a, n, ts, u, b, @, r, aU, m, @, n]).
entry(anzubieten, [a, n, ts, u, b, 'i:', t, @, n]).
entry(anzufangen, [a, n, ts, u, f, a, 'N', @, n]).
entry(anzugehen, [a, n, ts, u, g, 'e:', @, n]).
entry(anzugucken, [a, n, ts, u, g, 'U', k, @, n]).
entry(anzukommen, [a, n, ts, u, k, 'O', m, @, n]).
entry(anzukündigen, [a, n, ts, u, k, 'Y', n, d, 'I', g, @, n]).
entry('Anzu', [a, n, ts, 'u:']).
entry(anzureisen, [a, n, ts, u, r, aI, z, @, n]).
entry(anzurufen, [a, n, ts, u, r, 'u:', f, @, n]).
entry(anzusehen, [a, n, ts, u, z, 'e:', @, n]).
entry(anzusetzen, [a, n, ts, u, z, 'E', ts, @, n]).
entry(anzutreten, [a, n, ts, u, t, r, 'e:', t, @, n]).
entry('Aperitif', [a, p, e, r, i, t, 'i:', f]).
entry('Aperit', [a, p, e, r, i, t]).
entry('Apfel', [a, pf, @, l]).
entry('Appara', [a, p, a, r, 'a:']).
entry('Apparat', [a, p, a, r, 'a:', t]).
entry('Appetit', [a, p, e, t, 'i:', t]).
entry('Aprilhälfte', [a, p, r, 'I', l, h, 'E', l, f, t, @]).
entry('April', [a, p, r, 'I', l]).
entry('Aprilscherz', [a, p, r, 'I', l, 'S', 'E', '6', ts]).
entry('Aprils', [a, p, r, 'I', l, s]).
entry('Apriltage', [a, p, r, 'I', l, t, 'a:', g, @]).
entry('Aprilwoche', [a, p, r, 'I', l, v, 'O', x, @]).
entry('Apri', [a, p, r, 'I']).
entry(apropos, [a, p, r, o, p, 'o:']).
entry('Apr', [a, p, r]).
entry(ä, ['E:']).
entry('Arbak', [a, r, b, a, k]).
entry('Arbei', [a, r, b, aI]).
entry(arbeiten, [a, r, b, aI, t, @, n]).
entry(arbeite, [a, r, b, aI, t, @]).
entry('Arbeitervolk', [a, r, b, aI, t, '6', f, 'O', l, k]).
entry(arbeitet, [a, r, b, aI, t, @, t]).
entry('Arbeitgeber', [a, r, b, aI, t, g, 'e:', b, '6']).
entry('Arbeitnehmer', [a, r, b, aI, t, n, 'e:', m, '6']).
entry('Arbeit', [a, r, b, aI, t]).
entry('Arbeitsaufwand', [a, r, b, aI, ts, aU, f, v, a, n, t]).
entry(arbeitsbesessen, [a, r, b, aI, ts, b, @, z, 'E', s, @, n]).
entry('Arbeitsbesprechung', [a, r, b, aI, ts, b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Arbeitsbesuch', [a, r, b, aI, ts, b, @, z, 'u:', x]).
entry('Arbeitsblatt', [a, r, b, aI, ts, b, l, a, t]).
entry('Arbeitsessen', [a, r, b, aI, ts, 'E', s, @, n]).
entry('Arbeitsfrühstück', [a, r, b, aI, ts, f, r, 'y:', 'S', t, 'Y', k]).
entry('Arbeitsfrühstücks', [a, r, b, aI, ts, f, r, 'y:', 'S', t, 'Y', k, s]).
entry('Arbeitsgemeinschaft', [a, r, b, aI, ts, g, @, m, aI, n, 'S', a, f, t]).
entry('Arbeitsgespräch', [a, r, b, aI, ts, g, @, 'S', p, r, 'E:', 'C']).
entry('Arbeitskollegen', [a, r, b, aI, ts, k, 'O', l, 'e:', g, @, n]).
entry('Arbeitskraft', [a, r, b, aI, ts, k, r, a, f, t]).
entry('Arbeitskreis', [a, r, b, aI, ts, k, r, aI, s]).
entry(arbeitsmäßig, [a, r, b, aI, ts, m, 'E:', s, 'I', 'C']).
entry('Arbeitsmittagessen', [a, r, b, aI, ts, m, 'I', t, 'a:', k, 'E', s, @, n]).
entry('Arbeitsphasen', [a, r, b, aI, ts, f, 'a:', z, @, n]).
entry('Arbeitsplan', [a, r, b, aI, ts, p, l, 'a:', n]).
entry('Arbeits', [a, r, b, aI, ts]).
entry('Arbeitsräumen', [a, r, b, aI, ts, r, 'OY', m, @, n]).
entry(arbeitsreichen, [a, r, b, aI, ts, r, aI, 'C', @, n]).
entry('Arbeitsreise', [a, r, b, aI, ts, r, aI, z, @]).
entry('Arbeitsseminar', [a, r, b, aI, ts, z, 'E', m, 'I', n, a, r]).
entry('Arbeitssitzungen', [a, r, b, aI, ts, z, 'I', ts, 'U', 'N', @, n]).
entry('Arbeitssitzung', [a, r, b, aI, ts, z, 'I', ts, 'U', 'N']).
entry('Arbeitssitzu', [a, r, b, aI, ts, z, 'I', ts, 'U']).
entry('Arbeitstagen', [a, r, b, aI, ts, t, 'a:', g, @, n]).
entry('Arbeitstage', [a, r, b, aI, ts, t, 'a:', g, @]).
entry('Arbeitstag', [a, r, b, aI, ts, t, 'a:', k]).
entry('Arbeitstreffen', [a, r, b, aI, ts, t, r, 'E', f, @, n]).
entry('Arbeitstreffens', [a, r, b, aI, ts, t, r, 'E', f, @, n, s]).
entry('Arbeitstreffen-Termin', [a, r, b, aI, ts, t, r, 'E', f, @, n, t, 'E', '6', m, 'i:', n]).
entry('Arbeitstre', [a, r, b, aI, ts, t, r, 'E']).
entry('Arbeitsumfang', [a, r, b, aI, ts, 'U', m, f, a, 'N']).
entry('Arbeitswoche', [a, r, b, aI, ts, v, 'O', x, @]).
entry('Arbeitszeiten', [a, r, b, aI, ts, ts, aI, t, @, n]).
entry('Arbeitszeit', [a, r, b, aI, ts, ts, aI, t]).
entry('Architekt', [a, r, 'C', i, t, 'E', k, t]).
entry(äre, ['E:', r, @]).
entry('Aretz', ['a:', r, 'E', ts]).
entry(argen, [a, r, g, @, n]).
entry(ärgerlich, ['E', '6', g, '6', l, 'I', 'C']).
entry(ärgern, ['E', '6', g, '6', n]).
entry('Ärger', ['E', '6', g, '6']).
entry(arg, [a, r, k]).
entry('Argumentation', [a, r, g, u, m, 'E', n, t, a, ts, j, 'o:', n]).
entry('Argumente', [a, r, g, u, m, 'E', n, t, @]).
entry('Argument', [a, r, g, u, m, 'E', n, t]).
entry(arme, [a, r, m, @]).
entry('Arndt', [a, r, n, t]).
entry('Arnold-Schwarzenegger-Film', [a, r, n, 'O', l, tS, v, a, r, ts, @, n, 'E', g, '6', f, 'I', l, m]).
entry('Arrangement', [a, r, 'a~', 'Z', @, m, 'a~:']).
entry('Arrangements', [a, r, 'a~', 'Z', @, m, 'a~:', s]).
entry(arrangieren, [a, r, 'a~', 'Z', 'i:', r, @, n]).
entry(arrangiert, [a, r, 'a~', 'Z', 'i:', '6', t]).
entry('Arzt', [a, r, ts, t]).
entry('Arzttermin', [a, r, ts, t, t, 'E', '6', m, 'i:', n]).
entry('Asamstraße', [a, z, a, m, s, t, r, 'a:', s, @]).
entry('Asaoka', [a, z, 'a:', k, a]).
entry('Aschermi', [a, 'S', '6', m, 'I']).
entry('Aschermittwoch', [a, 'S', '6', m, 'I', t, v, 'O', x]).
entry('Aspekte', [a, s, p, 'E', k, t, @]).
entry('Aspekt', [a, s, p, 'E', k, t]).
entry(as, [a, s]).
entry('Assistentin', [a, s, 'I', s, t, 'E', n, t, 'I', n]).
entry('Athen', [a, t, 'e:', n]).
entry('Äther', ['E:', t, '6']).
entry('Athos', ['a:', t, 'O', s]).
entry('Atlanta', [a, t, l, a, n, t, a]).
entry('Atlantic-Hotel', [a, t, l, a, n, t, 'I', k, h, o, t, 'E', l]).
entry('Atlantis', [a, t, l, a, n, t, 'I', s]).
entry('Atmosphäre', [a, t, m, o, s, f, 'E:', r, @]).
entry('Attentat', [a, t, @, n, t, 'a:', t]).
entry(attraktiver, [a, t, r, a, k, t, 'i:', v, '6']).
entry('Attraktives', [a, t, r, a, k, t, 'i:', v, @, s]).
entry(attraktiv, [a, t, r, a, k, t, 'i:', f]).
entry('Atzinger', [a, ts, 'I', 'N', '6']).
entry('Aubergine', ['o:', b, '6', 'Z', 'i:', n, @]).
entry('Auber', [aU, b, '6']).
entry(auch, [aU, x]).
entry(aufbauen, [aU, f, b, aU, @, n]).
entry(aufbereiten, [aU, f, b, @, r, aI, t, @, n]).
entry(aufbereitet, [aU, f, b, @, r, aI, t, @, t]).
entry(aufbrechen, [aU, f, b, r, 'E', 'C', @, n]).
entry(aufdrängen, [aU, f, d, r, 'E', 'N', @, n]).
entry(aufeinanderfolgende, [aU, f, aI, n, a, n, d, '6', f, 'O', l, g, @, n, d, @]).
entry(aufeinanderkommen, [aU, f, aI, n, a, n, d, '6', k, 'O', m, @, n]).
entry('Aufenthaltes', [aU, f, 'E', n, t, h, a, l, t, @, s]).
entry('Aufenthalt', [aU, f, 'E', n, t, h, a, l, t]).
entry('Aufenthaltsräumen', [aU, f, 'E', n, t, h, a, l, ts, r, 'OY', m, @, n]).
entry(auffällt, [aU, f, f, 'E', l, t]).
entry('Aufführung', [aU, f, f, 'y:', r, 'U', 'N']).
entry('Aufgaben', [aU, f, g, 'a:', b, @, n]).
entry('Aufgabe', [aU, f, g, 'a:', b, @]).
entry(aufgearbeitet, [aU, f, g, @, a, r, b, aI, t, @, t]).
entry(aufgebacken, [aU, f, g, @, b, a, k, @, n]).
entry(aufgebaut, [aU, f, g, @, b, aU, t]).
entry(aufgedreht, [aU, f, g, @, d, r, 'e:', t]).
entry(aufgedrückt, [aU, f, g, @, d, r, 'Y', k, t]).
entry(aufgefaßt, [aU, f, g, @, f, a, s, t]).
entry(aufgeführt, [aU, f, g, @, f, 'y:', '6', t]).
entry(aufgehende, [aU, f, g, 'e:', @, n, d, @]).
entry(aufgelockerter, [aU, f, g, @, l, 'O', k, '6', t, '6']).
entry(aufgepaßt, [aU, f, g, @, p, a, s, t]).
entry(aufge, [aU, f, g, @]).
entry(aufgeschlagen, [aU, f, g, @, 'S', l, 'a:', g, @, n]).
entry(aufgeschoben, [aU, f, g, @, 'S', 'o:', b, @, n]).
entry(aufgeschrieben, [aU, f, g, @, 'S', r, 'i:', b, @, n]).
entry(aufgesetzt, [aU, f, g, @, z, 'E', ts, t]).
entry(aufgetaucht, [aU, f, g, @, t, aU, x, t]).
entry(aufgezählt, [aU, f, g, @, ts, 'E:', l, t]).
entry(aufgezeichnet, [aU, f, g, @, ts, aI, 'C', n, @, t]).
entry(aufgrund, [aU, f, g, r, 'U', n, t]).
entry(aufhaben, [aU, f, h, 'a:', b, @, n]).
entry(aufhalten, [aU, f, h, a, l, t, @, n]).
entry(aufhalte, [aU, f, h, a, l, t, @]).
entry('Aufheben', [aU, f, h, 'e:', b, @, n]).
entry('Aufhebens', [aU, f, h, 'e:', b, @, n, s]).
entry(aufhören, [aU, f, h, '2:', r, @, n]).
entry(aufkommen, [aU, f, k, 'O', m, @, n]).
entry(aufkreuzen, [aU, f, k, r, 'OY', ts, @, n]).
entry('Auflösung', [aU, f, l, '2:', z, 'U', 'N']).
entry(aufmachen, [aU, f, m, a, x, @, n]).
entry(aufmerksam, [aU, f, m, 'E', '6', k, z, 'a:', m]).
entry(aufnahmebereit, [aU, f, n, 'a:', m, @, b, @, r, aI, t]).
entry(aufnehmen, [aU, f, n, 'e:', m, @, n]).
entry('Auf-Nummer-Sicher-Gehen', [aU, f, n, 'U', m, '6', z, 'I', 'C', '6', g, 'e:', @, n]).
entry(auf, [aU, f]).
entry(aufräumen, [aU, f, r, 'OY', m, @, n]).
entry(aufrecht, [aU, f, r, 'E', 'C', t]).
entry(aufregend, [aU, f, r, 'e:', g, @, n, t]).
entry(aufregt, [aU, f, r, 'e:', k, t]).
entry(aufreißen, [aU, f, r, aI, s, @, n]).
entry(aufscheuchen, [aU, f, 'S', 'OY', 'C', @, n]).
entry(aufschieben, [aU, f, 'S', 'i:', b, @, n]).
entry(aufschlage, [aU, f, 'S', l, 'a:', g, @]).
entry(aufschreiben, [aU, f, 'S', r, aI, b, @, n]).
entry(aufschreibe, [aU, f, 'S', r, aI, b, @]).
entry(aufsetzen, [aU, f, z, 'E', ts, @, n]).
entry('Aufsichtsrat', [aU, f, z, 'I', 'C', ts, r, 'a:', t]).
entry(aufs, [aU, f, s]).
entry(aufstehen, [aU, f, 'S', t, 'e:', @, n]).
entry(aufstehe, [aU, f, 'S', t, 'e:', @]).
entry(aufsteigen, [aU, f, 'S', t, aI, g, @, n]).
entry(aufsuchen, [aU, f, z, 'u:', x, @, n]).
entry(auftauchen, [aU, f, t, aU, x, @, n]).
entry(auftaucht, [aU, f, t, aU, x, t]).
entry('Auftrag', [aU, f, t, r, 'a:', k]).
entry(auftreiben, [aU, f, t, r, aI, b, @, n]).
entry(auftreten, [aU, f, t, r, 'e:', t, @, n]).
entry('Aufwand', [aU, f, v, a, n, t]).
entry('Aufwärmen', [aU, f, v, 'E', '6', m, @, n]).
entry(aufwarten, [aU, f, v, a, r, t, @, n]).
entry(aufwärts, [aU, f, v, 'E', '6', ts]).
entry(aufweisen, [aU, f, v, aI, z, @, n]).
entry(aufwendiges, [aU, f, v, 'E', n, d, 'I', g, @, s]).
entry(aufwendig, [aU, f, v, 'E', n, d, 'I', 'C']).
entry(aufzählen, [aU, f, ts, 'E:', l, @, n]).
entry(aufzuhalten, [aU, f, ts, u, h, a, l, t, @, n]).
entry(aufzustehen, [aU, f, ts, u, 'S', t, 'e:', @, n]).
entry(aufzusuchen, [aU, f, ts, u, z, 'u:', x, @, n]).
entry(aufzuzählen, [aU, f, ts, u, ts, 'E:', l, @, n]).
entry('Augenblick', [aU, g, @, n, b, l, 'I', k]).
entry('Augen', [aU, g, @, n]).
entry('Augenschein', [aU, g, @, n, 'S', aI, n]).
entry('Auge', [aU, g, @]).
entry('Aug', [aU, k]).
entry('Augsburg', [aU, k, s, b, 'U', '6', k]).
entry('Augus', [aU, g, 'U', s]).
entry('Augusthälfte', [aU, g, 'U', s, t, h, 'E', l, f, t, @]).
entry('August', [aU, g, 'U', s, t]).
entry('Augusttage', [aU, g, 'U', s, t, t, 'a:', g, @]).
entry('Augustwochen', [aU, g, 'U', s, t, v, 'O', x, @, n]).
entry('Augustwoche', [aU, g, 'U', s, t, v, 'O', x, @]).
entry('Augustw', [aU, g, 'U', s, t, v]).
entry(au, [aU]).
entry(ausarbeiten, [aU, s, a, r, b, aI, t, @, n]).
entry(auschecken, [aU, s, tS, 'E', k, @, n]).
entry(ausdehnen, [aU, s, d, 'e:', n, @, n]).
entry(ausdenken, [aU, s, d, 'E', 'N', k, @, n]).
entry(ausdiskutieren, [aU, s, d, 'I', s, k, u, t, 'i:', r, @, n]).
entry(ausdrucken, [aU, s, d, r, 'U', k, @, n]).
entry(auseinandergerissen, [aU, s, aI, n, a, n, d, '6', g, @, r, 'I', s, @, n]).
entry(auseinandergezerrt, [aU, s, aI, n, a, n, d, '6', g, @, ts, 'E', '6', t]).
entry(auseinanderlegen, [aU, s, aI, n, a, n, d, '6', l, 'e:', g, @, n]).
entry(auseinander, [aU, s, aI, n, a, n, d, '6']).
entry(auseinandersetzen, [aU, s, aI, n, a, n, d, '6', z, 'E', ts, @, n]).
entry('Auseinandersetzungen', [aU, s, aI, n, a, n, d, '6', z, 'E', ts, 'U', 'N', @, n]).
entry(auseinanderstreuen, [aU, s, aI, n, a, n, d, '6', 'S', t, r, 'OY', @, n]).
entry(auseinanderziehen, [aU, s, aI, n, a, n, d, '6', ts, 'i:', @, n]).
entry(auseinanderzusetzen, [aU, s, aI, n, a, n, d, '6', ts, u, z, 'E', ts, @, n]).
entry(auseinanderzuzerren, [aU, s, aI, n, a, n, d, '6', ts, u, ts, 'E', r, @, n]).
entry(auseinanderzuziehen, [aU, s, aI, n, a, n, d, '6', ts, u, ts, 'i:', @, n]).
entry('Außendienst', [aU, s, @, n, d, 'i:', n, s, t]).
entry(außen, [aU, s, @, n]).
entry(außenrum, [aU, s, @, n, r, 'U', m]).
entry('Außenstelle', [aU, s, @, n, 'S', t, 'E', l, @]).
entry(außerdem, [aU, s, '6', d, 'e:', m]).
entry(äußeren, ['OY', s, @, r, @, n]).
entry('Außergewöhnliches', [aU, s, '6', g, @, v, '2:', n, l, 'I', 'C', @, s]).
entry(außerhalb, [aU, s, '6', h, a, l, p]).
entry(außerordentlichen, [aU, s, '6', 'O', '6', d, @, n, t, l, 'I', 'C', @, n]).
entry(außerordentlich, [aU, s, '6', 'O', '6', d, @, n, t, l, 'I', 'C']).
entry(außer, [aU, s, '6']).
entry(äußersten, ['OY', s, '6', s, t, @, n]).
entry(äußerst, ['OY', s, '6', s, t]).
entry(ausfallen, [aU, s, f, a, l, @, n]).
entry(ausfalle, [aU, s, f, a, l, @]).
entry(ausfällt, [aU, s, f, 'E', l, t]).
entry(ausfindig, [aU, s, f, 'I', n, d, 'I', 'C']).
entry('Ausfluges', [aU, s, f, l, 'u:', g, @, s]).
entry('Ausflug', [aU, s, f, l, 'u:', k]).
entry('Ausflugs', [aU, s, f, l, 'u:', k, s]).
entry(ausformulieren, [aU, s, f, 'O', '6', m, u, l, 'i:', r, @, n]).
entry(ausführen, [aU, s, f, 'y:', r, @, n]).
entry(ausführliche, [aU, s, f, 'y:', '6', l, 'I', 'C', @]).
entry(ausführlich, [aU, s, f, 'y:', '6', l, 'I', 'C']).
entry('Ausgabe', [aU, s, g, 'a:', b, @]).
entry('Ausgang', [aU, s, g, a, 'N']).
entry('Ausgangspunkt', [aU, s, g, a, 'N', s, p, 'U', 'N', k, t]).
entry(ausgearbeitet, [aU, s, g, @, a, r, b, aI, t, @, t]).
entry(ausgebaut, [aU, s, g, @, b, aU, t]).
entry(ausgeben, [aU, s, g, 'e:', b, @, n]).
entry(ausgebucht, [aU, s, g, @, b, 'u:', x, t]).
entry(ausgebügelt, [aU, s, g, @, b, 'y:', g, @, l, t]).
entry(ausgecheckt, [aU, s, g, @, tS, 'E', k, t]).
entry(ausgedacht, [aU, s, g, @, d, a, x, t]).
entry(ausgedehnt, [aU, s, g, @, d, 'e:', n, t]).
entry(ausgedrückt, [aU, s, g, @, d, r, 'Y', k, t]).
entry(ausgefallene, [aU, s, g, @, f, a, l, @, n, @]).
entry('Ausgefalleneres', [aU, s, g, @, f, a, l, @, n, @, r, @, s]).
entry(ausgefallen, [aU, s, g, @, f, a, l, @, n]).
entry(ausgefüllt, [aU, s, g, @, f, 'Y', l, t]).
entry(ausgehend, [aU, s, g, 'e:', @, n, t]).
entry(ausgehen, [aU, s, g, 'e:', @, n]).
entry(ausgeht, [aU, s, g, 'e:', t]).
entry(ausgelastet, [aU, s, g, @, l, a, s, t, @, t]).
entry(ausgelegt, [aU, s, g, @, l, 'e:', k, t]).
entry(ausgemachten, [aU, s, g, @, m, a, x, t, @, n]).
entry(ausgemachter, [aU, s, g, @, m, a, x, t, '6']).
entry(ausgemacht, [aU, s, g, @, m, a, x, t]).
entry(ausgenommen, [aU, s, g, @, n, 'O', m, @, n]).
entry(ausgeplanten, [aU, s, g, @, p, l, 'a:', n, t, @, n]).
entry(ausgeplant, [aU, s, g, @, p, l, 'a:', n, t]).
entry(ausge, [aU, s, g, @]).
entry(ausgerechnet, [aU, s, g, @, r, 'E', 'C', n, @, t]).
entry(ausgeruht, [aU, s, g, @, r, 'u:', t]).
entry(ausgesagt, [aU, s, g, @, z, 'a:', k, t]).
entry(ausgeschaltet, [aU, s, g, @, 'S', a, l, t, @, t]).
entry(ausgeschaut, [aU, s, g, @, 'S', aU, t]).
entry(ausgeschlafen, [aU, s, g, @, 'S', l, 'a:', f, @, n]).
entry(ausgeschlossen, [aU, s, g, @, 'S', l, 'O', s, @, n]).
entry('Ausgeschwitzte', [aU, s, g, @, 'S', v, 'I', ts, t, @]).
entry(ausgestattetes, [aU, s, g, @, 'S', t, a, t, @, t, @, s]).
entry(ausgestattet, [aU, s, g, @, 'S', t, a, t, @, t]).
entry(ausgestrichen, [aU, s, g, @, 'S', t, r, 'I', 'C', @, n]).
entry(ausgesucht, [aU, s, g, @, z, 'u:', x, t]).
entry(ausgewählt, [aU, s, g, @, v, 'E:', l, t]).
entry(ausgeweitete, [aU, s, g, @, v, aI, t, @, t, @]).
entry(ausgezeichnete, [aU, s, g, @, ts, aI, 'C', n, @, t, @]).
entry(ausgezeichnet, [aU, s, g, @, ts, aI, 'C', n, @, t]).
entry(ausgibt, [aU, s, g, 'i:', p, t]).
entry(ausgiebigen, [aU, s, g, 'i:', b, 'I', g, @, n]).
entry(ausgiebiges, [aU, s, g, 'i:', b, 'I', g, @, s]).
entry(ausgucken, [aU, s, g, 'U', k, @, n]).
entry(aushalten, [aU, s, h, a, l, t, @, n]).
entry(ausharren, [aU, s, h, a, r, @, n]).
entry(auskennen, [aU, s, k, 'E', n, @, n]).
entry(ausklammern, [aU, s, k, l, a, m, '6', n]).
entry('Ausklang', [aU, s, k, l, a, 'N']).
entry(ausklingen, [aU, s, k, l, 'I', 'N', @, n]).
entry(auskommen, [aU, s, k, 'O', m, @, n]).
entry(auskucken, [aU, s, k, 'U', k, @, n]).
entry('Auskünfte', [aU, s, k, 'Y', n, f, t, @]).
entry('Auskunft', [aU, s, k, 'U', n, f, t]).
entry(ausländischen, [aU, s, l, 'E', n, d, 'I', 'S', @, n]).
entry(ausländisches, [aU, s, l, 'E', n, d, 'I', 'S', @, s]).
entry('Ausland', [aU, s, l, a, n, t]).
entry('Auslandsreise', [aU, s, l, a, n, ts, r, aI, z, @]).
entry(auslassen, [aU, s, l, a, s, @, n]).
entry(ausmachen, [aU, s, m, a, x, @, n]).
entry(ausmacht, [aU, s, m, a, x, t]).
entry('Ausmachungen', [aU, s, m, a, x, 'U', 'N', @, n]).
entry(ausm, [aU, s, m]).
entry('Ausnahme', [aU, s, n, 'a:', m, @]).
entry(ausnahmsweise, [aU, s, n, 'a:', m, s, v, aI, z, @]).
entry(ausnutzen, [aU, s, n, 'U', ts, @, n]).
entry(ausnützen, [aU, s, n, 'Y', ts, @, n]).
entry(auspacken, [aU, s, p, a, k, @, n]).
entry(ausp, [aU, s, p]).
entry(ausprobieren, [aU, s, p, r, o, b, 'i:', r, @, n]).
entry(ausprobiert, [aU, s, p, r, o, b, 'i:', '6', t]).
entry(aus, [aU, s]).
entry(ausreichend, [aU, s, r, aI, 'C', @, n, t]).
entry(ausreichen, [aU, s, r, aI, 'C', @, n]).
entry(ausreicht, [aU, s, r, aI, 'C', t]).
entry('Ausrichtung', [aU, s, r, 'I', 'C', t, 'U', 'N']).
entry(ausruhen, [aU, s, r, 'u:', @, n]).
entry('Aussagen', [aU, s, z, 'a:', g, @, n]).
entry('Aussage', [aU, s, z, 'a:', g, @]).
entry(aussah, [aU, s, z, 'a:']).
entry(aussa, [aU, s, z, a]).
entry(ausschauen, [aU, s, 'S', aU, @, n]).
entry(ausschaut, [aU, s, 'S', aU, t]).
entry(ausscheidet, [aU, s, 'S', aI, d, @, t]).
entry(ausschlafen, [aU, s, 'S', l, 'a:', f, @, n]).
entry(ausschließen, [aU, s, 'S', l, 'i:', s, @, n]).
entry(ausschließlich, [aU, s, 'S', l, 'i:', s, l, 'I', 'C']).
entry('Ausschlußtermine', [aU, s, 'S', l, 'U', s, t, 'E', '6', m, 'i:', n, @]).
entry(aussehen, [aU, s, z, 'e:', @, n]).
entry('Aussicht', [aU, s, z, 'I', 'C', t]).
entry(aussichtslos, [aU, s, z, 'I', 'C', ts, l, 'o:', s]).
entry(aussieht, [aU, s, z, 'i:', t]).
entry(ausspannen, [aU, s, 'S', p, a, n, @, n]).
entry(aussparen, [aU, s, 'S', p, 'a:', r, @, n]).
entry(aussprechen, [aU, s, 'S', p, r, 'E', 'C', @, n]).
entry(ausspreche, [aU, s, 'S', p, r, 'E', 'C', @]).
entry('Ausstattungen', [aU, s, 'S', t, a, t, 'U', 'N', @, n]).
entry('Ausstattung', [aU, s, 'S', t, a, t, 'U', 'N']).
entry(ausstehenden, [aU, s, 'S', t, 'e:', @, n, d, @, n]).
entry(ausstehen, [aU, s, 'S', t, 'e:', @, n]).
entry(aussteigen, [aU, s, 'S', t, aI, g, @, n]).
entry('Ausstellungen', [aU, s, 'S', t, 'E', l, 'U', 'N', @, n]).
entry(ausstellung, [aU, s, 'S', t, 'E', l, 'U', 'N']).
entry(aussuchen, [aU, s, z, 'u:', x, @, n]).
entry(aussuche, [aU, s, z, 'u:', x, @]).
entry(austauschen, [aU, s, t, aU, 'S', @, n]).
entry(austesten, [aU, s, t, 'E', s, t, @, n]).
entry(austoben, [aU, s, t, 'o:', b, @, n]).
entry(ausverkauft, [aU, s, f, 'E', '6', k, aU, f, t]).
entry(auswählen, [aU, s, v, 'E:', l, @, n]).
entry('Auswahl', [aU, s, v, 'a:', l]).
entry(auswärts, [aU, s, v, 'E', '6', ts]).
entry('Auswärtsspiel', [aU, s, v, 'E', '6', ts, 'S', p, 'i:', l]).
entry('Auswärtstermine', [aU, s, v, 'E', '6', ts, t, 'E', '6', m, 'i:', n, @]).
entry(ausweichen, [aU, s, v, aI, 'C', @, n]).
entry('Ausweichmöglich', [aU, s, v, aI, 'C', m, '2:', k, l, 'I', 'C']).
entry('Ausweichtag', [aU, s, v, aI, 'C', t, 'a:', k]).
entry('Ausweichtermine', [aU, s, v, aI, 'C', t, 'E', '6', m, 'i:', n, @]).
entry('Ausweichtermin', [aU, s, v, aI, 'C', t, 'E', '6', m, 'i:', n]).
entry(auswerten, [aU, s, v, 'e:', '6', t, @, n]).
entry('Auswo', [aU, s, v, 'o:']).
entry(auszudenken, [aU, s, ts, u, d, 'E', 'N', k, @, n]).
entry(auszudrucken, [aU, s, ts, u, d, r, 'U', k, @, n]).
entry(auszugeben, [aU, s, ts, u, g, 'e:', b, @, n]).
entry('Auszugsliste', [aU, s, ts, 'u:', k, s, l, 'I', s, t, @]).
entry(auszuhandeln, [aU, s, ts, u, h, a, n, d, @, l, n]).
entry(auszukennen, [aU, s, ts, u, k, 'E', n, @, n]).
entry(auszumachen, [aU, s, ts, u, m, a, x, @, n]).
entry(auszureden, [aU, s, ts, u, r, 'e:', d, @, n]).
entry(auszurichten, [aU, s, ts, u, r, 'I', 'C', t, @, n]).
entry(auszuschließen, [aU, s, ts, u, 'S', l, 'i:', s, @, n]).
entry(auszusuchen, [aU, s, ts, u, z, 'u:', x, @, n]).
entry(auszuwerten, [aU, s, ts, u, v, 'e:', '6', t, @, n]).
entry('Autobahnen', [aU, t, o, b, 'a:', n, @, n]).
entry('Autobahn', [aU, t, o, b, 'a:', n]).
entry('Autofahrerin', [aU, t, o, f, 'a:', r, @, r, 'I', n]).
entry('Autofreak', [aU, t, o, f, r, 'i:', k]).
entry('Automobilausstellung', [aU, t, o, m, o, b, 'i:', l, aU, s, 'S', t, 'E', l, 'U', 'N']).
entry('Automobil', [aU, t, o, m, o, b, 'i:', l]).
entry('Auto', [aU, t, o]).
entry('Autos', [aU, t, o, s]).
entry('Autovermietung', [aU, t, o, f, 'E', '6', m, 'i:', t, 'U', 'N']).
entry('Aversion', [a, v, 'E', '6', z, j, 'o:', n]).
entry(ay, ['E', j]).
entry(ba, [b, a]).
entry('Ba', [b, 'a:']).
entry(backe, [b, a, k, @]).
entry(backen, [b, a, k, @, n]).
entry('Bäcker', [b, 'E', k, '6']).
entry(bäckt, [b, 'E', k, t]).
entry(bad, [b, 'a:', t]).
entry('Baden-Baden', [b, 'a:', d, @, n, b, 'a:', d, @, n]).
entry(baden, [b, 'a:', d, @, n]).
entry('Baden-Württemberg', [b, 'a:', d, @, n, v, 'Y', '6', t, @, m, b, 'E', '6', k]).
entry('Badewanne', [b, 'a:', d, @, v, a, n, @]).
entry('Badezimmer', [b, 'a:', d, @, ts, 'I', m, '6']).
entry('Baggersee', [b, a, g, '6', z, 'e:']).
entry(bäh, [b, 'E:']).
entry('Bahn', [b, 'a:', n]).
entry('Bahn-Card', [b, 'a:', n, k, a, r, t]).
entry('Bahn-Erfahrungen', [b, 'a:', n, 'E', '6', f, 'a:', r, 'U', 'N', @, n]).
entry('Bahnfahrerei', [b, 'a:', n, f, 'a:', r, @, r, aI]).
entry('Bahnfahrt', [b, 'a:', n, f, 'a:', r, t]).
entry('Bahngerüttel', [b, 'a:', n, g, @, r, 'Y', t, @, l]).
entry('Bahngleis', [b, 'a:', n, g, l, aI, s]).
entry(bahnhof, [b, 'a:', n, h, 'o:', f]).
entry('Bahnhofnähe', [b, 'a:', n, h, 'o:', f, n, 'E:', @]).
entry('Bahnhofs', [b, 'a:', n, h, 'o:', f, s]).
entry('Bahnhofskiosk', [b, 'a:', n, h, 'o:', f, s, k, 'i:', 'O', s, k]).
entry('Bahnhofsnähe', [b, 'a:', n, h, 'o:', f, s, n, 'E:', @]).
entry('Bahnkarten', [b, 'a:', n, k, a, r, t, @, n]).
entry('Bahnserver', [b, 'a:', n, s, '9', '6', v, '6']).
entry('Bahnsteig', [b, 'a:', n, 'S', t, aI, k]).
entry('Bahnticket', [b, 'a:', n, t, 'I', k, @, t]).
entry('Bahntickets', [b, 'a:', n, t, 'I', k, @, ts]).
entry('Bahnverbindung', [b, 'a:', n, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('Bahnverbindungen', [b, 'a:', n, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry('Baiersdorf', [b, aI, '6', s, d, 'O', '6', f]).
entry(bal, [b, a, l]).
entry(bald, [b, a, l, t]).
entry(baldigen, [b, a, l, d, 'I', g, @, n]).
entry(baldmöglichst, [b, a, l, t, m, '2:', k, l, 'I', 'C', s, t]).
entry(ballen, [b, a, l, @, n]).
entry('Ballhaus', [b, a, l, h, aU, s]).
entry('Ballhof', [b, a, l, h, 'o:', f]).
entry('Bambas', [b, a, m, b, a, s]).
entry(bäm, [b, 'E', m]).
entry('Bamberg', [b, a, m, b, 'E', '6', k]).
entry('Bandaufnahme', [b, a, n, t, aU, f, n, 'a:', m, @]).
entry('Bände', [b, 'E', n, d, @]).
entry('Bange', [b, a, 'N', @]).
entry('Bank', [b, a, 'N', k]).
entry('Bankrott', [b, a, 'N', k, r, 'O', t]).
entry(bar, [b, 'a:', r]).
entry('Barbara', [b, a, r, b, a, r, a]).
entry('Bardenhagen', [b, a, r, d, @, n, h, 'a:', g, @, n]).
entry(baren, [b, 'a:', r, @, n]).
entry('Bargeld', [b, 'a:', r, g, 'E', l, t]).
entry('Barockgarten', [b, a, r, 'O', k, g, a, r, t, @, n]).
entry('Bars', [b, 'a:', r, s]).
entry('Bartels', [b, a, r, t, @, l, s]).
entry(barten, [b, 'a:', r, t, @, n]).
entry('Barth', [b, a, r, t]).
entry('Bartlme', [b, a, r, t, l, m, e]).
entry(baseball, [b, e, 'I', s, b, 'O', l]).
entry('Bashford', [b, 'E', 'S', f, @, d]).
entry('Bassenhorst', [b, a, s, @, n, h, 'O', '6', s, t]).
entry('Batliner', [b, a, t, l, 'i:', n, '6']).
entry('Bauch-Weg-Gürtel', [b, aU, x, v, 'E', k, g, 'Y', '6', t, @, l]).
entry(bauen, [b, aU, @, n]).
entry('Bauer', [b, aU, '6']).
entry('Baumann', [b, aU, m, a, n]).
entry('Bäume', [b, 'OY', m, @]).
entry('Bäumgen', [b, 'OY', m, g, @, n]).
entry('Bauner', [b, aU, n, '6']).
entry('Baustelle', [b, aU, 'S', t, 'E', l, @]).
entry('Baustoffkunde-Vorlesung', [b, aU, 'S', t, 'O', f, k, 'U', n, d, @, f, 'o:', '6', l, 'e:', z, 'U', 'N']).
entry(baut, [b, aU, t]).
entry('Bayer', [b, aI, '6']).
entry('Bayerischen-Hof', [b, aI, @, r, 'I', 'S', @, n, h, 'o:', f]).
entry('Bayerischen-Landesbank', [b, aI, @, r, 'I', 'S', @, n, l, a, n, d, @, s, b, a, 'N', k]).
entry('Bayerischer-Hof', [b, aI, @, r, 'I', 'S', '6', h, 'o:', f]).
entry('Bayern', [b, aI, '6', n]).
entry('Bayreuth', [b, aI, r, 'OY', t]).
entry(b, [b, 'e:']).
entry(beabsichtige, [b, @, a, p, z, 'I', 'C', t, 'I', g, @]).
entry(beabsichtigt, [b, @, a, p, z, 'I', 'C', t, 'I', 'C', t]).
entry(beachten, [b, @, a, x, t, @, n]).
entry(beanspruchen, [b, @, a, n, 'S', p, r, 'U', x, @, n]).
entry(beansprucht, [b, @, a, n, 'S', p, r, 'U', x, t]).
entry(beantragt, [b, @, a, n, t, r, 'a:', k, t]).
entry(bearbeitet, [b, @, a, r, b, aI, t, @, t]).
entry('Beate', [b, e, 'a:', t, @]).
entry(beauftrage, [b, @, aU, f, t, r, 'a:', g, @]).
entry(beauftragen, [b, @, aU, f, t, r, 'a:', g, @, n]).
entry(beauftragt, [b, @, aU, f, t, r, 'a:', k, t]).
entry(be, [b, @]).
entry('Be', [b, e]).
entry('Becher', [b, 'E', 'C', '6']).
entry(bedacht, [b, @, d, a, x, t]).
entry(bedanke, [b, @, d, a, 'N', k, @]).
entry(bedanken, [b, @, d, a, 'N', k, @, n]).
entry(bedauere, [b, @, d, aU, @, r, @]).
entry(bedauerlich, [b, @, d, aU, '6', l, 'I', 'C']).
entry(bedauerlicherweise, [b, @, d, aU, '6', l, 'I', 'C', '6', v, aI, z, @]).
entry(bedauern, [b, @, d, aU, '6', n]).
entry(bed, [b, 'E', d]).
entry(bedenken, [b, @, d, 'E', 'N', k, @, n]).
entry(bedeuten, [b, @, d, 'OY', t, @, n]).
entry(bedeutend, [b, @, d, 'OY', t, @, n, t]).
entry(bedeutendsten, [b, @, d, 'OY', t, @, n, ts, t, @, n]).
entry(bedeutet, [b, @, d, 'OY', t, @, t]).
entry('Bedienung', [b, @, d, 'i:', n, 'U', 'N']).
entry('Bedienungen', [b, @, d, 'i:', n, 'U', 'N', @, n]).
entry(beehren, [b, @, 'e:', r, @, n]).
entry(beeile, [b, @, aI, l, @]).
entry(beeilen, [b, @, aI, l, @, n]).
entry(beeintra, [b, @, aI, n, t, r, a]).
entry(beenden, [b, @, 'E', n, d, @, n]).
entry(beendet, [b, @, 'E', n, d, @, t]).
entry(beengt, [b, @, 'E', 'N', t]).
entry(befaßten, [b, @, f, a, s, t, @, n]).
entry(befinde, [b, @, f, 'I', n, d, @]).
entry(befinden, [b, @, f, 'I', n, d, @, n]).
entry(befindet, [b, @, f, 'I', n, d, @, t]).
entry(befördert, [b, @, f, '9', '6', d, '6', t]).
entry(befragen, [b, @, f, r, 'a:', g, @, n]).
entry(befreundet, [b, @, f, r, 'OY', n, d, @, t]).
entry(befriedigend, [b, @, f, r, 'i:', d, 'I', g, @, n, t]).
entry(befürchte, [b, @, f, 'Y', '6', 'C', t, @]).
entry(befürchtet, [b, @, f, 'Y', '6', 'C', t, @, t]).
entry(begeben, [b, @, g, 'e:', b, @, n]).
entry(begegnen, [b, @, g, 'e:', g, n, @, n]).
entry(begegnet, [b, @, g, 'e:', g, n, @, t]).
entry('Begegnung', [b, @, g, 'e:', g, n, 'U', 'N']).
entry('Begehr', [b, @, g, 'e:', '6']).
entry(begeistern, [b, @, g, aI, s, t, '6', n]).
entry(begeistert, [b, @, g, aI, s, t, '6', t]).
entry(begeisterte, [b, @, g, aI, s, t, '6', t, @]).
entry(begi, [b, @, g, 'I']).
entry(begießen, [b, @, g, 'i:', s, @, n]).
entry('Beginn', [b, @, g, 'I', n]).
entry(beginnen, [b, @, g, 'I', n, @, n]).
entry(beginnend, [b, @, g, 'I', n, @, n, t]).
entry(beginnenden, [b, @, g, 'I', n, @, n, d, @, n]).
entry(beginnt, [b, @, g, 'I', n, t]).
entry(begleiten, [b, @, g, l, aI, t, @, n]).
entry('Begleitungen', [b, @, g, l, aI, t, 'U', 'N', @, n]).
entry(begrenzt, [b, @, g, r, 'E', n, ts, t]).
entry(begrenzte, [b, @, g, r, 'E', n, ts, t, @]).
entry(begrenzten, [b, @, g, r, 'E', n, ts, t, @, n]).
entry('Begrenzung', [b, @, g, r, 'E', n, ts, 'U', 'N']).
entry('Begriffe', [b, @, g, r, 'I', f, @]).
entry(begriffen, [b, @, g, r, 'I', f, @, n]).
entry(begrüße, [b, @, g, r, 'y:', s, @]).
entry(begrüßen, [b, @, g, r, 'y:', s, @, n]).
entry(begrüßens, [b, @, g, r, 'y:', s, @, n, s]).
entry('Begrüßung', [b, @, g, r, 'y:', s, 'U', 'N']).
entry(begutachten, [b, @, g, 'u:', t, a, x, t, @, n]).
entry(behalte, [b, @, h, a, l, t, @]).
entry(behalten, [b, @, h, a, l, t, @, n]).
entry('Beham', [b, e, h, a, m]).
entry(behaupten, [b, @, h, aU, p, t, @, n]).
entry(behilflich, [b, @, h, 'I', l, f, l, 'I', 'C']).
entry('Behne', [b, 'e:', n, @]).
entry('Behörde', [b, @, h, '2:', '6', d, @]).
entry(bei, [b, aI]).
entry(beibehalten, [b, aI, b, @, h, a, l, t, @, n]).
entry(beide, [b, aI, d, @]).
entry(beiden, [b, aI, d, @, n]).
entry(beiderseitig, [b, aI, d, '6', z, aI, t, 'I', 'C']).
entry(beides, [b, aI, d, @, s]).
entry(beieinander, [b, aI, aI, n, a, n, d, '6']).
entry(beifügen, [b, aI, f, 'y:', g, @, n]).
entry(beilegen, [b, aI, l, 'e:', g, @, n]).
entry(beim, [b, aI, m]).
entry(beinah, [b, aI, n, 'a:']).
entry(beinahe, [b, aI, n, 'a:', @]).
entry(beinhaltet, [b, @, 'I', n, h, a, l, t, @, t]).
entry(beisammen, [b, aI, z, a, m, @, n]).
entry('Beisammensein', [b, aI, z, a, m, @, n, z, aI, n]).
entry(beiseite, [b, aI, z, aI, t, @]).
entry(beißen, [b, aI, s, @, n]).
entry('Beispiel', [b, aI, 'S', p, 'i:', l]).
entry(beispielsweise, [b, aI, 'S', p, 'i:', l, s, v, aI, z, @]).
entry(beiterin, [b, aI, t, @, r, 'I', n]).
entry(beizeiten, [b, aI, ts, aI, t, @, n]).
entry(beje, [b, @, j, e]).
entry(bekäme, [b, @, k, 'E:', m, @]).
entry(bekämpft, [b, @, k, 'E', m, pf, t]).
entry(bekannt, [b, @, k, a, n, t]).
entry('Bekannte', [b, @, k, a, n, t, @]).
entry(bekannten, [b, @, k, a, n, t, @, n]).
entry('Bekanntenkreis', [b, @, k, a, n, t, @, n, k, r, aI, s]).
entry(bekanntlich, [b, @, k, a, n, t, l, 'I', 'C']).
entry(beklagen, [b, @, k, l, 'a:', g, @, n]).
entry(bekomme, [b, @, k, 'O', m, @]).
entry(bekommen, [b, @, k, 'O', m, @, n]).
entry(bekommt, [b, @, k, 'O', m, t]).
entry(belassen, [b, @, l, a, s, @, n]).
entry(belasten, [b, @, l, a, s, t, @, n]).
entry(belastet, [b, @, l, a, s, t, @, t]).
entry(belästige, [b, @, l, 'E', s, t, 'I', g, @]).
entry(belästigen, [b, @, l, 'E', s, t, 'I', g, @, n]).
entry(beläuft, [b, @, l, 'OY', f, t]).
entry(beleg, [b, @, l, 'e:', k]).
entry(belegen, [b, @, l, 'e:', g, @, n]).
entry('Belegschaft', [b, @, l, 'e:', k, 'S', a, f, t]).
entry(belegt, [b, @, l, 'e:', k, t]).
entry(beli, [b, @, l, i]).
entry(beliebig, [b, @, l, 'i:', b, 'I', 'C']).
entry(beliebigen, [b, @, l, 'i:', b, 'I', g, @, n]).
entry(beliebiger, [b, @, l, 'i:', b, 'I', g, '6']).
entry(beliebmig, [b, @, l, 'i:', b, m, 'I', 'C']).
entry('Bella-Italia', [b, 'E', l, a, i, t, 'a:', l, j, a]).
entry('Bell', [b, 'E', l]).
entry(bem, [b, @, m]).
entry(bemessen, [b, @, m, 'E', s, @, n]).
entry(bemühe, [b, @, m, 'y:', @]).
entry(bemühen, [b, @, m, 'y:', @, n]).
entry(bemüht, [b, @, m, 'y:', t]).
entry(benachrichtige, [b, @, n, 'a:', x, r, 'I', 'C', t, 'I', g, @]).
entry(benachrichtigen, [b, @, n, 'a:', x, r, 'I', 'C', t, 'I', g, @, n]).
entry(benachrichtigt, [b, @, n, 'a:', x, r, 'I', 'C', t, 'I', 'C', t]).
entry(benannten, [b, @, n, a, n, t, @, n]).
entry(ben, [b, @, n]).
entry(benötige, [b, @, n, '2:', t, 'I', g, @]).
entry(benötigen, [b, @, n, '2:', t, 'I', g, @, n]).
entry(benötigt, [b, @, n, '2:', t, 'I', 'C', t]).
entry(benötigte, [b, @, n, '2:', t, 'I', 'C', t, @]).
entry(benutze, [b, @, n, 'U', ts, @]).
entry(benutzen, [b, @, n, 'U', ts, @, n]).
entry('Benz', [b, 'E', n, ts]).
entry(beobachten, [b, @, 'o:', b, a, x, t, @, n]).
entry(beordert, [b, @, 'O', '6', d, '6', t]).
entry(beplanen, [b, @, p, l, 'a:', n, @, n]).
entry(bequem, [b, @, k, v, 'e:', m]).
entry(bequemen, [b, @, k, v, 'e:', m, @, n]).
entry(bequemer, [b, @, k, v, 'e:', m, '6']).
entry(bequemes, [b, @, k, v, 'e:', m, @, s]).
entry('Bequemlichkeit', [b, @, k, v, 'e:', m, l, 'I', 'C', k, aI, t]).
entry(bequemste, [b, @, k, v, 'e:', m, s, t, @]).
entry(bequemsten, [b, @, k, v, 'e:', m, s, t, @, n]).
entry('Bera', [b, @, r, 'a:']).
entry(beraten, [b, @, r, 'a:', t, @, n]).
entry(beratschlagen, [b, @, r, 'a:', tS, l, 'a:', g, @, n]).
entry(ber, [b, @, r]).
entry(berechnen, [b, @, r, 'E', 'C', n, @, n]).
entry(berechnet, [b, @, r, 'E', 'C', n, @, t]).
entry(bereden, [b, @, r, 'e:', d, @, n]).
entry('Bereich', [b, @, r, aI, 'C']).
entry(bereinschst, [b, @, r, aI, n, 'S', s, t]).
entry(bereit, [b, @, r, aI, t]).
entry(bereite, [b, @, r, aI, t, @]).
entry(bereiten, [b, @, r, aI, t, @, n]).
entry(bereitet, [b, @, r, aI, t, @, t]).
entry(bereitgelegt, [b, @, r, aI, t, g, @, l, 'e:', k, t]).
entry(bereithalten, [b, @, r, aI, t, h, a, l, t, @, n]).
entry(bereits, [b, @, r, aI, ts]).
entry('Bereitschaft', [b, @, r, aI, tS, a, f, t]).
entry(bereitstellen, [b, @, r, aI, tS, t, 'E', l, @, n]).
entry('Berg', [b, 'E', '6', k]).
entry('Berge', [b, 'E', '6', g, @]).
entry('Bergengrün', [b, 'E', '6', g, @, n, g, r, 'y:', n]).
entry('Bergmann', [b, 'E', '6', k, m, a, n]).
entry('Bericht', [b, @, r, 'I', 'C', t]).
entry('Berichte', [b, @, r, 'I', 'C', t, @]).
entry(berichten, [b, @, r, 'I', 'C', t, @, n]).
entry('Berichtes', [b, @, r, 'I', 'C', t, @, s]).
entry(berichtet, [b, @, r, 'I', 'C', t, @, t]).
entry(berichtigen, [b, @, r, 'I', 'C', t, 'I', g, @, n]).
entry('Berichts', [b, @, r, 'I', 'C', ts]).
entry('Berichtverfassung', [b, @, r, 'I', 'C', t, f, 'E', '6', f, a, s, 'U', 'N']).
entry('Berit', [b, 'e:', r, 'I', t]).
entry('Berkemer', [b, 'E', '6', k, @, m, '6']).
entry('Berli', [b, 'E', '6', l, 'i:']).
entry('Berlin', [b, 'E', '6', l, 'i:', n]).
entry('Berlin-Videos', [b, 'E', '6', l, 'i:', n, v, 'i:', d, e, o, s]).
entry('Bernsmann', [b, 'E', '6', n, s, m, a, n]).
entry('Berta', [b, 'E', '6', t, a]).
entry('Bertele', [b, 'E', '6', t, @, l, @]).
entry(berücksichtigen, [b, @, r, 'Y', k, z, 'I', 'C', t, 'I', g, @, n]).
entry('Berücksichtigung', [b, @, r, 'Y', k, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry(beruflich, [b, @, r, 'u:', f, l, 'I', 'C']).
entry('Berufliche', [b, @, r, 'u:', f, l, 'I', 'C', @]).
entry(beruflichen, [b, @, r, 'u:', f, l, 'I', 'C', @, n]).
entry(beruhigend, [b, @, r, 'u:', 'I', g, @, n, t]).
entry(beruhigt, [b, @, r, 'u:', 'I', 'C', t]).
entry(berühmte, [b, @, r, 'y:', m, t, @]).
entry(besagte, [b, @, z, 'a:', k, t, @]).
entry(besagtem, [b, @, z, 'a:', k, t, @, m]).
entry('Bes', [b, @, z]).
entry(bescha, [b, @, 'S', a]).
entry(beschaffe, [b, @, 'S', a, f, @]).
entry(beschaffen, [b, @, 'S', a, f, @, n]).
entry(beschäftigen, [b, @, 'S', 'E', f, t, 'I', g, @, n]).
entry(beschäftigt, [b, @, 'S', 'E', f, t, 'I', 'C', t]).
entry(beschäftigten, [b, @, 'S', 'E', f, t, 'I', 'C', t, @, n]).
entry(beschäftigter, [b, @, 'S', 'E', f, t, 'I', 'C', t, '6']).
entry(beschaulicher, [b, @, 'S', aU, l, 'I', 'C', '6']).
entry(besch, [b, @, 'S']).
entry('Bescheid', [b, @, 'S', aI, t]).
entry(beschließen, [b, @, 'S', l, 'i:', s, @, n]).
entry(beschränken, [b, @, 'S', r, 'E', 'N', k, @, n]).
entry(beschränkt, [b, @, 'S', r, 'E', 'N', k, t]).
entry(beseelt, [b, @, z, 'e:', l, t]).
entry(beseitigen, [b, @, z, aI, t, 'I', g, @, n]).
entry(besetzt, [b, @, z, 'E', ts, t]).
entry(besetzten, [b, @, z, 'E', ts, t, @, n]).
entry('Besetzung', [b, @, z, 'E', ts, 'U', 'N']).
entry(besich, [b, @, z, 'I', 'C']).
entry(besichtigen, [b, @, z, 'I', 'C', t, 'I', g, @, n]).
entry('Besichtigung', [b, @, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry('Besichtigungstour', [b, @, z, 'I', 'C', t, 'I', g, 'U', 'N', s, t, 'u:', '6']).
entry(besitze, [b, @, z, 'I', ts, @]).
entry(besitzt, [b, @, z, 'I', ts, t]).
entry(besondere, [b, @, z, 'O', n, d, @, r, @]).
entry(besonderen, [b, @, z, 'O', n, d, @, r, @, n]).
entry(besonderes, [b, @, z, 'O', n, d, @, r, @, s]).
entry('Besonderheiten', [b, @, z, 'O', n, d, '6', h, aI, t, @, n]).
entry(besonders, [b, @, z, 'O', n, d, '6', s]).
entry(besorge, [b, @, z, 'O', '6', g, @]).
entry(besorgen, [b, @, z, 'O', '6', g, @, n]).
entry(besorgt, [b, @, z, 'O', '6', k, t]).
entry('Besorgungen', [b, @, z, 'O', '6', g, 'U', 'N', @, n]).
entry('Besp', [b, @, 'S', p]).
entry('Bespr', [b, @, 'S', p, r]).
entry('Bespre', [b, @, 'S', p, r, 'E']).
entry(besprech, [b, @, 'S', p, r, 'E', 'C']).
entry(bespreche, [b, @, 'S', p, r, 'E', 'C', @]).
entry(besprechen, [b, @, 'S', p, r, 'E', 'C', @, n]).
entry('Bespreching', [b, @, 'S', p, r, 'E', 'C', 'I', 'N']).
entry(besprechung, [b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Besprechungen', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', @, n]).
entry('Besprechungs', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s]).
entry('Besprechungsraum', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, r, aU, m]).
entry('Besprechungsseminar', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, z, e, m, i, n, a, r]).
entry('Besprechungstermin', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, t, 'E', '6', m, 'i:', n]).
entry('Besprechungstermins', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, t, 'E', '6', m, 'i:', n, s]).
entry('Besprechungszimmer', [b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, ts, 'I', m, '6']).
entry(besprochen, [b, @, 'S', p, r, 'O', x, @, n]).
entry('Besprochene', [b, @, 'S', p, r, 'O', x, @, n, @]).
entry(besser, [b, 'E', s, '6']).
entry(bessere, [b, 'E', s, @, r, @]).
entry(besseren, [b, 'E', s, @, r, @, n]).
entry(besseres, [b, 'E', s, @, r, @, s]).
entry(bestätige, [b, @, 'S', t, 'E:', t, 'I', g, @]).
entry(bestätigen, [b, @, 'S', t, 'E:', t, 'I', g, @, n]).
entry(bestätigt, [b, @, 'S', t, 'E:', t, 'I', 'C', t]).
entry('Bestätigung', [b, @, 'S', t, 'E:', t, 'I', g, 'U', 'N']).
entry('Bestätigungen', [b, @, 'S', t, 'E:', t, 'I', g, 'U', 'N', @, n]).
entry(best, [b, 'E', s, t]).
entry(beste, [b, 'E', s, t, @]).
entry(bestehe, [b, @, 'S', t, 'e:', @]).
entry(bestehen, [b, @, 'S', t, 'e:', @, n]).
entry(besteht, [b, @, 'S', t, 'e:', t]).
entry('Bestei', [b, @, 'S', t, aI]).
entry(bestelle, [b, @, 'S', t, 'E', l, @]).
entry(bestellen, [b, @, 'S', t, 'E', l, @, n]).
entry(bestellt, [b, @, 'S', t, 'E', l, t]).
entry(besten, [b, 'E', s, t, @, n]).
entry(bestens, [b, 'E', s, t, @, n, s]).
entry(bestimm, [b, @, 'S', t, 'I', m]).
entry(bestimmen, [b, @, 'S', t, 'I', m, @, n]).
entry(bestimmt, [b, @, 'S', t, 'I', m, t]).
entry(bestimmte, [b, @, 'S', t, 'I', m, t, @]).
entry(bestimmten, [b, @, 'S', t, 'I', m, t, @, n]).
entry(bestimmtes, [b, @, 'S', t, 'I', m, t, @, s]).
entry('Bestrebungen', [b, @, 'S', t, r, 'e:', b, 'U', 'N', @, n]).
entry(bestreiten, [b, @, 'S', t, r, aI, t, @, n]).
entry(bestünde, [b, @, 'S', t, 'Y', n, d, @]).
entry(besu, [b, @, z, u]).
entry(besuch, [b, @, z, 'u:', x]).
entry(besuche, [b, @, z, 'u:', x, @]).
entry(besuchen, [b, @, z, 'u:', x, @, n]).
entry('Besucher', [b, @, z, 'u:', x, '6']).
entry('Besuchergruppe', [b, @, z, 'u:', x, '6', g, r, 'U', p, @]).
entry('Besuchs', [b, @, z, 'u:', x, s]).
entry('Besuchsreisen', [b, @, z, 'u:', x, s, r, aI, z, @, n]).
entry('Besuchstermin', [b, @, z, 'u:', x, s, t, 'E', '6', m, 'i:', n]).
entry(besucht, [b, @, z, 'u:', x, t]).
entry(besücht, [b, @, z, 'y:', 'C', t]).
entry(besuchten, [b, @, z, 'u:', x, t, @, n]).
entry(besur, [b, @, z, 'u:', '6']).
entry(betätigen, [b, @, t, 'E:', t, 'I', g, @, n]).
entry(betäubt, [b, @, t, 'OY', p, t]).
entry('Bet', [b, @, t]).
entry('Betracht', [b, @, t, r, a, x, t]).
entry(betrachte, [b, @, t, r, a, x, t, @]).
entry(betrachten, [b, @, t, r, a, x, t, @, n]).
entry(beträchtlicher, [b, @, t, r, 'E', 'C', t, l, 'I', 'C', '6']).
entry(betraf, [b, @, t, r, 'a:', f]).
entry('Betrag', [b, @, t, r, 'a:', k]).
entry(betragen, [b, @, t, r, 'a:', g, @, n]).
entry(beträgt, [b, @, t, r, 'E:', k, t]).
entry(betreffen, [b, @, t, r, 'E', f, @, n]).
entry(betreffens, [b, @, t, r, 'E', f, @, n, s]).
entry(betreuen, [b, @, t, r, 'OY', @, n]).
entry('Betrieb', [b, @, t, r, 'i:', p]).
entry(betrieben, [b, @, t, r, 'i:', b, @, n]).
entry('Betriebsausflug', [b, @, t, r, 'i:', p, s, aU, s, f, l, 'u:', k]).
entry('Betriebs', [b, @, t, r, 'i:', p, s]).
entry('Betriebsferien', [b, @, t, r, 'i:', p, s, f, 'e:', '6', j, @, n]).
entry('Betriebskommunikation', [b, @, t, r, 'i:', p, s, k, 'O', m, u, n, i, k, a, ts, j, 'o:', n]).
entry('Betriebskommunikations-Seminar', [b, @, t, r, 'i:', p, s, k, 'O', m, u, n, i, k, a, ts, j, 'o:', n, s, z, e, m, i, n, a, r]).
entry('Betriebsrat', [b, @, t, r, 'i:', p, s, r, 'a:', t]).
entry('Betriebssystem', [b, @, t, r, 'i:', p, s, z, 'Y', s, t, 'e:', m]).
entry('Betriebs-Vorfeier', [b, @, t, r, 'i:', p, s, f, 'o:', '6', f, aI, '6']).
entry(betrifft, [b, @, t, r, 'I', f, t]).
entry(betrinken, [b, @, t, r, 'I', 'N', k, @, n]).
entry(betrüblich, [b, @, t, r, 'y:', p, l, 'I', 'C']).
entry('Bett', [b, 'E', t]).
entry('Betten', [b, 'E', t, @, n]).
entry('Bettina', [b, 'E', t, 'i:', n, a]).
entry(beunruhigend, [b, @, 'U', n, r, 'u:', 'I', g, @, n, t]).
entry(beunruhigende, [b, @, 'U', n, r, 'u:', 'I', g, @, n, d, @]).
entry(beurteilen, [b, @, 'U', '6', t, aI, l, @, n]).
entry(bevor, [b, @, f, 'o:', '6']).
entry(bevorstehende, [b, @, f, 'o:', '6', 'S', t, 'e:', @, n, d, @]).
entry(bevorsteht, [b, @, f, 'o:', '6', 'S', t, 'e:', t]).
entry(bevorzuge, [b, @, f, 'o:', '6', ts, 'u:', g, @]).
entry(bevorzugen, [b, @, f, 'o:', '6', ts, 'u:', g, @, n]).
entry(bevorzugst, [b, @, f, 'o:', '6', ts, 'u:', k, s, t]).
entry(bevorzugte, [b, @, f, 'o:', '6', ts, 'u:', k, t, @]).
entry(bewährt, [b, @, v, 'E:', '6', t]).
entry(bewältigen, [b, @, v, 'E', l, t, 'I', g, @, n]).
entry(bewegen, [b, @, v, 'e:', g, @, n]).
entry(bewegt, [b, @, v, 'e:', k, t]).
entry('Bewegung', [b, @, v, 'e:', g, 'U', 'N']).
entry(bewerkstelligen, [b, @, v, 'E', '6', k, 'S', t, 'E', l, 'I', g, @, n]).
entry(bewerkstelligt, [b, @, v, 'E', '6', k, 'S', t, 'E', l, 'I', 'C', t]).
entry(bewilligt, [b, @, v, 'I', l, 'I', 'C', t]).
entry(bewirtet, [b, @, v, 'I', '6', t, @, t]).
entry(bewögen, [b, @, v, '2:', g, @, n]).
entry(bewußt, [b, @, v, 'U', s, t]).
entry(bezahle, [b, @, ts, 'a:', l, @]).
entry(bezahlen, [b, @, ts, 'a:', l, @, n]).
entry(bezahlt, [b, @, ts, 'a:', l, t]).
entry('Bezahlung', [b, @, ts, 'a:', l, 'U', 'N']).
entry(beziehen, [b, @, ts, 'i:', @, n]).
entry(bezieht, [b, @, ts, 'i:', t]).
entry(beziehungsw, [b, @, ts, 'i:', 'U', 'N', s, v]).
entry(beziehungswei, [b, @, ts, 'i:', 'U', 'N', s, v, aI]).
entry(beziehungsweise, [b, @, ts, 'i:', 'U', 'N', s, v, aI, z, @]).
entry(bezöge, [b, @, ts, '2:', g, @]).
entry('Bezug', [b, @, ts, 'u:', k]).
entry(bezüglich, [b, @, ts, 'y:', k, l, 'I', 'C']).
entry(bezugnehmend, [b, @, ts, 'u:', k, n, 'e:', m, @, n, t]).
entry(bi, [b, 'I']).
entry('Bi', [b, i]).
entry('Bibliothek', [b, i, b, l, i, o, t, 'e:', k]).
entry('Bielefeld', [b, 'i:', l, @, f, 'E', l, t]).
entry('Bielmeier', [b, 'i:', l, m, aI, '6']).
entry('Bier', [b, 'i:', '6']).
entry('Bierchen', [b, 'i:', '6', 'C', @, n]).
entry('Biergärten', [b, 'i:', '6', g, 'E', '6', t, @, n]).
entry('Bierstedt', [b, 'i:', '6', s, t, 'E', t]).
entry(biete, [b, 'i:', t, @]).
entry(bieten, [b, 'i:', t, @, n]).
entry(bietet, [b, 'i:', t, @, t]).
entry('Bilde', [b, 'I', l, d, @]).
entry(bilden, [b, 'I', l, d, @, n]).
entry('Bilder', [b, 'I', l, d, '6']).
entry('Bildschirm', [b, 'I', l, tS, 'I', '6', m]).
entry('Bildungsreise', [b, 'I', l, d, 'U', 'N', s, r, aI, z, @]).
entry('Bill', [b, 'I', l]).
entry('Billen', [b, 'I', l, @, n]).
entry(billig, [b, 'I', l, 'I', 'C']).
entry(billige, [b, 'I', l, 'I', g, @]).
entry(billigen, [b, 'I', l, 'I', g, @, n]).
entry(billiger, [b, 'I', l, 'I', g, '6']).
entry(billigere, [b, 'I', l, 'I', g, @, r, @]).
entry(billigeren, [b, 'I', l, 'I', g, @, r, @, n]).
entry(billigeres, [b, 'I', l, 'I', g, @, r, @, s]).
entry(billigste, [b, 'I', l, 'I', 'C', s, t, @]).
entry(billigsten, [b, 'I', l, 'I', k, s, t, @, n]).
entry(bim, [b, 'I', m]).
entry(binären, [b, i, n, 'E:', r, @, n]).
entry(bin, [b, 'I', n]).
entry('Bindestrich', [b, 'I', n, d, @, 'S', t, r, 'I', 'C']).
entry('Bindestrich-Name', [b, 'I', n, d, @, 'S', t, r, 'I', 'C', n, 'a:', m, @]).
entry(bindungen, [b, 'I', n, d, 'U', 'N', @, n]).
entry(biologischer, [b, i, o, l, 'o:', g, 'I', 'S', '6']).
entry('Birgit', [b, 'I', '6', g, 'I', t]).
entry(birm, [b, 'I', '6', m]).
entry(bis, [b, 'I', s]).
entry(bißche, [b, 'I', s, 'C', @]).
entry(bißchen, [b, 'I', s, 'C', @, n]).
entry(bisher, [b, 'I', s, h, 'e:', '6']).
entry(bislang, [b, 'I', s, l, a, 'N']).
entry(bissel, [b, 'I', s, @, l]).
entry(bist, [b, 'I', s, t]).
entry('Bistro', [b, 'I', s, t, r, 'o:']).
entry(bit, [b, 'I', t]).
entry(bitte, [b, 'I', t, @]).
entry(bitten, [b, 'I', t, @, n]).
entry('Bitterfeld', [b, 'I', t, '6', f, 'E', l, t]).
entry(bla, [b, l, 'a:']).
entry('Bla-Damm', [b, l, a, d, a, m]).
entry(blak, [b, l, a, k]).
entry(blatt, [b, l, a, t]).
entry(blättere, [b, l, 'E', t, @, r, @]).
entry(blättern, [b, l, 'E', t, '6', n]).
entry(blau, [b, l, aU]).
entry('Blaue', [b, l, aU, @]).
entry('Blauen', [b, l, aU, @, n]).
entry(blaumachen, [b, l, aU, m, a, x, @, n]).
entry(bleib, [b, l, aI, p]).
entry(bleibe, [b, l, aI, b, @]).
entry(bleiben, [b, l, aI, b, @, n]).
entry(blei, [b, l, aI]).
entry(bleibt, [b, l, aI, p, t]).
entry(blendend, [b, l, 'E', n, d, @, n, t]).
entry('Blick', [b, l, 'I', k]).
entry(bliebe, [b, l, 'i:', b, @]).
entry(blieben, [b, l, 'i:', b, @, n]).
entry(blind, [b, l, 'I', n, t]).
entry(blitzschnell, [b, l, 'I', ts, 'S', n, 'E', l]).
entry(bloc, [b, l, 'O', k]).
entry('Blöcke', [b, l, '9', k, @]).
entry('Blöcken', [b, l, '9', k, @, n]).
entry(blockiere, [b, l, 'O', k, 'i:', r, @]).
entry(blockiert, [b, l, 'O', k, 'i:', '6', t]).
entry('Blocks', [b, l, 'O', k, s]).
entry('Blockseminar', [b, l, 'O', k, z, e, m, i, n, 'a:', r]).
entry('Blockseminare', [b, l, 'O', k, z, e, m, i, n, 'a:', r, @]).
entry(blöd, [b, l, '2:', t]).
entry(blöde, [b, l, '2:', d, @]).
entry('Blödsinn', [b, l, '2:', t, z, 'I', n]).
entry(bloß, [b, l, 'o:', s]).
entry(blüht, [b, l, 'y:', t]).
entry('Blüm', [b, l, 'y:', m]).
entry('Blume', [b, l, 'u:', m, @]).
entry('Blutspende', [b, l, 'u:', tS, p, 'E', n, d, @]).
entry('Blutspenden', [b, l, 'u:', tS, p, 'E', n, d, @, n]).
entry('Blutspende-Termin', [b, l, 'u:', tS, p, 'E', n, d, @, t, 'E', '6', m, 'i:', n]).
entry(boah, [b, o, 'a:']).
entry(bö, [b, '2:']).
entry('Bob', [b, 'O', b]).
entry('Bobby', [b, 'O', b, i]).
entry('Bobitsch', [b, 'o:', b, 'I', tS]).
entry('Bochum', [b, 'o:', x, 'U', m]).
entry('Boden', [b, 'o:', d, @, n]).
entry('Bodenpersonal', [b, 'o:', d, @, n, p, 'E', '6', z, o, n, a, l]).
entry('Böer', [b, '2:', '6']).
entry('Böhle', [b, '2:', l, @]).
entry('Böhmsche-Brauhaus', [b, '2:', m, 'S', @, b, r, aU, h, aU, s]).
entry('Böhmsches-Brauhaus', [b, '2:', m, 'S', @, s, b, r, aU, h, aU, s]).
entry('Bonn', [b, 'O', n]).
entry('Bonner', [b, 'O', n, '6']).
entry('Bönsch', [b, '9', n, 'S']).
entry('Bordrestaurant', [b, 'O', '6', t, r, 'E', s, t, o, r, 'a~:']).
entry('Borghoff', [b, 'O', '6', k, h, 'O', f]).
entry('Borucki', [b, o, r, 'U', k, i]).
entry('Boß', [b, 'O', s]).
entry(böse, [b, '2:', z, @]).
entry('Botanischen-Garten', [b, o, t, 'a:', n, 'I', 'S', @, n, g, a, r, t, @, n]).
entry(böte, [b, '2:', t, @]).
entry('Boy', [b, 'OY']).
entry('Boyer-Moore-Algorithmus', [b, 'OY', '6', m, 'u:', '6', a, l, g, o, r, 'I', t, m, 'U', s]).
entry(brächten, [b, r, 'E', 'C', t, @, n]).
entry('Brandt', [b, r, a, n, t]).
entry(brasilianisch, [b, r, a, z, i, l, j, 'a:', n, 'I', 'S']).
entry(bräu, [b, r, 'OY']).
entry(brauche, [b, r, aU, x, @]).
entry(brauchen, [b, r, aU, x, @, n]).
entry(braucht, [b, r, aU, x, t]).
entry(brauchte, [b, r, aU, x, t, @]).
entry(bräuchte, [b, r, 'OY', 'C', t, @]).
entry(brauchten, [b, r, aU, x, t, @, n]).
entry(bräuchten, [b, r, 'OY', 'C', t, @, n]).
entry('Brauer', [b, r, aU, '6']).
entry(bräunen, [b, r, 'OY', n, @, n]).
entry(braungebrannt, [b, r, aU, n, g, @, b, r, a, n, t]).
entry('Braunschweig', [b, r, aU, n, 'S', v, aI, k]).
entry(br, [b, r]).
entry(breche, [b, r, 'E', 'C', @]).
entry('Breit', [b, r, aI, t]).
entry(breitgeschlagen, [b, r, aI, t, g, @, 'S', l, 'a:', g, @, n]).
entry('Bremen', [b, r, 'e:', m, @, n]).
entry('Brenner', [b, r, 'E', n, '6']).
entry('Bretten', [b, r, 'E', t, @, n]).
entry('Breuer', [b, r, 'OY', '6']).
entry('Breuler', [b, r, 'OY', l, '6']).
entry('Brib', [b, r, 'I', p]).
entry(bri, [b, r, 'I']).
entry(bricht, [b, r, 'I', 'C', t]).
entry(brick, [b, r, 'I', k]).
entry('Briefing', [b, r, 'i:', f, 'I', 'N']).
entry('Brille', [b, r, 'I', l, @]).
entry(bringe, [b, r, 'I', 'N', @]).
entry(bringen, [b, r, 'I', 'N', @, n]).
entry(bringt, [b, r, 'I', 'N', t]).
entry(brisant, [b, r, i, z, a, n, t]).
entry(brisanter, [b, r, i, z, a, n, t, '6']).
entry('British-Airways', [b, r, 'I', t, 'I', 'S', 'E', '6', w, e, 'I', z]).
entry('Britschow', [b, r, 'I', tS, 'o:']).
entry('Bröggelwirth', [b, r, '9', g, @, l, v, 'I', '6', t]).
entry('Brohl', [b, r, 'o:', l]).
entry('Broschüre', [b, r, 'O', 'S', 'y:', r, @]).
entry('Broschüren', [b, r, 'O', 'S', 'y:', r, @, n]).
entry('Brötchen', [b, r, '2:', t, 'C', @, n]).
entry(brück, [b, r, 'Y', k]).
entry('Brücke', [b, r, 'Y', k, @]).
entry('Bruder', [b, r, 'u:', d, '6']).
entry('Brunchen', [b, r, a, n, 'S', @, n]).
entry('Brüssel', [b, r, 'Y', s, @, l]).
entry(bubstabiere, [b, 'u:', p, 'S', t, a, b, 'i:', r, @]).
entry('Buch', [b, 'u:', x]).
entry(buche, [b, 'u:', x, @]).
entry(buchen, [b, 'u:', x, @, n]).
entry('Bücher', [b, 'y:', 'C', '6']).
entry(buchs, [b, 'u:', x, s]).
entry('Buchsch', [b, 'u:', x, 'S']).
entry(buchspa, [b, 'u:', x, 'S', p, a]).
entry(buchstabiere, [b, 'u:', x, 'S', t, a, b, 'i:', r, @]).
entry(buchstabieren, [b, 'u:', x, 'S', t, a, b, 'i:', r, @, n]).
entry('Buchstabiererei', [b, 'u:', x, 'S', t, a, b, 'i:', r, @, r, aI]).
entry(buchstabiert, [b, 'u:', x, 'S', t, a, b, 'i:', '6', t]).
entry('Buchstabierung', [b, 'u:', x, 'S', t, a, b, 'i:', r, 'U', 'N']).
entry(buchst, [b, 'u:', x, s, t]).
entry(bucht, [b, 'u:', x, t]).
entry('Buchung', [b, 'u:', x, 'U', 'N']).
entry(buchungen, [b, 'u:', x, 'U', 'N', @, n]).
entry('Buchungsbestätigung', [b, 'u:', x, 'U', 'N', s, b, @, 'S', t, 'E:', t, 'I', g, 'U', 'N']).
entry('Buchungsterminen', [b, 'u:', x, 'U', 'N', s, t, 'E', '6', m, 'i:', n, @, n]).
entry('Budget', [b, 'Y', d, 'Z', 'e:']).
entry('Budgets', [b, 'Y', d, 'Z', 'e:', s]).
entry(buffet, [b, 'Y', f, 'e:']).
entry('Bühne', [b, 'y:', n, @]).
entry('Bulmahn', [b, 'u:', l, m, a, n]).
entry('Bummel', [b, 'U', m, @, l]).
entry(bummeln, [b, 'U', m, @, l, n]).
entry('Bundesbahn', [b, 'U', n, d, @, s, b, 'a:', n]).
entry('Bundesgebiet', [b, 'U', n, d, @, s, g, @, b, 'i:', t]).
entry('Bundeskanzler', [b, 'U', n, d, @, s, k, a, n, ts, l, '6']).
entry('Bundesländer', [b, 'U', n, d, @, s, l, 'E', n, d, '6']).
entry('Bundesregierung', [b, 'U', n, d, @, s, r, 'E', g, 'i:', r, 'U', 'N']).
entry(bunt, [b, 'U', n, t]).
entry('Burger', [b, '9', '6', g, '6']).
entry('Büro', [b, y, r, 'o:']).
entry('Büros', [b, y, r, 'o:', s]).
entry('Busbahnhof', [b, 'U', s, b, 'a:', n, h, 'o:', f]).
entry('Bus', [b, 'U', s]).
entry(busch, [b, 'U', 'S']).
entry('Busineß', [b, 'I', s, n, @, s]).
entry('Busineß-Class', [b, 'I', s, n, @, s, k, l, 'a:', s]).
entry('Busineß-Ticket', [b, 'I', s, n, @, s, t, 'I', k, @, t]).
entry('Buslinie', [b, 'U', s, l, 'i:', n, j, @]).
entry('Bußmonat', [b, 'u:', s, m, 'o:', n, a, t]).
entry('Busnetz', [b, 'U', s, n, 'E', ts]).
entry('Busreise', [b, 'U', s, r, aI, z, @]).
entry('Busse', [b, 'U', s, @]).
entry('Bussen', [b, 'U', s, @, n]).
entry('Bußtag', [b, 'u:', s, t, 'a:', k]).
entry('Bustransfer', [b, 'U', s, t, r, a, n, s, f, 'e:', '6']).
entry('Buß-und-Bettag', [b, 'u:', s, 'U', n, t, b, 'e:', t, t, 'a:', k]).
entry('Busverbindung', [b, 'U', s, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('But', [b, 'U', t]).
entry('Butler', [b, a, t, l, '6']).
entry('Cabriolet', [k, a, b, r, i, o, l, 'e:']).
entry('Cafe', [k, a, f, 'e:']).
entry('Cafeteria', [k, a, f, e, t, @, r, 'i:', a]).
entry(canceln, [k, 'E', n, ts, @, l, n]).
entry(card, [k, 'a:', r, d]).
entry('Carpenter', [k, 'a:', r, p, @, n, t, '6']).
entry('Cäsar', [ts, 'E:', z, a, r]).
entry(cashier, [k, 'E', 'S', 'I', '6']).
entry('CeBit-Messe', [ts, 'e:', b, 'I', t, m, 'E', s, @]).
entry('CeBit', [ts, 'e:', b, 'I', t]).
entry('Celle', [ts, 'E', l, @]).
entry('Cent', [s, 'E', n, t]).
entry(cetera, [ts, 'E', t, @, r, a]).
entry('Champagner', ['S', a, m, p, a, n, j, '6']).
entry('Chancen', ['S', 'a~:', s, @, n]).
entry('Chance', ['S', 'a~:', s, @]).
entry('Chaos', [k, 'a:', 'O', s]).
entry('Charles', [tS, 'a:', l, s]).
entry('Chauffeur', ['S', 'O', f, '2:', '6']).
entry(ch, ['C']).
entry(checken, [tS, 'E', k, @, n]).
entry(checke, [tS, 'E', k, @]).
entry('Check-In-Counter', [tS, 'E', k, 'I', n, k, aU, n, t, '6']).
entry(checkin, [tS, 'E', k, 'I', n]).
entry(checkout, [tS, 'E', k, aU, t]).
entry('Chefetage', ['S', e, f, e, t, 'a:', 'Z', @]).
entry('Chef', ['S', 'E', f]).
entry('Chefs', ['S', 'E', f, s]).
entry('Chemnitz', [k, 'E', m, n, 'I', ts]).
entry(che, [x, @]).
entry('Chez-Jacques', ['S', e, 'Z', a, k]).
entry('China', ['C', 'i:', n, a]).
entry(chines, ['C', i, n, e, s]).
entry('Chinese', ['C', i, n, 'e:', z, @]).
entry('Chinesen', ['C', i, n, 'e:', z, @, n]).
entry('Chinesisch', ['C', i, n, 'e:', z, 'I', 'S']).
entry(chinesischen, ['C', i, n, 'e:', z, 'I', 'S', @, n]).
entry(chinesisches, ['C', i, n, 'e:', z, 'I', 'S', @, s]).
entry(chotto, [k, 'O', t, o]).
entry('Chris', [k, r, 'I', s]).
entry('Christian', [k, r, 'I', s, t, j, a, n]).
entry('Christie', [k, r, 'I', s, t, i]).
entry('Christi-Himmelfahrt', [k, r, 'I', s, t, i, h, 'I', m, @, l, f, 'a:', r, t]).
entry('Christi-Himmelfahrt-Woche', [k, r, 'I', s, t, i, h, 'I', m, @, l, f, 'a:', r, t, v, 'O', x, @]).
entry('Christin', [k, r, 'I', s, t, 'I', n]).
entry('Christ', [k, r, 'I', s, t]).
entry(christlichere, [k, r, 'I', s, t, l, 'I', 'C', @, r, @]).
entry('Christopher', [k, r, 'I', s, t, @, f, @]).
entry('Chr', [k, r]).
entry(chronologischen, [k, r, o, n, o, l, 'o:', g, 'I', 'S', @, n]).
entry(chsten, ['C', s, t, @, n]).
entry(chung, [x, 'U', 'N']).
entry('Citylight', [s, 'I', t, i, l, aI, t]).
entry('City', [s, 'I', t, i]).
entry('Ciupke', [ts, 'U', p, k, @]).
entry(ckiert, [k, 'i:', '6', t]).
entry(class, [k, l, 'a:', s]).
entry('Claudia', [k, l, aU, d, j, a]).
entry('Clou', [k, l, 'u:']).
entry('Coburg', [k, 'o:', b, 'U', '6', k]).
entry('Cocktails', [k, 'O', k, t, 'e:', l, s]).
entry('Cognac', [k, 'O', n, j, a, k]).
entry('Co', [k, o]).
entry('Colgate', [k, @, 'U', l, g, e, 'I', t]).
entry('Colina', [k, o, l, 'i:', n, a]).
entry('Computeranlage', [k, 'O', m, p, j, 'u:', t, '6', a, n, l, 'a:', g, @]).
entry('Computerbeschaffung', [k, 'O', m, p, j, 'u:', t, '6', b, @, 'S', a, f, 'U', 'N']).
entry('Computer', [k, 'O', m, p, j, 'u:', t, '6']).
entry('Computermesse', [k, 'O', m, p, j, 'u:', t, '6', m, 'E', s, @]).
entry('Computern', [k, 'O', m, p, j, 'u:', t, '6', n]).
entry('Computers', [k, 'O', m, p, j, 'u:', t, '6', s]).
entry('Concorde-am-Leineschloß', [k, 'O', n, k, 'O', '6', t, a, m, l, aI, n, @, 'S', l, 'O', s]).
entry('Conesa', [k, 'O', n, 'e:', z, a]).
entry('Controller', [k, 'O', n, t, r, 'o:', l, '6']).
entry(counter, [k, aU, n, t, '6']).
entry('Cousine', [k, u, z, 'i:', n, @]).
entry('Cristal-Hotel', [k, r, 'I', s, t, @, l, h, o, t, 'E', l]).
entry('Cristal', [k, r, 'I', s, t, @, l]).
entry('Cristial', [k, r, 'I', s, t, j, a, l]).
entry('Cuno', [k, 'u:', n, o]).
entry('Curtis', [k, '9', '6', t, 'I', s]).
entry(dabeibleiben, [d, a, b, aI, b, l, aI, b, @, n]).
entry(dabei, [d, a, b, aI]).
entry(dabeihabe, [d, a, b, aI, h, 'a:', b, @]).
entry(dabeihaben, [d, a, b, aI, h, 'a:', b, @, n]).
entry(dabeisein, [d, a, b, aI, z, aI, n]).
entry(dableiben, [d, 'a:', b, l, aI, b, @, n]).
entry(daccord, [d, a, k, 'o:', '6']).
entry('Dach', [d, a, x]).
entry(dachte, [d, a, x, t, @]).
entry(dachten, [d, a, x, t, @, n]).
entry(da, [d, 'a:']).
entry(dadadam, [d, a, d, a, d, a, m]).
entry(dä, [d, 'E']).
entry(dadurch, [d, a, d, 'U', '6', 'C']).
entry(daf, [d, a, f]).
entry(dafür, [d, a, f, 'y:', '6']).
entry(dagegen, [d, a, g, 'e:', g, @, n]).
entry(dagevö, [d, a, g, @, v, '2']).
entry('Da-Giovanni', [d, a, d, 'Z', o, v, a, n, i]).
entry(dah, [d, a, h]).
entry(daheim, [d, a, h, aI, m]).
entry(daher, [d, a, h, 'e:', '6']).
entry(dahin, [d, a, h, 'I', n]).
entry(dahingehend, [d, 'a:', h, 'I', n, g, 'e:', @, n, t]).
entry(dahinkommen, [d, a, h, 'I', n, k, 'O', m, @, n]).
entry(dahinter, [d, a, h, 'I', n, t, '6']).
entry(dahinzukommen, [d, a, h, 'I', n, ts, u, k, 'O', m, @, n]).
entry('Dahnken', [d, 'a:', n, k, @, n]).
entry(daliegen, [d, 'a:', l, 'i:', g, @, n]).
entry(damals, [d, 'a:', m, a, l, s]).
entry(dam, [d, a, m]).
entry('Dam', [d, 'a:', m]).
entry('Dame', [d, 'a:', m, @]).
entry(damit, [d, a, m, 'I', t]).
entry('Dammtor', [d, 'E', m, t, 'O', '6']).
entry('Dampfbad', [d, a, m, pf, b, 'a:', t]).
entry(dampfbaden, [d, a, m, pf, b, 'a:', d, @, n]).
entry('Dampfbädern', [d, a, m, pf, b, 'E:', d, '6', n]).
entry('Dampfbad-Hotel', [d, a, m, pf, b, 'a:', t, h, o, t, 'E', l]).
entry('Dampfplauderei', [d, a, m, pf, p, l, aU, d, @, r, aI]).
entry(danach, [d, a, n, 'a:', x]).
entry(dan, [d, a, n]).
entry(daneben, [d, a, n, 'e:', b, @, n]).
entry('Dani', [d, 'a:', n, i]).
entry(dankbar, [d, a, 'N', k, b, 'a:', r]).
entry('Dank', [d, a, 'N', k]).
entry(danke, [d, a, 'N', k, @]).
entry(danken, [d, a, 'N', k, @, n]).
entry('Danksagung', [d, a, 'N', k, z, 'a:', g, 'U', 'N']).
entry(danns, [d, a, n, s]).
entry(daran, [d, a, r, a, n]).
entry(darauf, [d, a, r, aU, f]).
entry(darauffolgend, [d, a, r, aU, f, f, 'O', l, g, @, n, t]).
entry(darauffolgende, [d, a, r, aU, f, f, 'O', l, g, @, n, d, @]).
entry(darauffolgenden, [d, a, r, aU, f, f, 'O', l, g, @, n, d, @, n]).
entry(daraufhin, [d, a, r, aU, f, h, 'I', n]).
entry(daraus, [d, a, r, aU, s]).
entry(darbringen, [d, 'a:', r, b, r, 'I', 'N', @, n]).
entry(dar, [d, a, r]).
entry(darf, [d, a, r, f]).
entry(dargestellt, [d, 'a:', r, g, @, 'S', t, 'E', l, t]).
entry(darin, [d, 'a:', r, 'I', n]).
entry('Darmstadt', [d, a, r, m, 'S', t, a, t]).
entry(darstellen, [d, 'a:', r, 'S', t, 'E', l, @, n]).
entry(darstellenden, [d, 'a:', r, 'S', t, 'E', l, @, n, d, @, n]).
entry(darüber, [d, a, r, 'y:', b, '6']).
entry(darum, [d, 'a:', r, 'U', m]).
entry(daß, [d, a, s]).
entry(dasselbe, [d, a, s, z, 'E', l, b, @]).
entry(date, [d, 'e:', t]).
entry('Datenangabe', [d, 'a:', t, @, n, a, n, g, 'a:', b, @]).
entry(daten, [d, 'a:', t, @, n]).
entry('Datenschutz', [d, 'a:', t, @, n, 'S', 'U', ts]).
entry('Datenschutzplan', [d, 'a:', t, @, n, 'S', 'U', ts, p, l, 'a:', n]).
entry('Datensicherung', [d, 'a:', t, @, n, z, 'I', 'C', '6', r, 'U', 'N']).
entry('Datum', [d, 'a:', t, 'U', m]).
entry('Datums', [d, 'a:', t, 'U', m, s]).
entry(dau, [d, aU]).
entry(dauer, [d, aU, '6']).
entry(dauern, [d, aU, '6', n]).
entry(dauernd, [d, aU, '6', n, t]).
entry(dauert, [d, aU, '6', t]).
entry('Dauerverpflichtungen', [d, aU, '6', f, 'E', '6', pf, l, 'I', 'C', t, 'U', 'N', @, n]).
entry('Däumchen', [d, 'OY', m, 'C', @, n]).
entry('Daumen', [d, aU, m, @, n]).
entry('Dave', [d, 'e:', f]).
entry(davon, [d, a, f, 'O', n]).
entry(davontragen, [d, a, f, 'O', n, t, r, 'a:', g, @, n]).
entry(davor, [d, a, f, 'o:', '6']).
entry(davorlegen, [d, a, f, 'o:', '6', l, 'e:', g, @, n]).
entry(daz, [d, a, ts]).
entry(dazu, [d, a, ts, 'u:']).
entry(dazugehört, [d, a, ts, 'u:', g, @, h, '2:', '6', t]).
entry(dazunehmen, [d, a, ts, 'u:', n, 'e:', m, @, n]).
entry(dazuplanen, [d, a, ts, 'u:', p, l, 'a:', n, @, n]).
entry(dazurechnen, [d, a, ts, 'u:', r, 'E', 'C', n, @, n]).
entry(dazusagen, [d, a, ts, 'u:', z, 'a:', g, @, n]).
entry(dazusein, [d, a, ts, 'u:', z, aI, n]).
entry(dazustoßen, [d, a, ts, 'u:', 'S', t, 'o:', s, @, n]).
entry(dazuzählen, [d, a, ts, 'u:', ts, 'E:', l, @, n]).
entry(dazwischenbekommen, [d, a, ts, v, 'I', 'S', @, n, b, @, k, 'O', m, @, n]).
entry(dazwischen, [d, a, ts, v, 'I', 'S', @, n]).
entry(dazwischenkommen, [d, a, ts, v, 'I', 'S', @, n, k, 'O', m, @, n]).
entry(dazwischenkommt, [d, a, ts, v, 'I', 'S', @, n, k, 'O', m, t]).
entry(dazwischenliegen, [d, a, ts, v, 'I', 'S', @, n, l, 'i:', g, @, n]).
entry(dazwischenquetschen, [d, a, ts, v, 'I', 'S', @, n, k, v, 'E', tS, @, n]).
entry(dazwischenschalten, [d, a, ts, v, 'I', 'S', @, n, 'S', a, l, t, @, n]).
entry(dazwischenschieben, [d, a, ts, v, 'I', 'S', @, n, 'S', 'i:', b, @, n]).
entry('D', [d, 'e:']).
entry('Debatte', [d, e, b, a, t, @]).
entry(deckt, [d, 'E', k, t]).
entry(de, [d, e]).
entry(definitiv, [d, e, f, i, n, i, t, 'i:', f]).
entry(dehnt, [d, 'e:', n, t]).
entry('Deichsel', [d, e, 'I', k, s, @, l]).
entry(dein, [d, aI, n]).
entry(deine, [d, aI, n, @]).
entry(deinem, [d, aI, n, @, m]).
entry(deinen, [d, aI, n, @, n]).
entry(deiner, [d, aI, n, '6']).
entry(deines, [d, aI, n, @, s]).
entry(deklarieren, [d, e, k, l, a, r, 'i:', r, @, n]).
entry(deklariert, [d, e, k, l, a, r, 'i:', '6', t]).
entry(delegieren, [d, e, l, e, g, 'i:', r, @, n]).
entry(deln, [d, 'E', l, n]).
entry(dem, [d, 'e:', m]).
entry(dementsprechend, [d, 'e:', m, 'E', n, tS, p, r, 'E', 'C', @, n, t]).
entry(demnach, [d, 'e:', m, n, 'a:', x]).
entry(demnächst, [d, 'e:', m, n, 'E:', 'C', s, t]).
entry(demselben, [d, 'e:', m, z, 'E', l, b, @, n]).
entry(demzufolge, [d, 'e:', m, ts, u, f, 'O', l, g, @]).
entry(den, [d, 'e:', n]).
entry('Denecke', [d, 'e:', n, @, k, @]).
entry(denen, [d, 'e:', n, @, n]).
entry(denkbar, [d, 'E', 'N', k, b, 'a:', r]).
entry(denk, [d, 'E', 'N', k]).
entry(denke, [d, 'E', 'N', k, @]).
entry(denken, [d, 'E', 'N', k, @, n]).
entry(denkst, [d, 'E', 'N', k, s, t]).
entry(denn, [d, 'E', n]).
entry(dennoch, [d, 'E', n, 'O', x]).
entry(denselben, [d, 'e:', n, z, 'E', l, b, @, n]).
entry(derartig, [d, 'e:', '6', 'a:', r, t, 'I', 'C']).
entry(der, [d, 'e:', '6']).
entry(dergleichen, [d, 'e:', '6', g, l, aI, 'C', @, n]).
entry(derjenige, [d, 'e:', '6', j, 'e:', n, 'I', g, @]).
entry(dermaßen, [d, 'e:', r, m, 'a:', s, @, n]).
entry(derselbe, [d, 'e:', '6', z, 'E', l, b, @]).
entry(derselben, [d, 'e:', '6', z, 'E', l, b, @, n]).
entry(derweil, [d, 'e:', '6', v, aI, l]).
entry(derzeit, [d, 'e:', '6', ts, aI, t]).
entry('Der-zerbrochene-Krug', [d, 'e:', '6', ts, 'E', '6', b, r, 'O', x, @, n, @, k, r, 'u:', k]).
entry('Deschinger', [d, 'E', 'S', 'I', 'N', '6']).
entry(des, [d, 'E', s]).
entry(desgleichen, [d, 'E', s, g, l, aI, 'C', @, n]).
entry(deshalb, [d, 'E', s, h, a, l, p]).
entry(dessen, [d, 'E', s, @, n]).
entry(desto, [d, 'E', s, t, o]).
entry(deswegen, [d, 'E', s, v, 'e:', g, @, n]).
entry(detailliert, [d, e, t, a, j, 'i:', '6', t]).
entry('Details', [d, e, t, aI, s]).
entry(deutlich, [d, 'OY', t, l, 'I', 'C']).
entry(deutsch, [d, 'OY', tS]).
entry('Deutsche-Bahn', [d, 'OY', tS, @, b, 'a:', n]).
entry('Deutsche-Bundesbahn', [d, 'OY', tS, @, b, 'U', n, d, @, s, b, 'a:', n]).
entry(deutsche, [d, 'OY', tS, @]).
entry('Deutschen-Bahn', [d, 'OY', tS, @, n, b, 'a:', n]).
entry('Deutschen-Bundesbahn', [d, 'OY', tS, @, n, b, 'U', n, d, @, s, b, 'a:', n]).
entry(deutschen, [d, 'OY', tS, @, n]).
entry('Deutschherrenstraße', [d, 'OY', tS, h, 'E', r, @, n, 'S', t, r, 'a:', s, @]).
entry('Deutschland', [d, 'OY', tS, l, a, n, t]).
entry('Deutschland-Flügen', [d, 'OY', tS, l, a, n, t, f, l, 'y:', g, @, n]).
entry('Deutschlands', [d, 'OY', tS, l, a, n, ts]).
entry('Devise', [d, e, v, 'i:', z, @]).
entry('Dezember', [d, e, ts, 'E', m, b, '6']).
entry('Dezemberhälfte', [d, e, ts, 'E', m, b, '6', h, 'E', l, f, t, @]).
entry('Dezembers', [d, e, ts, 'E', m, b, '6', s]).
entry('Dezembertage', [d, e, ts, 'E', m, b, '6', t, 'a:', g, @]).
entry('Dezembertermin', [d, e, ts, 'E', m, b, '6', t, 'E', '6', m, 'i:', n]).
entry('Dezemberwoche', [d, e, ts, 'E', m, b, '6', v, 'O', x, @]).
entry('Dezemberwochen', [d, e, ts, 'E', m, b, '6', v, 'O', x, @, n]).
entry(df, [t, f]).
entry('Diaabend', [d, 'i:', a, 'a:', b, @, n, t]).
entry('Diaabend-Weintrink', [d, 'i:', a, 'a:', b, @, n, t, v, aI, n, t, r, 'I', 'N', k]).
entry('Dialog', [d, i, a, l, 'o:', k]).
entry('Dias', [d, 'i:', a, s]).
entry(dich, [d, 'I', 'C']).
entry(dicht, [d, 'I', 'C', t]).
entry(dichter, [d, 'I', 'C', t, '6']).
entry(dichtgedrängten, [d, 'I', 'C', t, g, @, d, r, 'E', 'N', t, @, n]).
entry(dick, [d, 'I', k]).
entry(dicke, [d, 'I', k, @]).
entry(di, [d, i]).
entry('Di', [d, 'i:']).
entry(dien, [d, 'i:', @, n]).
entry('Dien', [d, 'i:', n]).
entry(dienen, [d, 'i:', n, @, n]).
entry('Diens', [d, 'i:', n, s]).
entry(diensen, [d, 'i:', n, z, @, n]).
entry('Diensta', [d, 'i:', n, s, t, a]).
entry('Dienstag', [d, 'i:', n, s, t, 'a:', k]).
entry('Dienstage', [d, 'i:', n, s, t, 'a:', g, @]).
entry('Dienstagen', [d, 'i:', n, s, t, 'a:', g, @, n]).
entry(dienstags, [d, 'i:', n, s, t, 'a:', k, s]).
entry('Dienstagstermin', [d, 'i:', n, s, t, 'a:', k, s, t, 'E', '6', m, 'i:', n]).
entry('Dienstanweisung', [d, 'i:', n, s, t, a, n, v, aI, z, u, 'N']).
entry('Dienstauto', [d, 'i:', n, s, t, aU, t, o]).
entry('Dienstbericht', [d, 'i:', n, s, t, b, @, r, 'I', 'C', t]).
entry('Dienstbesprechung', [d, 'i:', n, s, t, b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Dienstbesprechungen', [d, 'i:', n, s, t, b, @, 'S', p, r, 'E', 'C', 'U', 'N', @, n]).
entry('Dienst', [d, 'i:', n, s, t]).
entry('Dienstkalender', [d, 'i:', n, s, t, k, a, l, 'E', n, d, '6']).
entry(dienstlich, [d, 'i:', n, s, t, l, 'I', 'C']).
entry('Dienstplanbesprechung', [d, 'i:', n, s, t, p, l, 'a:', n, b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Dienstplan', [d, 'i:', n, s, t, p, l, 'a:', n]).
entry('Dienstr', [d, 'i:', n, s, t, r]).
entry('Dienstreise', [d, 'i:', n, s, t, r, aI, z, @]).
entry('Dienstreisen', [d, 'i:', n, s, t, r, aI, z, @, n]).
entry('Dienststelle', [d, 'i:', n, s, tS, t, 'E', l, @]).
entry('Dienstverpflichtung', [d, 'i:', n, s, t, f, 'E', '6', pf, l, 'I', 'C', t, 'U', 'N']).
entry('Dienstwagen', [d, 'i:', n, s, t, v, 'a:', g, @, n]).
entry('Dienstwoche', [d, 'i:', n, s, t, v, 'O', x, @]).
entry('Dienstzeit', [d, 'i:', n, s, t, ts, aI, t]).
entry('Dienstzimmer', [d, 'i:', n, s, t, ts, 'I', m, '6']).
entry('Diepholz', [d, 'i:', p, h, 'O', l, ts]).
entry('Die-Physiker', [d, 'i:', f, 'y:', z, 'I', k, '6']).
entry(diesbezüglich, [d, 'i:', s, b, @, ts, 'y:', k, l, 'I', 'C']).
entry(dies, [d, 'i:', s]).
entry(diese, [d, 'i:', z, @]).
entry(dieselbe, [d, 'i:', z, 'E', l, b, @]).
entry(dieselben, [d, 'i:', z, 'E', l, b, @, n]).
entry(diesem, [d, 'i:', z, @, m]).
entry(diesen, [d, 'i:', z, @, n]).
entry(dieser, [d, 'i:', z, '6']).
entry(dieses, [d, 'i:', z, @, s]).
entry(diesjährige, [d, 'i:', s, j, 'E:', r, 'I', g, @]).
entry(diesjährigen, [d, 'i:', s, j, 'E:', r, 'I', g, @, n]).
entry(diesmal, [d, 'i:', s, m, 'a:', l]).
entry('Diesner', [d, 'i:', s, n, '6']).
entry('Dietmar', [d, 'i:', t, m, a, r]).
entry(dieweil, [d, 'i:', v, aI, l]).
entry(dil, [d, i, l]).
entry(ding, [d, 'I', 'N']).
entry('Dinge', [d, 'I', 'N', @]).
entry('Dingen', [d, 'I', 'N', @, n]).
entry('Dinger', [d, 'I', 'N', '6']).
entry('Dingler', [d, 'I', 'N', l, '6']).
entry('Dings', [d, 'I', 'N', s]).
entry(dir, [d, 'i:', '6']).
entry(direkt, [d, i, r, 'E', k, t]).
entry(direkte, [d, i, r, 'E', k, t, @]).
entry(direkten, [d, i, r, 'E', k, t, @, n]).
entry(direkter, [d, i, r, 'E', k, t, '6']).
entry('Direktflug', [d, i, r, 'E', k, t, f, l, 'u:', k]).
entry('Direktflüge', [d, i, r, 'E', k, t, f, l, 'y:', g, @]).
entry('Direktion', [d, i, r, 'E', k, ts, j, 'o:', n]).
entry(direktiven, [d, i, r, 'E', k, t, 'i:', v, @, n]).
entry('Direktorium', [d, i, r, 'E', k, t, 'o:', '6', j, 'U', m]).
entry(direnkt, [d, i, r, 'E', 'N', k, t]).
entry('Disco', [d, 'I', s, k, o]).
entry(discount, [d, 'I', s, k, aU, n, t]).
entry(discounts, [d, 'I', s, k, aU, n, ts]).
entry(dis, [d, 'I', s]).
entry('Diskothek', [d, 'I', s, k, o, t, 'e:', k]).
entry(diskriminierend, [d, 'I', s, k, r, i, m, i, n, 'i:', r, @, n, t]).
entry('Diskussion', [d, 'I', s, k, 'U', s, j, 'o:', n]).
entry('Diskussionspunkte', [d, 'I', s, k, 'U', s, j, 'o:', n, s, p, 'U', 'N', k, t, @]).
entry(disponibel, [d, 'I', s, p, o, n, 'i:', b, @, l]).
entry(disponierbar, [d, 'I', s, p, o, n, 'i:', '6', b, 'a:', r]).
entry(disponieren, [d, 'I', s, p, o, n, 'i:', r, @, n]).
entry('Disposition', [d, 'I', s, p, o, z, i, ts, j, 'o:', n]).
entry('Distanz', [d, 'I', s, t, a, n, ts]).
entry(diverse, [d, i, v, 'E', '6', z, @]).
entry(diversen, [d, i, v, 'E', '6', z, @, n]).
entry('Diviak', [d, i, v, i, a, k]).
entry(dleich, [d, l, aI, 'C']).
entry(doch, [d, 'O', x]).
entry(do, [d, o]).
entry('Doktorandentreffen', [d, 'O', k, t, o, r, a, n, d, @, n, t, r, 'E', f, @, n]).
entry('Doktor', [d, 'O', k, t, 'o:', '6']).
entry('Dollar', [d, 'O', l, a, r]).
entry('Doll', [d, 'O', l]).
entry('Dom', [d, 'o:', m]).
entry('Dominikanische-Republik', [d, o, m, i, n, i, k, 'a:', n, 'I', 'S', @, r, e, p, u, b, l, 'i:', k]).
entry('Don', [d, 'O', n]).
entry('Donigweg', [d, 'o:', n, 'I', 'C', v, 'e:', k]).
entry('Donnach', [d, 'O', n, a, x]).
entry('Donner', [d, 'O', n, '6']).
entry(donners, [d, 'O', n, '6', s]).
entry('Donnersta', [d, 'O', n, '6', s, t, 'a:']).
entry('Donnerstag', [d, 'O', n, '6', s, t, 'a:', k]).
entry('Donnerstage', [d, 'O', n, '6', s, t, 'a:', g, @]).
entry('Donnerstagen', [d, 'O', n, '6', s, t, 'a:', g, @, n]).
entry(donnerstags, [d, 'O', n, '6', s, t, 'a:', k, s]).
entry('Donnerst', [d, 'O', n, '6', s, t]).
entry(doof, [d, 'o:', f]).
entry(doofe, [d, 'o:', f, @]).
entry('Doppelbett', [d, 'O', p, @, l, b, 'E', t]).
entry('Doppel', [d, 'O', p, @, l]).
entry('Doppelname', [d, 'O', p, @, l, n, 'a:', m, @]).
entry('Doppel-Paula', [d, 'O', p, @, l, p, aU, l, a]).
entry(doppelt, [d, 'O', p, @, l, t]).
entry('Doppelte', [d, 'O', p, @, l, t, @]).
entry('Doppelverglasung', [d, 'O', p, @, l, f, 'E', '6', g, l, 'a:', z, 'U', 'N']).
entry('Doppelzimmer', [d, 'O', p, @, l, ts, 'I', m, '6']).
entry('Dora', [d, 'o:', r, a]).
entry('Dorint-Hotel', [d, o, r, 'I', n, t, h, o, t, 'E', l]).
entry('Doris', [d, 'o:', r, 'I', s]).
entry('Dorn', [d, 'O', '6', n]).
entry('Dorner', [d, 'O', '6', n, '6']).
entry(dortbleiben, [d, 'O', '6', t, b, l, aI, b, @, n]).
entry(dort, [d, 'O', '6', t]).
entry(dorten, [d, 'O', '6', t, @, n]).
entry(dorthi, [d, 'O', '6', t, i]).
entry(dorthin, [d, 'O', '6', t, h, 'I', n]).
entry(dortigen, [d, 'O', '6', t, 'I', g, @, n]).
entry('Dortm', [d, 'O', '6', t, m]).
entry('Dortmund', [d, 'O', '6', t, m, 'U', n, t]).
entry('Dotzler', [d, 'O', ts, l, '6']).
entry(dramatisch, [d, r, a, m, 'a:', t, 'I', 'S']).
entry(dran, [d, r, a, n]).
entry(drängeln, [d, r, 'E', 'N', @, l, n]).
entry(drängen, [d, r, 'E', 'N', @, n]).
entry(drängt, [d, r, 'E', 'N', t]).
entry(dranhängen, [d, r, a, n, h, 'E', 'N', @, n]).
entry(dranzuhängen, [d, r, a, n, ts, u, h, 'E', 'N', @, n]).
entry(drau, [d, r, aU]).
entry(drauf, [d, r, aU, f]).
entry(drauffolgende, [d, r, aU, f, f, 'O', l, g, @, n, d, @]).
entry(drauffolgenden, [d, r, aU, f, f, 'O', l, g, @, n, d, @, n]).
entry(draufgehen, [d, r, aU, f, g, 'e:', @, n]).
entry(draufhauen, [d, r, aU, f, h, aU, @, n]).
entry(draufstehen, [d, r, aU, f, 'S', t, 'e:', @, n]).
entry(draufzahle, [d, r, aU, f, ts, 'a:', l, @]).
entry(draufzahlen, [d, r, aU, f, ts, 'a:', l, @, n]).
entry(draus, [d, r, aU, s]).
entry(draußen, [d, r, aU, s, @, n]).
entry('Draxler', [d, r, a, k, s, l, '6']).
entry(dr, [d, r]).
entry(dre, [d, r, e]).
entry('Dreh', [d, r, 'e:']).
entry(drehen, [d, r, 'e:', @, n]).
entry('Drehrestaurant', [d, r, 'e:', r, 'E', s, t, o, r, 'a~:']).
entry(dreht, [d, r, 'e:', t]).
entry(drei, [d, r, aI]).
entry(dreieinhalb, [d, r, aI, aI, n, h, a, l, p]).
entry(dreien, [d, r, aI, @, n]).
entry(dreifache, [d, r, aI, f, a, x, @]).
entry(dreihundert, [d, r, aI, h, 'U', n, d, '6', t]).
entry('Drei-Löwen', [d, r, aI, l, '2:', v, @, n]).
entry(dreimal, [d, r, aI, m, 'a:', l]).
entry(dreiß, [d, r, aI, s]).
entry(dreißi, [d, r, aI, s, 'I']).
entry(dreißig, [d, r, aI, s, 'I', 'C']).
entry(dreißigste, [d, r, aI, s, 'I', 'C', s, t, @]).
entry(dreißigsten, [d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(dreißigster, [d, r, aI, s, 'I', 'C', s, t, '6']).
entry(dreistündigen, [d, r, aI, 'S', t, 'Y', n, d, 'I', g, @, n]).
entry(dreistündiger, [d, r, aI, 'S', t, 'Y', n, d, 'I', g, '6']).
entry('Drei-Tages-Reise', [d, r, aI, t, 'a:', g, @, s, r, aI, z, @]).
entry('Drei-Tage-Treffen', [d, r, aI, t, 'a:', g, @, t, r, 'E', f, @, n]).
entry(dreitägige, [d, r, aI, t, 'E:', g, 'I', g, @]).
entry(dreitägigen, [d, r, aI, t, 'E:', g, 'I', g, @, n]).
entry(dreitägiges, [d, r, aI, t, 'E:', g, 'I', g, @, s]).
entry(dreitäki, [d, r, aI, t, 'E:', k, i]).
entry(dreiundachtzig, [d, r, aI, 'U', n, t, a, x, ts, 'I', 'C']).
entry(dreiund, [d, r, aI, 'U', n, t]).
entry(dreiunddreißig, [d, r, aI, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(dreiunddreißigste, [d, r, aI, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @]).
entry(dreiunddreißigsten, [d, r, aI, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(dreiundfünfzig, [d, r, aI, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(dreiundneunzig, [d, r, aI, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(dreiun, [d, r, aI, 'U', n]).
entry(dreiundsechzig, [d, r, aI, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(dreiundsiebzig, [d, r, aI, 'U', n, t, z, 'i:', p, ts, 'I', 'C']).
entry(dreiundvi, [d, r, aI, 'U', n, t, f, 'I']).
entry(dreiundvierzig, [d, r, aI, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(dreiundvierzigsten, [d, r, aI, 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(dreiundzwanz, [d, r, aI, 'U', n, t, ts, v, a, n, ts]).
entry(dreiundzwanzig, [d, r, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(dreiundzwanziger, [d, r, aI, 'U', n, t, ts, v, a, n, ts, 'I', g, '6']).
entry(dreiundzwanzigs, [d, r, aI, 'U', n, t, t, v, a, n, ts, 'I', 'C', s]).
entry(dreiundzwanzigste, [d, r, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(dreiundzwanzigsten, [d, r, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(dreiundzwanzigster, [d, r, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(dreiviertel, [d, r, aI, f, 'I', '6', t, @, l]).
entry('Dreiviertelstunde', [d, r, aI, f, 'I', '6', t, @, l, 'S', t, 'U', n, d, @]).
entry('Drei-Viertel-Takt', [d, r, aI, f, 'I', '6', t, @, l, t, a, k, t]).
entry(dreiz, [d, r, aI, ts]).
entry(dreize, [d, r, aI, ts, e]).
entry(dreizeh, [d, r, aI, ts, 'e:']).
entry(dreizehn, [d, r, aI, ts, 'e:', n]).
entry(dreizehnte, [d, r, aI, ts, 'e:', n, t, @]).
entry(dreizehnten, [d, r, aI, ts, 'e:', n, t, @, n]).
entry(dreizehnter, [d, r, aI, ts, 'e:', n, t, '6']).
entry('Dresden', [d, r, 'e:', s, d, @, n]).
entry(dri, [d, r, 'I']).
entry(drin, [d, r, 'I', n]).
entry(dringend, [d, r, 'I', 'N', @, n, t]).
entry(dringende, [d, r, 'I', 'N', @, n, d, @]).
entry(dringenden, [d, r, 'I', 'N', @, n, d, @, n]).
entry(dringender, [d, r, 'I', 'N', @, n, d, '6']).
entry(dringendst, [d, r, 'I', 'N', @, n, ts, t]).
entry('Dringlichkeit', [d, r, 'I', 'N', l, 'I', 'C', k, aI, t]).
entry(drinhängen, [d, r, 'I', n, h, 'E', 'N', @, n]).
entry('Drink', [d, r, 'I', 'N', k]).
entry(drinks, [d, r, 'I', 'N', k, s]).
entry(drinliegt, [d, r, 'I', n, l, 'i:', k, t]).
entry(drinnen, [d, r, 'I', n, @, n]).
entry(drinstehen, [d, r, 'I', n, 'S', t, 'e:', @, n]).
entry(drit, [d, r, 'I', t]).
entry(dritte, [d, r, 'I', t, @]).
entry('Drittel', [d, r, 'I', t, @, l]).
entry(dritten, [d, r, 'I', t, @, n]).
entry(dritter, [d, r, 'I', t, '6']).
entry(drittes, [d, r, 'I', t, @, s]).
entry(droben, [d, r, 'o:', b, @, n]).
entry(drüber, [d, r, 'y:', b, '6']).
entry(drucke, [d, r, 'U', k, @]).
entry(drücke, [d, r, 'Y', k, @]).
entry(drücken, [d, r, 'Y', k, @, n]).
entry(drufs, [d, r, 'U', f, s]).
entry(drum, [d, r, 'U', m]).
entry(drumherum, [d, r, 'U', m, h, 'E', r, 'U', m]).
entry(drumrum, [d, r, 'U', m, r, 'U', m]).
entry('Drum-und-Dran', [d, r, 'U', m, 'U', n, t, d, r, a, n]).
entry(dsann, [ts, a, n]).
entry(ds, [ts]).
entry(d, [t]).
entry('Duderstadt', [d, 'u:', d, '6', 'S', t, a, t]).
entry(du, [d, 'u:']).
entry(dudum, [d, u, d, 'U', m]).
entry(dü, [d, 'Y']).
entry('Duisburg', [d, 'y:', s, b, 'U', '6', k]).
entry('Duis', [d, 'y:', s]).
entry(dumm, [d, 'U', m]).
entry('Dumme', [d, 'U', m, @]).
entry(dummerweise, [d, 'U', m, '6', v, aI, z, @]).
entry('Dummes', [d, 'U', m, @, s]).
entry(dunkel, [d, 'U', 'N', k, @, l]).
entry(durcharbeiten, [d, 'U', '6', 'C', a, r, b, aI, t, @, n]).
entry(durchaus, [d, 'U', '6', 'C', aU, s]).
entry(durchchecken, [d, 'U', '6', 'C', tS, 'E', k, @, n]).
entry('Durchdrehen', [d, 'U', '6', 'C', d, r, 'e:', @, n]).
entry(durch, [d, 'U', '6', 'C']).
entry(durcheinander, [d, 'U', '6', 'C', aI, n, a, n, d, '6']).
entry(durcheinandergekommen, [d, 'U', '6', 'C', aI, n, a, n, d, '6', g, @, k, 'O', m, @, n]).
entry(durcheinandergeraten, [d, 'U', '6', 'C', aI, n, a, n, d, '6', g, @, r, 'a:', t, @, n]).
entry(durchfahren, [d, 'U', '6', 'C', f, 'a:', r, @, n]).
entry(durchfährt, [d, 'U', '6', 'C', f, 'E', '6', t]).
entry(durchforsten, [d, 'U', '6', 'C', f, 'O', '6', s, t, @, n]).
entry(durchführe, [d, 'U', '6', 'C', f, 'y:', r, @]).
entry(durchführen, [d, 'U', '6', 'C', f, 'y:', r, @, n]).
entry(durchgängig, [d, 'U', '6', 'C', g, 'E', 'N', 'I', 'C']).
entry(durchgeben, [d, 'U', '6', 'C', g, 'e:', b, @, n]).
entry(durchgedrungen, [d, 'U', '6', 'C', g, @, d, r, 'U', 'N', @, n]).
entry(durchgedüst, [d, 'U', '6', 'C', g, @, d, 'y:', s, t]).
entry(durchgehend, [d, 'U', '6', 'C', g, 'e:', @, n, t]).
entry(durchgehen, [d, 'U', '6', 'C', g, 'e:', @, n]).
entry(durchgelaufen, [d, 'U', '6', 'C', g, @, l, aU, f, @, n]).
entry(durchgesagt, [d, 'U', '6', 'C', g, @, z, 'a:', k, t]).
entry(durchgesehen, [d, 'U', '6', 'C', g, @, z, 'e:', @, n]).
entry(durchgestrichen, [d, 'U', '6', 'C', g, @, 'S', t, r, 'I', 'C', @, n]).
entry(durchgucken, [d, 'U', '6', 'C', g, 'U', k, @, n]).
entry(durchhaben, [d, 'U', '6', 'C', h, 'a:', b, @, n]).
entry('Durchhänger', [d, 'U', '6', 'C', h, 'E', 'N', '6']).
entry(durchkomme, [d, 'U', '6', 'C', k, 'O', m, @]).
entry(durchkommen, [d, 'U', '6', 'C', k, 'O', m, @, n]).
entry(durchkriegen, [d, 'U', '6', 'C', k, r, 'i:', g, @, n]).
entry(durchmachen, [d, 'U', '6', 'C', m, a, x, @, n]).
entry(durchprobiert, [d, 'U', '6', 'C', p, r, o, b, 'i:', '6', t]).
entry(durchrechnen, [d, 'U', '6', 'C', r, 'E', 'C', n, @, n]).
entry(durchrufen, [d, 'U', '6', 'C', r, 'u:', f, @, n]).
entry(durchsagen, [d, 'U', '6', 'C', z, 'a:', g, @, n]).
entry(durchschauen, [d, 'U', '6', 'C', 'S', aU, @, n]).
entry(durchschlendert, [d, 'U', '6', 'C', 'S', l, 'E', n, d, '6', t]).
entry(durchsehen, [d, 'U', '6', 'C', z, 'e:', @, n]).
entry(durchsprechen, [d, 'U', '6', 'C', 'S', p, r, 'E', 'C', @, n]).
entry(durchstarten, [d, 'U', '6', 'C', 'S', t, a, r, t, @, n]).
entry(durchwachsen, [d, 'U', '6', 'C', v, a, k, s, @, n]).
entry(durchziehen, [d, 'U', '6', 'C', ts, 'i:', @, n]).
entry(durchzieht, [d, 'U', '6', 'C', ts, 'i:', t]).
entry(durchzuführen, [d, 'U', '6', 'C', ts, u, f, 'y:', r, @, n]).
entry(durchzugehen, [d, 'U', '6', 'C', ts, u, g, 'e:', @, n]).
entry(durchzusch, [d, 'U', '6', 'C', ts, 'U', 'S']).
entry(durchzusprechen, [d, 'U', '6', 'C', ts, u, 'S', p, r, 'E', 'C', @, n]).
entry(durchzuziehen, [d, 'U', '6', 'C', ts, u, ts, 'i:', @, n]).
entry(dürfen, [d, 'Y', '6', f, @, n]).
entry(dürfte, [d, 'Y', '6', f, t, @]).
entry(dürften, [d, 'Y', '6', f, t, @, n]).
entry('Dürrenmatt', [d, 'Y', r, @, n, m, a, t]).
entry('Dusche', [d, 'u:', 'S', @]).
entry(düsen, [d, 'y:', z, @, n]).
entry('Düsseldorf', [d, 'Y', s, @, l, d, 'O', '6', f]).
entry('Dütan', [d, 'y:', t, a, n]).
entry(easy, ['i:', z, i]).
entry(ebenfalls, ['e:', b, @, n, f, a, l, s]).
entry(eben, ['e:', b, @, n]).
entry(ebenso, ['e:', b, @, n, z, 'o:']).
entry(ebensowenig, ['e:', b, @, n, z, 'o:', v, 'e:', n, 'I', 'C']).
entry('Eberle', ['e:', b, '6', l, @]).
entry('Eble', ['e:', b, l, @]).
entry('Ebrach', ['e:', b, r, a, x]).
entry(echte, ['E', 'C', t, @]).
entry(echter, ['E', 'C', t, '6']).
entry(echt, ['E', 'C', t]).
entry('Ecken', ['E', k, @, n]).
entry('Ecke', ['E', k, @]).
entry('Eck', ['E', k]).
entry('Economy-Class', ['I', k, 'O', n, @, m, i, k, l, 'a:', s]).
entry(edel, ['e:', d, @, l]).
entry('Eden', ['e:', d, @, n]).
entry(ee, ['e:']).
entry(effektiv, ['E', f, 'E', k, t, 'i:', f]).
entry(effektivsten, ['E', f, 'E', k, t, 'i:', f, s, t, @, n]).
entry(ef, ['E', f]).
entry(egal, [e, g, 'a:', l]).
entry('Eggers', ['E', g, '6', s]).
entry(egoistisch, [e, g, o, 'I', s, t, 'I', 'S']).
entry(eg, ['e:', k]).
entry('Ehefrauen', ['e:', @, f, r, aU, @, n]).
entry(ehemalige, ['e:', @, m, 'a:', l, 'I', g, @]).
entry(ehemaliger, ['e:', @, m, 'a:', l, 'I', g, '6']).
entry('Ehemann', ['e:', @, m, a, n]).
entry(ehe, ['e:', @]).
entry(eher, ['e:', '6']).
entry(ehesten, ['e:', @, s, t, @, n]).
entry(ehnte, ['e:', n, t, @]).
entry('Ehre', ['e:', r, @]).
entry(ehrlich, ['e:', '6', l, 'I', 'C']).
entry(ehrt, ['e:', '6', t]).
entry('Eibl', [aI, b, l]).
entry('Eich', [aI, 'C']).
entry(eicht, [aI, 'C', t]).
entry(eiene, [aI, @, n, @]).
entry(eigenartig, [aI, g, @, n, 'a:', r, t, 'I', 'C']).
entry(eigenem, [aI, g, @, n, @, m]).
entry(eigenen, [aI, g, @, n, @, n]).
entry(eigene, [aI, g, @, n, @]).
entry(eigener, [aI, g, @, n, '6']).
entry(eigenes, [aI, g, @, n, @, s]).
entry(eigen, [aI, g, @, n]).
entry('Eigenschaften', [aI, g, @, n, 'S', a, f, t, @, n]).
entry(eigentlichen, [aI, g, @, n, t, l, 'I', 'C', @, n]).
entry(eigentliche, [aI, g, @, n, t, l, 'I', 'C', @]).
entry(eigentlich, [aI, g, @, n, t, l, 'I', 'C']).
entry(eignen, [aI, g, n, @, n]).
entry(eignet, [aI, g, n, @, t]).
entry('Eignung', [aI, g, n, 'U', 'N']).
entry('Eilenriede', [aI, l, @, n, r, 'i:', d, @]).
entry('Eile', [aI, l, @]).
entry(eilig, [aI, l, 'I', 'C']).
entry(eilt, [aI, l, t]).
entry(einander, [aI, n, a, n, d, '6']).
entry(einbaren, [aI, n, b, 'a:', r, @, n]).
entry(einbauen, [aI, n, b, aU, @, n]).
entry(einberechnen, [aI, n, b, @, r, 'E', 'C', n, @, n]).
entry(einberufen, [aI, n, b, @, r, 'u:', f, @, n]).
entry(einbeziehen, [aI, n, b, @, ts, 'i:', @, n]).
entry('Einbezug', [aI, n, b, @, ts, 'u:', k]).
entry(einbringen, [aI, n, b, r, 'I', 'N', @, n]).
entry(einbuchen, [aI, n, b, 'u:', x, @, n]).
entry(einchecken, [aI, n, tS, 'E', k, @, n]).
entry(eindeutig, [aI, n, d, 'OY', t, 'I', 'C']).
entry('Eindrücken', [aI, n, d, r, 'Y', k, @, n]).
entry('Eindrücke', [aI, n, d, r, 'Y', k, @]).
entry('Eindruck', [aI, n, d, r, 'U', k]).
entry(eineinhalb, [aI, n, aI, n, h, a, l, p]).
entry(eineinhalbs, [aI, n, aI, n, h, a, l, p, s]).
entry(eineinhalbtägigen, [aI, n, aI, n, h, a, l, p, t, 'E:', g, 'I', g, @, n]).
entry(eineinhalbtägige, [aI, n, aI, n, h, a, l, p, t, 'E:', g, 'I', g, @]).
entry(eineinhalbtägiges, [aI, n, aI, n, h, a, l, p, t, 'E:', g, 'I', g, @, s]).
entry(eineinhalbtägi, [aI, n, aI, n, h, a, l, p, t, 'E:', g, 'I']).
entry(einem, [aI, n, @, m]).
entry(einen, [aI, n, @, n]).
entry(eine, [aI, n, @]).
entry(einerlei, [aI, n, '6', l, aI]).
entry(einer, [aI, n, '6']).
entry(einerseits, [aI, n, '6', z, aI, ts]).
entry(einerthalt, [aI, n, '6', t, h, a, l, t]).
entry(eines, [aI, n, @, s]).
entry(einfachen, [aI, n, f, a, x, @, n]).
entry(einfache, [aI, n, f, a, x, @]).
entry(einfacheres, [aI, n, f, a, x, @, r, @, s]).
entry(einfacher, [aI, n, f, a, x, '6']).
entry(einfaches, [aI, n, f, a, x, @, s]).
entry(einfach, [aI, n, f, a, x]).
entry(einfachsten, [aI, n, f, a, x, s, t, @, n]).
entry(einfachste, [aI, n, f, a, x, s, t, @]).
entry(einfallen, [aI, n, f, a, l, @, n]).
entry(einfällt, [aI, n, f, 'E', l, t]).
entry(einfiger, [aI, n, f, 'I', g, '6']).
entry(einf, [aI, n, f]).
entry('Eingang', [aI, n, g, a, 'N']).
entry('Eingangshalle', [aI, n, g, a, 'N', s, h, a, l, @]).
entry(eingebaut, [aI, n, g, @, b, aU, t]).
entry(eingebunden, [aI, n, g, @, b, 'U', n, d, @, n]).
entry(eingecheckt, [aI, n, g, @, tS, 'E', k, t]).
entry(eingedeckt, [aI, n, g, @, d, 'E', k, t]).
entry(eingefallen, [aI, n, g, @, f, a, l, @, n]).
entry(eingefleischte, [aI, n, g, @, f, l, aI, 'S', t, @]).
entry(eingehender, [aI, n, g, 'e:', @, n, d, '6']).
entry(eingehend, [aI, n, g, 'e:', @, n, t]).
entry(eingehen, [aI, n, g, 'e:', @, n]).
entry(eingeholt, [aI, n, g, @, h, 'o:', l, t]).
entry(eingeladen, [aI, n, g, @, l, 'a:', d, @, n]).
entry(eingeplant, [aI, n, g, @, p, l, 'a:', n, t]).
entry(eingeräumt, [aI, n, g, @, r, 'OY', m, t]).
entry(eingereicht, [aI, n, g, @, r, aI, 'C', t]).
entry(eingeschaltet, [aI, n, g, @, 'S', a, l, t, @, t]).
entry(eingeschlichen, [aI, n, g, @, 'S', l, 'I', 'C', @, n]).
entry(eingeschlossen, [aI, n, g, @, 'S', l, 'O', s, @, n]).
entry(eingeschränkt, [aI, n, g, @, 'S', r, 'E', 'N', k, t]).
entry(eingeschrieben, [aI, n, g, @, 'S', r, 'i:', b, @, n]).
entry(eingespannt, [aI, n, g, @, 'S', p, a, n, t]).
entry(eingestuft, [aI, n, g, @, 'S', t, 'u:', f, t]).
entry(eingeteilt, [aI, n, g, @, t, aI, l, t]).
entry(eingetragen, [aI, n, g, @, t, r, 'a:', g, @, n]).
entry(eingewöhnen, [aI, n, g, @, v, '2:', n, @, n]).
entry(eingezogen, [aI, n, g, @, ts, 'o:', g, @, n]).
entry(eingezwängt, [aI, n, g, @, ts, v, 'E', 'N', t]).
entry(einhalb, [aI, n, h, a, l, p]).
entry(einhalten, [aI, n, h, a, l, t, @, n]).
entry('Einheit', [aI, n, h, aI, t]).
entry(einhundert, [aI, n, h, 'U', n, d, '6', t]).
entry(einigen, [aI, n, 'I', g, @, n]).
entry(einige, [aI, n, 'I', g, @]).
entry(einigermaßen, [aI, n, 'I', g, '6', m, 'a:', s, @, n]).
entry(einiger, [aI, n, 'I', g, '6']).
entry(einiges, [aI, n, 'I', g, @, s]).
entry(einig, [aI, n, 'I', 'C']).
entry('Einigung', [aI, n, 'I', g, 'U', 'N']).
entry(einkalkulieren, [aI, n, k, a, l, k, u, l, 'i:', r, @, n]).
entry(einkaufen, [aI, n, k, aU, f, @, n]).
entry('Einkäufe', [aI, n, k, 'OY', f, @]).
entry('Einkaufsmöglichkeiten', [aI, n, k, aU, f, s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Einkaufsstraßen', [aI, n, k, aU, f, s, 'S', t, r, 'a:', s, @, n]).
entry('Einkaufszentrum', [aI, n, k, aU, f, s, ts, 'E', n, t, r, 'U', m]).
entry(einkehren, [aI, n, k, 'e:', r, @, n]).
entry('Einklang', [aI, n, k, l, a, 'N']).
entry(einladen, [aI, n, l, 'a:', d, @, n]).
entry('Einladung', [aI, n, l, 'a:', d, 'U', 'N']).
entry(einlassen, [aI, n, l, a, s, @, n]).
entry(einlaufen, [aI, n, l, aU, f, @, n]).
entry(einlegen, [aI, n, l, 'e:', g, @, n]).
entry(einmalige, [aI, n, m, 'a:', l, 'I', g, @]).
entry(einmal, [aI, n, m, 'a:', l]).
entry(einmonatsweise, [aI, n, m, 'o:', n, a, ts, v, aI, z, @]).
entry(einnehmen, [aI, n, n, 'e:', m, @, n]).
entry(einpacken, [aI, n, p, a, k, @, n]).
entry(einplanen, [aI, n, p, l, 'a:', n, @, n]).
entry(ein, [aI, n]).
entry(einquartieren, [aI, n, k, v, a, r, t, 'i:', r, @, n]).
entry(einräumen, [aI, n, r, 'OY', m, @, n]).
entry(einrechnen, [aI, n, r, 'E', 'C', n, @, n]).
entry('Einreichungsschluß', [aI, n, r, aI, 'C', 'U', 'N', s, 'S', l, 'U', s]).
entry(einrichten, [aI, n, r, 'I', 'C', t, @, n]).
entry(einsatzbereit, [aI, n, z, a, ts, b, @, r, aI, t]).
entry(einsatzfähig, [aI, n, z, a, ts, f, 'E:', 'I', 'C']).
entry(einschieben, [aI, n, 'S', 'i:', b, @, n]).
entry(einschließen, [aI, n, 'S', l, 'i:', s, @, n]).
entry(einschließlich, [aI, n, 'S', l, 'i:', s, l, 'I', 'C']).
entry('Einschnitten', [aI, n, 'S', n, 'I', t, @, n]).
entry(einschränken, [aI, n, 'S', r, 'E', 'N', k, @, n]).
entry('Einschränkungen', [aI, n, 'S', r, 'E', 'N', k, 'U', 'N', @, n]).
entry('Einschränkung', [aI, n, 'S', r, 'E', 'N', k, 'U', 'N']).
entry(eins, [aI, n, s]).
entry(einsteigen, [aI, n, 'S', t, aI, g, @, n]).
entry(einstellen, [aI, n, 'S', t, 'E', l, @, n]).
entry('Einstellung', [aI, n, 'S', t, 'E', l, 'U', 'N']).
entry('Einstimmung', [aI, n, 'S', t, 'I', m, 'U', 'N']).
entry('Ein-Stunden-Takt', [aI, n, 'S', t, 'U', n, d, @, n, t, a, k, t]).
entry(einstweilen, [aI, n, s, t, v, aI, l, @, n]).
entry(eintägigen, [aI, n, t, 'E:', g, 'I', g, @, n]).
entry(eintägige, [aI, n, t, 'E:', g, 'I', g, @]).
entry(eintägiger, [aI, n, t, 'E:', g, 'I', g, '6']).
entry(eintägiges, [aI, n, t, 'E:', g, 'I', g, @, s]).
entry(eintägig, [aI, n, t, 'E:', g, 'I', 'C']).
entry(einteigigen, [aI, n, t, aI, g, 'I', g, @, n]).
entry(einteilen, [aI, n, t, aI, l, @, n]).
entry(einträfen, [aI, n, t, r, 'E:', f, @, n]).
entry(eintragen, [aI, n, t, r, 'a:', g, @, n]).
entry('Einträge', [aI, n, t, r, 'E:', g, @]).
entry('Eintrag', [aI, n, t, r, 'a:', k]).
entry(einträgt, [aI, n, t, r, 'E:', k, t]).
entry('Eintragungen', [aI, n, t, r, 'a:', g, 'U', 'N', @, n]).
entry('Eintragung', [aI, n, t, r, 'a:', g, 'U', 'N']).
entry(eintreffen, [aI, n, t, r, 'E', f, @, n]).
entry(eintrifft, [aI, n, t, r, 'I', f, t]).
entry('Eintrittskarten', [aI, n, t, r, 'I', ts, k, a, r, t, @, n]).
entry('Eintrittskarte', [aI, n, t, r, 'I', ts, k, a, r, t, @]).
entry(einunddreißig, [aI, n, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(einunddreißigs, [aI, n, 'U', n, t, d, r, aI, s, 'I', 'C', s]).
entry(einunddreißigsten, [aI, n, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(einunddreißigste, [aI, n, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @]).
entry(einunddreißigster, [aI, n, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, '6']).
entry(einunddr, [aI, n, 'U', n, t, d, r]).
entry(einundfünfzig, [aI, n, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(einundfünfzigste, [aI, n, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C', s, t, @]).
entry(einund, [aI, n, 'U', n, t]).
entry(einundsechzig, [aI, n, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(einundvierzig, [aI, n, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(einundzwanzig, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(einundzwanzigs, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s]).
entry(einundzwanzigstem, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, m]).
entry(einundzwanzigsten, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(einundzwanzigste, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(einundzwanzigster, [aI, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(einundzwanz, [aI, n, 'U', n, t, ts, v, a, n, ts]).
entry(einverstanden, [aI, n, f, 'E', '6', 'S', t, a, n, d, @, n]).
entry(einverstan, [aI, n, f, 'E', '6', 'S', t, a, n]).
entry(einversta, [aI, n, f, 'E', '6', 'S', t, a]).
entry('Einwände', [aI, n, v, 'E', n, d, @]).
entry(einwandfrei, [aI, n, v, a, n, t, f, r, aI]).
entry(einwenden, [aI, n, v, 'E', n, d, @, n]).
entry(einwerfen, [aI, n, v, 'E', '6', f, @, n]).
entry(einwer, [aI, n, v, 'E', '6']).
entry('Ein-Wochen-Abständen', [aI, n, v, 'O', x, @, n, a, p, 'S', t, 'E', n, d, @, n]).
entry('Ein-Wochen-Kurs', [aI, n, v, 'O', x, @, n, k, 'U', '6', s]).
entry('Ein-Wochen-Rhythmus', [aI, n, v, 'O', x, @, n, r, 'Y', t, m, 'U', s]).
entry(einwöchigen, [aI, n, v, '9', 'C', 'I', g, @, n]).
entry(einwöchige, [aI, n, v, '9', 'C', 'I', g, @]).
entry(einwöchig, [aI, n, v, '9', 'C', 'I', 'C']).
entry('Einzelblöcke', [aI, n, ts, @, l, b, l, '9', k, @]).
entry('Einzelheiten', [aI, n, ts, @, l, h, aI, t, @, n]).
entry(einzelnen, [aI, n, ts, @, l, n, @, n]).
entry(einzelne, [aI, n, ts, @, l, n, @]).
entry(einzeln, [aI, n, ts, @, l, n]).
entry('Einzel', [aI, n, ts, @, l]).
entry('Einzeltagen', [aI, n, ts, @, l, t, 'a:', g, @, n]).
entry('Einzeltermine', [aI, n, ts, @, l, t, 'E', '6', m, 'i:', n, @]).
entry('Einzelveranstaltungen', [aI, n, ts, @, l, f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', @, n]).
entry('Einzelzimmern', [aI, n, ts, @, l, ts, 'I', m, '6', n]).
entry('Einzelzimmerpreis', [aI, n, ts, 'E', l, ts, 'I', m, '6', p, r, aI, s]).
entry('Einzelzimmer', [aI, n, ts, @, l, ts, 'I', m, '6']).
entry('Einzelzimmers', [aI, n, ts, @, l, ts, 'I', m, '6', s]).
entry('Einzelzimmerzuschlag', [aI, n, ts, @, l, ts, 'I', m, '6', ts, 'u:', 'S', l, 'a:', k]).
entry(einziehen, [aI, n, ts, 'i:', @, n]).
entry(einzigen, [aI, n, ts, 'I', g, @, n]).
entry(einzige, [aI, n, ts, 'I', g, @]).
entry(einziger, [aI, n, ts, 'I', g, '6']).
entry(einziges, [aI, n, ts, 'I', g, @, s]).
entry(einzig, [aI, n, ts, 'I', 'C']).
entry(einzigste, [aI, n, ts, 'I', 'C', s, t, @]).
entry(einz, [aI, n, ts]).
entry(einzubauen, [aI, n, ts, u, b, aU, @, n]).
entry(einzubeziehen, [aI, n, ts, u, b, @, ts, 'i:', @, n]).
entry(einzuchecken, [aI, n, ts, u, tS, 'E', k, @, n]).
entry(einzufangen, [aI, n, ts, u, f, a, 'N', @, n]).
entry(einzugrenzen, [aI, n, ts, u, g, r, 'E', n, ts, @, n]).
entry(einzuhalten, [aI, n, ts, u, h, a, l, t, @, n]).
entry(einzukaufen, [aI, n, ts, u, k, aU, f, @, n]).
entry(einzunehmen, [aI, n, ts, u, n, 'e:', m, @, n]).
entry(einzuplanen, [aI, n, ts, u, p, l, 'a:', n, @, n]).
entry(einzuquartieren, [aI, n, ts, u, k, v, a, r, t, 'i:', r, @, n]).
entry(einzurechnen, [aI, n, ts, u, r, 'E', 'C', n, @, n]).
entry(einzurichten, [aI, n, ts, u, r, 'I', 'C', t, @, n]).
entry(einzuschieben, [aI, n, ts, u, 'S', 'i:', b, @, n]).
entry(einzuschließen, [aI, n, ts, u, 'S', l, 'i:', s, @, n]).
entry(einzuwenden, [aI, n, ts, u, v, 'E', n, d, @, n]).
entry(ei, [aI]).
entry('Eisenbahn', [aI, z, @, n, b, 'a:', n]).
entry('Eisen', [aI, z, @, n]).
entry('Eisler', [aI, s, l, '6']).
entry(eitag, [aI, t, 'a:', k]).
entry(eit, [aI, t]).
entry(eiten, [aI, t, @, n]).
entry(eiundzwanzigsten, [aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry('Eize', [aI, ts, @]).
entry('Eiz', [aI, ts]).
entry(elbstverständlich, ['E', l, p, s, t, f, 'E', '6', 'S', t, 'E', n, t, l, 'I', 'C']).
entry(elegantere, [e, l, e, g, a, n, t, @, r, @]).
entry(eleganter, [e, l, e, g, a, n, t, '6']).
entry(elegant, [e, l, e, g, a, n, t]).
entry('Elektronika', [e, l, 'E', k, t, r, 'o:', n, i, k, a]).
entry(elf, ['E', l, f]).
entry(elften, ['E', l, f, t, @, n]).
entry(elfte, ['E', l, f, t, @]).
entry(elfter, ['E', l, f, t, '6']).
entry('Elf-Uhr-Fünfzehn-Affinität', ['E', l, f, 'u:', '6', f, 'Y', n, f, ts, 'e:', n, a, f, i, n, i, t, 'E:', t]).
entry('Elitäres', [e, l, i, t, 'E:', r, @, s]).
entry('Elke', ['E', l, k, @]).
entry('Elsen', ['E', l, z, @, n]).
entry('Elternabend', ['E', l, t, '6', n, 'a:', b, @, n, t]).
entry('Eltern', ['E', l, t, '6', n]).
entry('Elternsprechtag', ['E', l, t, '6', n, 'S', p, r, 'E', 'C', t, 'a:', k]).
entry('E-Mail', ['i:', m, 'e:', l]).
entry('Emig', ['E', m, 'I', 'C']).
entry('Emil', ['e:', m, 'i:', l]).
entry(empfangen, ['E', m, pf, a, 'N', @, n]).
entry(empfehlen, ['E', m, pf, 'e:', l, @, n]).
entry(empfehlenswert, ['E', m, pf, 'e:', l, @, n, s, v, 'e:', '6', t]).
entry(empfehle, ['E', m, pf, 'e:', l, @]).
entry('Empfehlungen', ['E', m, pf, 'e:', l, 'U', 'N', @, n]).
entry('Empfehlung', ['E', m, pf, 'e:', l, 'U', 'N']).
entry(empfohlen, ['E', m, pf, 'o:', l, @, n]).
entry('Endeffekt', ['E', n, t, 'E', f, 'E', k, t]).
entry(enden, ['E', n, d, @, n]).
entry(ende, ['E', n, d, @]).
entry(endet, ['E', n, d, @, t]).
entry(endgültig, ['E', n, t, g, 'Y', l, t, 'I', 'C']).
entry(endlich, ['E', n, t, l, 'I', 'C']).
entry(endlos, ['E', n, t, l, 'o:', s]).
entry(end, ['E', n, t]).
entry(endseminar, ['E', n, t, z, e, m, 'I', n, 'a:', r]).
entry('Energien', [e, n, 'E', '6', g, 'i:', @, n]).
entry(enerw, ['e:', n, 'E', '6', v]).
entry(engagierte, ['a~', g, a, 'Z', 'i:', '6', t, @]).
entry(engagiert, ['a~', g, a, 'Z', 'i:', '6', t]).
entry(engen, ['E', 'N', @, n]).
entry(engere, ['E', 'N', @, r, @]).
entry(enger, ['E', 'N', '6']).
entry('Englisch', ['E', 'N', l, 'I', 'S']).
entry(eng, ['E', 'N']).
entry(engsten, ['E', 'N', s, t, @, n]).
entry(enke, ['E', 'N', k, @]).
entry(enorme, [e, n, 'O', '6', m, @]).
entry(enormer, [e, n, 'O', '6', m, '6']).
entry(entbehren, ['E', n, t, b, 'e:', r, @, n]).
entry(entbehrlich, ['E', n, t, b, 'e:', '6', l, 'I', 'C']).
entry(entbinden, ['E', n, t, b, 'I', n, d, @, n]).
entry(entdeckt, ['E', n, t, d, 'E', k, t]).
entry(enten, ['E', n, t, @, n]).
entry(entfällt, ['E', n, t, f, 'E', l, t]).
entry(entfernen, ['E', n, t, f, 'E', '6', n, @, n]).
entry(entfernt, ['E', n, t, f, 'E', '6', n, t]).
entry('Entfernung', ['E', n, t, f, 'E', '6', n, 'U', 'N']).
entry(entfliehen, ['E', n, t, f, l, 'i:', @, n]).
entry(entgegenkommend, ['E', n, t, g, 'e:', g, @, n, k, 'O', m, @, n, t]).
entry(entgegenkommen, ['E', n, t, g, 'e:', g, @, n, k, 'O', m, @, n]).
entry(entgegennehmen, ['E', n, t, g, 'e:', g, @, n, n, 'e:', m, @, n]).
entry(entgegen, ['E', n, t, g, 'e:', g, @, n]).
entry(enthalten, ['E', n, t, h, a, l, t, @, n]).
entry(entlassen, ['E', n, t, l, a, s, @, n]).
entry(entnehmen, ['E', n, t, n, 'e:', m, @, n]).
entry(entnehme, ['E', n, t, n, 'e:', m, @]).
entry(entnommen, ['E', n, t, n, 'O', m, @, n]).
entry(entscheidend, ['E', n, tS, aI, d, @, n, t]).
entry(entscheiden, ['E', n, tS, aI, d, @, n]).
entry(entscheide, ['E', n, tS, aI, d, @]).
entry('Entscheidung', ['E', n, tS, aI, d, 'U', 'N']).
entry(entschieden, ['E', n, tS, 'i:', d, @, n]).
entry(entschlossen, ['E', n, tS, l, 'O', s, @, n]).
entry(entsch, ['E', n, tS]).
entry(entschuldigen, ['E', n, tS, 'U', l, d, 'I', g, @, n]).
entry('Entschuldigung', ['E', n, tS, 'U', l, d, 'I', g, 'U', 'N']).
entry('Entschu', ['E', n, tS, 'U']).
entry(entsetzlich, ['E', n, t, z, 'E', ts, l, 'I', 'C']).
entry(entsetzt, ['E', n, t, z, 'E', ts, t]).
entry(entsinne, ['E', n, t, z, 'I', n, @]).
entry(entspannend, ['E', n, tS, p, a, n, n, @, n, t]).
entry(entspannen, ['E', n, tS, p, a, n, @, n]).
entry(entspannter, ['E', n, tS, p, a, n, t, '6']).
entry(entspannt, ['E', n, tS, p, a, n, t]).
entry('Entspannung', ['E', n, tS, p, a, n, 'U', 'N']).
entry(entsprechenden, ['E', n, tS, p, r, 'E', 'C', @, n, d, @, n]).
entry(entsprechende, ['E', n, tS, p, r, 'E', 'C', @, n, d, @]).
entry(entsprechendes, ['E', n, tS, p, r, 'E', 'C', @, n, t]).
entry(entsprechen, ['E', n, tS, p, r, 'E', 'C', @, n]).
entry(entspricht, ['E', n, tS, p, r, 'I', 'C', t]).
entry(enttäuschen, ['E', n, t, t, 'OY', 'S', @, n]).
entry(enttäuscht, ['E', n, t, t, 'OY', 'S', t]).
entry(entweder, ['E', n, t, v, 'e:', d, '6']).
entry(entwe, ['E', n, t, v, 'e:']).
entry(entwickeln, ['E', n, t, v, 'I', k, @, l, n]).
entry(entwickelt, ['E', n, t, v, 'I', k, @, l, t]).
entry(entzückt, ['E', n, t, ts, 'Y', k, t]).
entry('Equipment', ['E', k, v, 'I', p, m, 'E', n, t]).
entry('Erachtens', ['E', '6', a, x, t, @, n, s]).
entry(erachte, ['E', '6', a, x, t, @]).
entry(erarbeiten, ['E', '6', a, r, b, aI, t, @, n]).
entry(erarbeitet, ['E', '6', a, r, b, aI, t, @, t]).
entry(erbar, ['E', '6', b, 'a:', r]).
entry('Erdgeschoß', ['e:', '6', t, g, @, 'S', 'O', s]).
entry('Erding', ['E', '6', d, 'I', 'N']).
entry('Ereignis', ['E', '6', aI, g, n, 'I', s]).
entry('Ereignisse', ['E', '6', aI, g, n, 'I', s, @]).
entry(erfahren, ['E', '6', f, 'a:', r, @, n]).
entry('Erfahrungen', ['E', '6', f, 'a:', r, 'U', 'N', @, n]).
entry('Erfahrung', ['E', '6', f, 'a:', r, 'U', 'N']).
entry(erfahrungsgemäß, ['E', '6', f, 'a:', r, 'U', 'N', s, g, @, m, 'E:', s]).
entry('Erfolge', ['E', '6', f, 'O', l, g, @]).
entry('Erfolg', ['E', '6', f, 'O', l, k]).
entry(erfolgreichem, ['E', '6', f, 'O', l, k, r, aI, 'C', @, m]).
entry(erfolgreichen, ['E', r, f, 'O', l, k, r, aI, 'C', @, n]).
entry(erfolgreiche, ['E', '6', f, 'O', l, k, r, aI, 'C', @]).
entry(erfolgreiches, ['E', '6', f, 'O', l, k, r, aI, 'C', @, s]).
entry(erfolgreich, ['E', '6', f, 'O', l, k, r, aI, 'C']).
entry(erfolgversprechender, ['E', r, f, 'O', l, k, f, 'E', '6', 'S', p, r, 'E', 'C', @, n, d, '6']).
entry(erforderlich, ['E', '6', f, 'O', '6', d, '6', l, 'I', 'C']).
entry(erfreulichen, ['E', '6', f, r, 'OY', l, 'I', 'C', @, n]).
entry(erfreulich, ['E', r, f, r, 'OY', l, 'I', 'C']).
entry(erfreut, ['E', '6', f, r, 'OY', t]).
entry(erfüllen, ['E', '6', f, 'Y', l, @, n]).
entry(erfüllt, ['E', '6', f, 'Y', l, t]).
entry('Erfurt', ['E', '6', f, 'U', '6', t]).
entry(ergänzen, ['E', '6', g, 'E', n, ts, @, n]).
entry('Ergänzung', ['E', '6', g, 'E', n, ts, 'U', 'N']).
entry(ergeben, ['E', '6', g, 'e:', b, @, n]).
entry('Ergebnissen', ['E', '6', g, 'e:', p, n, 'I', s, @, n]).
entry('Ergebnisse', ['E', '6', g, 'e:', p, n, 'I', s, @]).
entry(ergibt, ['E', '6', g, 'i:', p, t]).
entry(ergi, ['E', '6', g, i]).
entry(erhalten, ['E', '6', h, a, l, t, @, n]).
entry(erhältlich, ['E', '6', h, 'E', l, t, l, 'I', 'C']).
entry(erheblich, ['E', '6', h, 'e:', p, l, 'I', 'C']).
entry(erholen, ['E', '6', h, 'o:', l, @, n]).
entry(erholt, ['E', '6', h, 'o:', l, t]).
entry('Erholung', ['E', '6', h, 'o:', l, 'U', 'N']).
entry('Erholungspausen', ['E', '6', h, 'o:', l, 'U', 'N', s, p, aU, z, @, n]).
entry(erhören, ['E', '6', h, '2:', r, @, n]).
entry(erinnere, ['E', '6', 'I', n, @, r, @]).
entry(erinnern, ['E', '6', 'I', n, '6', n]).
entry(erinnerst, ['E', '6', 'I', n, '6', s, t]).
entry(erinnert, ['E', '6', 'I', n, '6', t]).
entry('Erinnerung', ['E', '6', 'I', n, '6', r, 'U', 'N']).
entry('Erinnerungsschreiben', ['E', '6', 'I', n, @, r, 'U', 'N', s, 'S', r, aI, b, @, n]).
entry(erkältet, ['E', '6', k, 'E', l, t, @, t]).
entry(erkennen, ['E', '6', k, 'E', n, @, n]).
entry(erkenne, ['E', '6', k, 'E', n, @]).
entry(erklären, ['E', '6', k, l, 'E:', r, @, n]).
entry(erkläre, ['E', '6', k, l, 'E:', r, @]).
entry(erklärt, ['E', '6', k, l, 'E:', '6', t]).
entry(erkundigen, ['E', '6', k, 'U', n, d, 'I', g, @, n]).
entry(erkundige, ['E', '6', k, 'U', n, d, 'I', g, @]).
entry(erkundigt, ['E', '6', k, 'U', n, d, 'I', 'C', t]).
entry('Erkundigungen', ['E', '6', k, 'U', n, d, 'I', g, 'U', 'N', @, n]).
entry(erkund, ['E', '6', k, 'U', n, t]).
entry(erlauben, ['E', '6', l, aU, b, @, n]).
entry(erlaube, ['E', '6', l, aU, b, @]).
entry(erlaubt, ['E', '6', l, aU, p, t]).
entry(erleben, ['E', '6', l, 'e:', b, @, n]).
entry(erledigen, ['E', '6', l, 'e:', d, 'I', g, @, n]).
entry(erledige, ['E', '6', l, 'e:', d, 'I', g, @]).
entry(erledigt, ['E', '6', l, 'e:', d, 'I', 'C', t]).
entry('Erledigungen', ['E', '6', l, 'e:', d, 'I', g, 'U', 'N', @, n]).
entry('Erledung', ['E', '6', l, 'e:', d, 'U', 'N']).
entry(erleichtert, ['E', '6', l, aI, 'C', t, '6', t]).
entry(erle, ['E', '6', l, e]).
entry('Ermäßigungen', ['E', '6', m, 'E:', s, 'I', g, 'U', 'N', @, n]).
entry('Ermäßigung', ['E', '6', m, 'E:', s, 'I', g, 'U', 'N']).
entry(ermöglicht, ['E', '6', m, '2:', k, l, 'I', 'C', t]).
entry(ermüdend, ['E', '6', m, 'y:', d, @, n, t]).
entry(erne, ['E', '6', n, @]).
entry(erneut, ['E', '6', n, 'OY', t]).
entry(ernst, ['E', '6', n, s, t]).
entry(eröffnetes, ['E', '6', '9', f, n, @, t, @, s]).
entry(eröffnet, ['E', '6', '9', f, n, @, t]).
entry('Eröffnung', ['E', '6', '9', f, n, 'U', 'N']).
entry(erörtern, ['E', '6', '9', '6', t, '6', n]).
entry(erplex, ['E', '6', p, l, 'E', k, s]).
entry('Erpril', ['E', '6', p, r, 'I', l]).
entry(erreichbar, ['E', '6', r, aI, 'C', b, 'a:', r]).
entry(erreichen, ['E', '6', r, aI, 'C', @, n]).
entry(erreiche, ['E', '6', r, aI, 'C', @]).
entry(erreicht, ['E', '6', r, aI, 'C', t]).
entry(errei, ['E', '6', r, aI]).
entry(err, ['E', '6', r]).
entry('Ersatzsauna', ['E', '6', z, a, ts, z, aU, n, a]).
entry('Ersatztermin', ['E', '6', z, a, ts, t, 'E', '6', m, 'i:', n]).
entry('Ersatzwoche', ['E', '6', z, a, ts, v, 'O', x, @]).
entry(erscheinen, ['E', '6', 'S', aI, n, @, n]).
entry(erscheine, ['E', '6', 'S', aI, n, @]).
entry(erscheint, ['E', '6', 'S', aI, n, t]).
entry(erschöpft, ['E', '6', 'S', '9', pf, t]).
entry(erschwert, ['E', '6', 'S', v, 'e:', '6', t]).
entry(erschwinglich, ['E', r, 'S', v, 'I', 'N', l, 'I', 'C']).
entry(ersetzt, ['E', '6', z, 'E', ts, t]).
entry(ersichtlich, ['E', '6', z, 'I', 'C', t, l, 'I', 'C']).
entry('Ersparnis', ['E', '6', 'S', p, 'a:', r, n, 'I', s]).
entry(erspart, ['E', '6', 'S', p, 'a:', r, t]).
entry(ers, ['E', '6', s]).
entry(erstar, ['E', '6', 'S', t, a, r]).
entry(erstatten, ['E', '6', 'S', t, a, t, @, n]).
entry(erstaunliche, ['E', '6', 'S', t, aU, n, l, 'I', 'C', @]).
entry(erstaunlicherweise, ['E', '6', 'S', t, aU, n, l, 'I', 'C', '6', v, aI, z, @]).
entry(erstaunlich, ['E', '6', 'S', t, aU, n, l, 'I', 'C']).
entry(erstehen, ['E', '6', 'S', t, 'e:', @, n]).
entry('Erste-Klasse-Fahrschein', ['e:', '6', s, t, @, k, l, a, s, @, f, 'a:', r, 'S', aI, n]).
entry('Erste-Klasse', ['e:', '6', s, t, @, k, l, a, s, @]).
entry('Erste-Klasse-Tickets', ['e:', '6', s, t, @, k, l, a, s, @, t, 'I', k, @, ts]).
entry(erstellen, ['E', '6', 'S', t, 'E', l, @, n]).
entry('Erste-Mai-Feiertag', ['e:', '6', s, t, @, m, aI, f, aI, '6', t, 'a:', k]).
entry(ersten, ['e:', '6', s, t, @, n]).
entry(erstens, ['e:', '6', s, t, @, n, s]).
entry(erste, ['e:', '6', s, t, @]).
entry(erstere, ['e:', '6', s, t, @, r, @]).
entry(erster, ['e:', '6', s, t, '6']).
entry(erstes, ['e:', '6', s, t, @, s]).
entry(erstgenannte, ['e:', '6', s, t, g, @, n, a, n, t, @]).
entry(erstgenannter, ['e:', '6', s, t, g, @, n, a, n, t, '6']).
entry(erst, ['e:', '6', s, t]).
entry(erstrecken, ['E', '6', 'S', t, r, 'E', k, @, n]).
entry(erstreckt, ['E', '6', 'S', t, r, 'E', k, t]).
entry(ersucht, ['E', '6', z, 'U', x, t]).
entry(erteilen, ['E', '6', t, aI, l, @, n]).
entry(erträglich, ['E', '6', t, r, 'E:', k, l, 'I', 'C']).
entry(eruieren, [e, r, u, 'i:', r, @, n]).
entry(ervorragend, ['E', '6', f, 'o:', '6', r, 'a:', g, @, n, t]).
entry(erwähnen, ['E', '6', v, 'E:', n, @, n]).
entry(erwähnte, ['E', '6', v, 'E:', n, t, @]).
entry(erwähnt, ['E', '6', v, 'E:', n, t]).
entry(erwarten, ['E', '6', v, a, r, t, @, n]).
entry(erwarte, ['E', '6', v, a, r, t, @]).
entry(erwartet, ['E', '6', v, a, r, t, @, t]).
entry('Erwartungen', ['E', '6', v, a, r, t, 'U', 'N', @, n]).
entry(erweisen, ['E', '6', v, aI, z, @, n]).
entry(erweiterte, ['E', '6', v, aI, t, '6', t, @]).
entry('Erweiterung', ['E', '6', v, aI, t, @, r, 'U', 'N']).
entry(erwiesen, ['E', '6', v, 'i:', z, @, n]).
entry(erwischen, ['E', '6', v, 'I', 'S', @, n]).
entry(erwische, ['E', '6', v, 'I', 'S', @]).
entry(erwischt, ['E', '6', v, 'I', 'S', t]).
entry(erwünscht, ['E', '6', v, 'Y', n, 'S', t]).
entry(erzählen, ['E', '6', ts, 'E:', l, @, n]).
entry(erzähle, ['E', '6', ts, 'E:', l, @]).
entry(erzählt, ['E', '6', ts, 'E:', l, t]).
entry('Erz', ['E', '6', ts]).
entry('Eschede', ['E', 'S', @, d, @]).
entry('Eschen', ['E', 'S', @, n]).
entry('Espressobar', ['E', s, p, r, 'E', s, o, b, 'a:', r]).
entry('Espressostand', ['E', s, p, r, 'E', s, o, 'S', t, a, n, t]).
entry(es, ['E', s]).
entry('Essengehen', ['E', s, @, n, g, 'e:', @, n]).
entry(essen, ['E', s, @, n]).
entry(essenstechnisch, ['E', s, @, n, s, t, 'E', 'C', n, 'I', 'S']).
entry('Essenszeiten', ['E', s, @, n, s, ts, aI, t, @, n]).
entry(esse, ['E', s, @]).
entry('Esslingen', ['E', s, l, 'I', 'N', @, n]).
entry('Essling', ['E', s, l, 'I', 'N']).
entry('Esther', ['E', s, t, '6']).
entry('Etage', [e, t, 'a:', 'Z', @]).
entry('Etat', [e, t, 'a:']).
entry(etliche, ['E', t, l, 'I', 'C', @]).
entry(etl, ['E', t, l]).
entry(et, ['E', t]).
entry(ets, ['E', ts]).
entry('Ettlingen-Bruchsal', ['E', t, l, 'I', 'N', @, n, b, r, 'U', x, z, 'a:', l]).
entry('Ettlingen', ['E', t, l, 'I', 'N', @, n]).
entry(etwa, ['E', t, v, a]).
entry(etwas, ['E', t, v, a, s]).
entry(etwender, ['E', t, v, 'E', n, d, '6']).
entry(etw, ['E', t, v]).
entry(etzt, ['E', ts, t]).
entry(euch, ['OY', 'C']).
entry('Eugene', [j, 'U', d, 'Z', 'i:', n]).
entry('Eulen', ['OY', l, @, n]).
entry(euphorisch, ['OY', f, 'o:', r, 'I', 'S']).
entry('Euro-Card', ['OY', r, o, k, 'a:', t]).
entry('Europäischen-Hof', ['OY', r, o, p, 'E:', 'I', 'S', @, n, h, 'o:', f]).
entry('Europa', ['OY', r, 'o:', p, a]).
entry('Europa-Universität', ['OY', r, 'o:', p, a, u, n, i, v, 'E', '6', z, i, t, 'E:', t]).
entry('Euro-Preis', ['OY', r, o, p, r, aI, s]).
entry('Euro', ['OY', r, o]).
entry('Euros', [j, 'U', r, 'O', z]).
entry('Eutin', ['OY', t, 'i:', n]).
entry('Evans', ['i:', v, @, n, z]).
entry('Eventualitäten', [e, v, 'E', n, t, u, a, l, i, t, 'E:', t, @, n]).
entry(eventuellen, [e, v, @, n, t, u, 'E', l, @, n]).
entry(eventuelle, [e, v, 'E', n, t, u, 'E', l, @]).
entry(eventuell, [e, v, 'E', n, t, u, 'E', l]).
entry(eventu, [e, v, 'E', n, t, u]).
entry(ev, [e, v]).
entry(ewigen, ['e:', v, 'I', g, @, n]).
entry(ewige, ['e:', v, 'I', g, @]).
entry('Ewigkeiten', ['e:', v, 'I', 'C', k, aI, t, @, n]).
entry('Ewigkeit', ['e:', v, 'I', 'C', k, aI, t]).
entry(ewig, ['e:', v, 'I', 'C']).
entry(exakte, ['E', k, s, a, k, t, @]).
entry(exakt, ['E', k, s, a, k, t]).
entry('Exemplar', ['E', k, s, 'E', m, p, l, 'a:', r]).
entry('Exer', ['E', k, s, '6']).
entry(exkursieren, ['E', k, s, k, 'U', '6', z, 'i:', r, @, n]).
entry('Exkursion', ['E', k, s, k, 'U', '6', z, j, 'o:', n]).
entry('Exl', ['E', k, s, l]).
entry(expandiert, ['E', k, s, p, a, n, d, 'i:', '6', t]).
entry('Experiment', ['E', k, s, p, e, r, i, m, 'E', n, t]).
entry(expliziteren, ['E', k, s, p, l, i, ts, 'i:', t, @, r, @, n]).
entry(explizit, ['E', k, s, p, l, i, ts, 'i:', t]).
entry('Expo-Messe', ['E', k, s, p, o, m, 'E', s, @]).
entry('Expo', ['E', k, s, p, o]).
entry('Expo-Zweitausend', ['E', k, s, p, o, ts, v, aI, t, aU, z, @, n, t]).
entry(exquisite, ['E', k, s, k, v, i, z, 'i:', t, @]).
entry(extra, ['E', k, s, t, r, a]).
entry('Extras', ['E', k, s, t, r, a, s]).
entry('Extremfall', ['E', k, s, t, r, 'e:', m, f, a, l]).
entry(extrem, ['E', k, s, t, r, 'e:', m]).
entry(exzellente, ['E', k, s, ts, 'E', l, 'E', n, t, @]).
entry(exzellent, ['E', k, s, ts, 'E', l, 'E', n, t]).
entry(fabelhaft, [f, 'a:', b, @, l, h, a, f, t]).
entry('Fabrik', [f, a, b, r, 'i:', k]).
entry('Fächerbad', [f, 'E', 'C', '6', b, 'a:', t]).
entry('Fach', [f, a, x]).
entry('Fachgebiet', [f, a, x, g, @, b, 'i:', t]).
entry(fachlich, [f, a, x, l, 'I', 'C']).
entry('Fachtagung', [f, a, x, t, 'a:', g, 'U', 'N']).
entry(fackeln, [f, a, k, @, l, n]).
entry(fa, [f, a]).
entry(fä, [f, 'E']).
entry('Fahrdauer', [f, 'a:', r, d, aU, '6']).
entry(fahre, [f, 'a:', r, @]).
entry(fahren, [f, 'a:', r, @, n]).
entry('Fahrerei', [f, 'a:', r, @, r, aI]).
entry('Fahrer', [f, 'a:', r, '6']).
entry(fahr, [f, 'a:', r]).
entry(fähr, [f, 'E:', '6']).
entry('Fahrgelegenheit', [f, 'a:', r, g, @, l, 'e:', g, @, n, h, aI, t]).
entry('Fahrkarte', [f, 'a:', r, k, 'a:', r, t, @]).
entry(fahrkarten, [f, 'a:', r, k, a, r, t, @, n]).
entry('Fahrkarten', [f, 'a:', r, k, 'a:', r, t, @, n]).
entry('Fahrkosten', [f, 'a:', r, k, 'O', s, t, @, n]).
entry('Fahrkt', [f, 'a:', r, k, t]).
entry('Fahrplanauszug', [f, 'a:', r, p, l, 'a:', n, aU, s, ts, 'u:', k]).
entry('Fahrpläne', [f, 'a:', r, p, l, 'E:', n, @]).
entry('Fahrplänen', [f, 'a:', r, p, l, 'E:', n, @, n]).
entry('Fahrplan', [f, 'a:', r, p, l, 'a:', n]).
entry('Fahrradtour', [f, 'a:', r, r, 'a:', t, t, 'u:', '6']).
entry('Fahrscheine', [f, 'a:', r, 'S', aI, n, @]).
entry('Fahrschein', [f, 'a:', r, 'S', aI, n]).
entry(fährst, [f, 'E:', '6', s, t]).
entry('Fahrstunden', [f, 'a:', r, 'S', t, 'U', n, d, @, n]).
entry('Fahrtakt', [f, 'a:', r, t, a, k, t]).
entry('Fahrtdauer', [f, 'a:', r, t, d, aU, '6']).
entry('Fahrten', [f, 'a:', r, t, @, n]).
entry('fahr-Termin', [f, 'a:', r, t, 'E', '6', m, 'i:', n]).
entry('Fahrt', [f, 'a:', r, t]).
entry(fährt, [f, 'E:', '6', t]).
entry('Fahrtkosten', [f, 'a:', r, t, k, 'O', s, t, @, n]).
entry('Fahrtmöglichkeiten', [f, 'a:', r, t, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Fahrtmöglichkeit', [f, 'a:', r, t, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Fahrtzeit', [f, 'a:', r, t, ts, aI, t]).
entry('Fahrzeit', [f, 'a:', r, ts, aI, t]).
entry('Fahrzeug', [f, 'a:', r, ts, 'OY', k]).
entry('Fakten', [f, a, k, t, @, n]).
entry('Fakultät', [f, a, k, 'U', l, t, 'E:', t]).
entry('Fal', [f, a, l]).
entry(falle, [f, a, l, @]).
entry('Fälle', [f, 'E', l, @]).
entry(fallen, [f, a, l, @, n]).
entry('Fällen', [f, 'E', l, @, n]).
entry(fällig, [f, 'E', l, 'I', 'C']).
entry(falls, [f, a, l, s]).
entry(fällt, [f, 'E', l, t]).
entry(falschen, [f, a, l, 'S', @, n]).
entry('Falsches', [f, a, l, 'S', @, s]).
entry(falsch, [f, a, l, 'S']).
entry('Fam', [f, a, m]).
entry(familiäre, [f, a, m, i, l, j, 'E:', r, @]).
entry(familiären, [f, a, m, 'I', l, j, 'E:', r, @, n]).
entry('Familie', [f, a, m, 'i:', l, j, @]).
entry('Familienfest', [f, a, m, 'i:', l, j, @, n, f, 'E', s, t]).
entry('Familientreffen', [f, a, m, 'i:', l, j, @, n, t, r, 'E', f, @, n]).
entry(fände, [f, 'E', n, d, @]).
entry(fanden, [f, a, n, d, @, n]).
entry(fänden, [f, 'E', n, d, @, n]).
entry(fand, [f, a, n, t]).
entry(fän, [f, 'E', n]).
entry(fange, [f, a, 'N', @]).
entry(fangen, [f, a, 'N', @, n]).
entry(fängt, [f, 'E', 'N', t]).
entry('Fasanengarten', [f, a, z, 'a:', n, @, n, g, a, r, t, @, n]).
entry('Fasching', [f, a, 'S', 'I', 'N']).
entry('Faschingsbeginn', [f, a, 'S', 'I', 'N', s, b, @, g, 'I', n]).
entry('Faschingstagen', [f, a, 'S', 'I', 'N', s, t, 'a:', g, @, n]).
entry('Faschingswoche', [f, a, 'S', 'I', 'N', s, v, 'O', x, @]).
entry('Faschingszeit', [f, a, 'S', 'I', 'N', s, ts, aI, t]).
entry('Faseler', [f, 'a:', z, @, l, '6']).
entry('Fas', [f, a, s]).
entry(fasse, [f, a, s, @]).
entry(fassen, [f, a, s, @, n]).
entry('Fassung', [f, a, s, 'U', 'N']).
entry(fast, [f, a, s, t]).
entry('Fastnacht', [f, a, s, t, n, a, x, t]).
entry(faul, [f, aU, l]).
entry('Faust', [f, aU, s, t]).
entry('Favorit', [f, a, v, o, r, 'i:', t]).
entry(faxe, [f, a, k, s, @]).
entry(faxen, [f, a, k, s, @, n]).
entry('Fax', [f, a, k, s]).
entry('Faxnummer', [f, a, k, s, n, 'U', m, '6']).
entry('FAZ', [f, a, ts]).
entry(fba, [f, b, a]).
entry(fd, [f, t]).
entry('Feb', [f, 'e:', b]).
entry('Februar', [f, 'e:', b, r, u, 'a:', r]).
entry('Februarhälfte', [f, 'e:', b, r, u, 'a:', r, h, 'E', l, f, t, @]).
entry('Februars', [f, 'e:', b, r, u, 'a:', r, s]).
entry('Februartagen', [f, 'e:', b, r, u, 'a:', r, t, 'a:', g, @, n]).
entry('Februartermin', [f, 'e:', b, r, u, 'a:', r, t, 'E', '6', m, 'i:', n]).
entry('Februarwoche', [f, 'e:', b, r, u, 'a:', r, v, 'O', x, @]).
entry('Februarwochenende', [f, 'e:', b, r, u, 'a:', r, v, 'O', x, @, n, 'E', n, d, @]).
entry('Februarwochen', [f, 'e:', b, r, u, 'a:', r, v, 'O', x, @, n]).
entry('Fe', [f, 'e:']).
entry(fehlen, [f, 'e:', l, @, n]).
entry('Fehler', [f, 'e:', l, '6']).
entry(fehlte, [f, 'e:', l, t, @]).
entry(fehlt, [f, 'e:', l, t]).
entry('Feidmann', [f, aI, t, m, a, n]).
entry('Feierabend', [f, aI, '6', 'a:', b, @, n, t]).
entry('Feieraben', [f, aI, '6', 'a:', b, @, n]).
entry(feiere, [f, aI, @, r, @]).
entry('Feier', [f, aI, '6']).
entry(feierlich, [f, aI, '6', l, 'I', 'C']).
entry('Feierlichkeiten', [f, aI, '6', l, 'I', 'C', k, aI, t, @, n]).
entry(feiern, [f, aI, '6', n]).
entry('Feierta', [f, aI, '6', t, 'a:']).
entry('Feiertage', [f, aI, '6', t, 'a:', g, @]).
entry('Feiertagen', [f, aI, '6', t, 'a:', g, @, n]).
entry('Feiertag', [f, aI, '6', t, 'a:', k]).
entry(fei, [f, aI]).
entry(feif, [f, aI, f]).
entry(feine, [f, aI, n, @]).
entry('Feines', [f, aI, n, @, s]).
entry(fein, [f, aI, n]).
entry('Feinschmecker', [f, aI, n, 'S', m, 'E', k, '6']).
entry('Feldstecher', [f, 'E', l, tS, t, 'E', 'C', '6']).
entry('Fel', [f, 'E', l]).
entry('Felix', [f, 'e:', l, 'I', k, s]).
entry('Felmy', [f, 'E', l, m, i]).
entry('Fenster', [f, 'E', n, s, t, '6']).
entry('Fensterplatz', [f, 'E', n, s, t, '6', p, l, a, ts]).
entry('Ferien', [f, 'e:', '6', j, @, n]).
entry(fernbleiben, [f, 'E', '6', n, b, l, aI, b, @, n]).
entry(ferner, [f, 'E', '6', n, '6']).
entry('Fernsehen', [f, 'E', '6', n, z, 'e:', @, n]).
entry('Fernsehkanal', [f, 'E', '6', n, z, 'e:', k, a, n, 'a:', l]).
entry('Ferrari', [f, e, r, 'a:', r, i]).
entry(fertigbringen, [f, 'E', '6', t, 'I', 'C', b, r, 'I', 'N', @, n]).
entry(fertig, [f, 'E', '6', t, 'I', 'C']).
entry(fertiggestellt, [f, 'E', '6', t, 'I', 'C', g, @, 'S', t, 'E', l, t]).
entry(feschen, [f, 'E', 'S', @, n]).
entry(fes, [f, e, s]).
entry(feste, [f, 'E', s, t, @]).
entry(festen, [f, 'E', s, t, @, n]).
entry(festes, [f, 'E', s, t, @, s]).
entry(fest, [f, 'E', s, t]).
entry(festgehalten, [f, 'E', s, t, g, @, h, a, l, t, @, n]).
entry(festgeklopft, [f, 'E', s, t, g, @, k, l, 'O', pf, t]).
entry(festgelegt, [f, 'E', s, t, g, @, l, 'e:', k, t]).
entry(festgemacht, [f, 'E', s, t, g, @, m, a, x, t]).
entry(festgesetzt, [f, 'E', s, t, g, @, z, 'E', ts, t]).
entry(festgestellt, [f, 'E', s, t, g, @, 'S', t, 'E', l, t]).
entry(festhalten, [f, 'E', s, t, h, a, l, t, @, n]).
entry(festklopfen, [f, 'E', s, t, k, l, 'O', pf, @, n]).
entry(festkriegen, [f, 'E', s, t, k, r, 'i:', g, @, n]).
entry(festlegen, [f, 'E', s, t, l, 'e:', g, @, n]).
entry('Festlegung', [f, 'E', s, t, l, 'e:', g, 'U', 'N']).
entry(festliegende, [f, 'E', s, t, l, 'i:', g, @, n, d, @]).
entry(festmachen, [f, 'E', s, t, m, a, x, @, n]).
entry(festschreiben, [f, 'E', s, tS, r, aI, b, @, n]).
entry(festsetzen, [f, 'E', s, t, z, 'E', ts, @, n]).
entry('Festspielen', [f, 'E', s, tS, p, 'i:', l, @, n]).
entry(feststehend, [f, 'E', s, tS, t, 'e:', @, n, t]).
entry(feststelle, [f, 'E', s, tS, t, 'E', l, @]).
entry(feststellen, [f, 'E', s, tS, t, 'E', l, @, n]).
entry('Festtagen', [f, 'E', s, t, t, 'a:', g, @, n]).
entry(festzuhalten, [f, 'E', s, t, ts, u, h, a, l, t, @, n]).
entry(festzulegen, [f, 'E', s, t, ts, u, l, 'e:', g, @, n]).
entry(festzumachen, [f, 'E', s, t, ts, u, m, a, x, @, n]).
entry(festzusetzen, [f, 'E', s, t, ts, u, z, 'E', ts, @, n]).
entry(fetter, [f, 'E', t, '6']).
entry(feuchtfröhlich, [f, 'OY', 'C', t, f, r, '2:', l, 'I', 'C']).
entry('Fichtinger', [f, 'I', 'C', t, 'I', 'N', '6']).
entry(fiele, [f, 'i:', l, @]).
entry(fiel, [f, 'i:', l]).
entry(fier, [f, 'i:', '6']).
entry(fi, [f, 'I']).
entry('Figura', [f, i, g, 'u:', r, a]).
entry('Figur', [f, i, g, 'u:', '6']).
entry('Filiadle', [f, i, l, j, 'a:', d, l, @]).
entry('Filia', [f, 'i:', l, j, a]).
entry('Filialbesichtigung', [f, i, l, j, 'a:', l, b, @, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry('Filialbesuche', [f, i, l, j, 'a:', l, b, @, z, 'u:', x, @]).
entry('Filialbesuch', [f, i, l, j, 'a:', l, b, @, z, 'u:', x]).
entry('Filiale', [f, i, l, j, 'a:', l, @]).
entry('Filialen', [f, i, l, j, 'a:', l, @, n]).
entry('Filial', [f, i, l, j, 'a:', l]).
entry('Filialgeschichte', [f, i, l, j, 'a:', l, g, @, 'S', 'I', 'C', t, @]).
entry('Filialia', [f, i, l, j, 'a:', l, i, a]).
entry('Filialleiterin', [f, i, l, j, 'a:', l, l, aI, t, @, r, 'I', n]).
entry('Filial-Treffen', [f, i, l, j, 'a:', l, t, r, 'E', f, @, n]).
entry('Fili', [f, 'i:', l, i]).
entry('Filjare', [f, i, l, j, 'a:', r, @]).
entry('Filmaufführung', [f, 'I', l, m, aU, f, f, 'y:', r, 'U', 'N']).
entry('Filme', [f, 'I', l, m, @]).
entry('Film', [f, 'I', l, m]).
entry('Filofax', [f, aI, l, o, f, a, k, s]).
entry('Fina', [f, i, n, a]).
entry(finanziell, [f, i, n, a, n, ts, j, 'E', l]).
entry('Finanzplanung', [f, i, n, a, n, ts, p, l, 'a:', n, 'U', 'N']).
entry(finde, [f, 'I', n, d, @]).
entry(finden, [f, 'I', n, d, @, n]).
entry(findest, [f, 'I', n, d, @, s, t]).
entry(findet, [f, 'I', n, d, @, t]).
entry('Fin', [f, i, n]).
entry('Fini', [f, i, n, i]).
entry(fin, ['I', n]).
entry('Firl', [f, 'I', '6', l]).
entry('Firma', [f, 'I', '6', m, a]).
entry('Firmenbesichtigung', [f, 'I', '6', m, @, n, b, @, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry('Firmen', [f, 'I', '6', m, @, n]).
entry('Firmenjet', [f, 'I', '6', m, @, n, d, 'Z', 'E', t]).
entry('Firmenkonto', [f, 'I', '6', m, @, n, k, 'O', n, t, o]).
entry('Firmenkosten', [f, 'I', '6', m, @, n, k, 'O', s, t, @, n]).
entry('Firmenwagen', [f, 'I', '6', m, @, n, v, 'a:', g, @, n]).
entry(first, [f, '9', '6', s, t]).
entry('Fischenbeck', [f, 'I', 'S', @, n, b, 'E', k]).
entry('Fischer', [f, 'I', 'S', '6']).
entry('Fischerkneipe', [f, 'I', 'S', '6', k, n, aI, p, @]).
entry('Fisch', [f, 'I', 'S']).
entry('Fischgerichte', [f, 'I', 'S', g, @, r, 'I', 'C', t, @]).
entry(fist, [f, 'I', s, t]).
entry(fit, [f, 'I', t]).
entry('Fitneßcenter', [f, 'I', t, n, 'E', s, s, 'E', n, t, '6']).
entry('Fitneßprogramm', [f, 'I', t, n, 'E', s, p, r, o, g, r, a, m]).
entry('Fitneßräumen', [f, 'I', t, n, 'E', s, r, 'OY', m, @, n]).
entry('Fitneßraum', [f, 'I', t, n, 'E', s, r, aU, m]).
entry('Fitneßstudio', [f, 'I', t, n, 'E', s, 'S', t, 'u:', d, j, o]).
entry(fixe, [f, 'I', k, s, @]).
entry(fix, [f, 'I', k, s]).
entry(fixieren, [f, 'I', k, s, 'i:', r, @, n]).
entry(flach, [f, l, a, x]).
entry('Flammersfeld', [f, l, a, m, '6', s, f, 'E', l, t]).
entry('Fläschchen', [f, l, 'E', 'S', 'C', @, n]).
entry('Flasche', [f, l, a, 'S', @]).
entry(fle, [f, l, 'E']).
entry('Fleischer', [f, l, aI, 'S', '6']).
entry('Flex', [f, l, 'E', k, s]).
entry(flexibel, [f, l, 'E', k, s, 'i:', b, @, l]).
entry(flexibler, [f, l, 'E', k, s, 'i:', b, l, '6']).
entry(fl, [f, l]).
entry('Flie', [f, l, 'i:']).
entry(fliege, [f, l, 'i:', g, @]).
entry(fliegen, [f, l, 'i:', g, @, n]).
entry('Fliegerei', [f, l, 'i:', g, @, r, aI]).
entry('Flieger', [f, l, 'i:', g, '6']).
entry(flieg, [f, l, 'i:', k]).
entry(fliegt, [f, l, 'i:', k, t]).
entry(flotter, [f, l, 'O', t, '6']).
entry(flott, [f, l, 'O', t]).
entry('Flu', [f, l, 'U']).
entry(flü, [f, l, y]).
entry('Flü', [f, l, 'y:']).
entry('Flugangst', [f, l, 'u:', k, a, 'N', s, t]).
entry('Flugbedingungen', [f, l, 'u:', k, b, @, d, 'I', 'N', 'U', 'N', @, n]).
entry('Flugbestellung', [f, l, 'u:', k, b, @, 'S', t, 'E', l, 'U', 'N']).
entry('Flugdaten', [f, l, 'u:', k, d, 'a:', t, @, n]).
entry('Flugdauer', [f, l, 'u:', k, d, aU, '6']).
entry('Flüge', [f, l, 'y:', g, @]).
entry('Flügel', [f, l, 'y:', g, @, l]).
entry('Flügen', [f, l, 'y:', g, @, n]).
entry('Flug', [f, l, 'u:', k]).
entry(flüg, [f, l, 'y:', k]).
entry('Fluggesellschaft', [f, l, 'u:', k, g, @, z, 'E', l, 'S', a, f, t]).
entry('Flughafen-Anfahrt', [f, l, 'u:', k, h, 'a:', f, @, n, a, n, f, 'a:', r, t]).
entry('Flughafenbus', [f, l, 'u:', k, h, 'a:', f, @, n, b, 'U', s]).
entry('Flughafen', [f, l, 'u:', k, h, 'a:', f, @, n]).
entry('Flughäfen', [f, l, 'u:', k, h, 'E:', f, @, n]).
entry('Flughafenhotel', [f, l, 'u:', k, h, 'a:', f, @, n, h, o, t, 'E', l]).
entry('Flughafenlinie', [f, l, 'u:', k, h, 'a:', f, @, n, l, 'i:', n, j, @]).
entry('Flughafensachen', [f, l, 'u:', k, h, 'a:', f, @, n, z, a, x, @, n]).
entry('Flughafens', [f, l, 'u:', k, h, 'a:', f, @, n, s]).
entry('Flugkarten', [f, l, 'u:', k, k, a, r, t, @, n]).
entry('Fluglinie', [f, l, 'u:', k, l, 'i:', n, j, @]).
entry('Flugmöglichkeiten', [f, l, 'u:', k, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Flugnummer', [f, l, 'u:', k, n, 'U', m, '6']).
entry('Flugpläne', [f, l, 'u:', k, p, l, 'E:', n, @]).
entry('Flugplan', [f, l, 'u:', k, p, l, 'a:', n]).
entry('Flugplatz', [f, l, 'u:', k, p, l, a, ts]).
entry('Flugreise', [f, l, 'u:', k, r, aI, z, @]).
entry('Flugreisen', [f, l, 'u:', k, r, aI, z, @, n]).
entry(flugtechnisch, [f, l, 'u:', k, t, 'E', 'C', n, 'I', 'S']).
entry('Flugtermin', [f, l, 'u:', k, t, 'E', '6', m, 'i:', n]).
entry('Flugticket', [f, l, 'u:', k, t, 'I', k, @, t]).
entry('Flugtickets', [f, l, 'u:', k, t, 'I', k, @, ts]).
entry('Flugunterlagen', [f, l, 'u:', k, 'U', n, t, '6', l, 'a:', g, @, n]).
entry('Flugverbindungen', [f, l, 'u:', k, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry('Flugverbindung', [f, l, 'u:', k, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('Flugv', [f, l, 'u:', k, f]).
entry('Flugzeig', [f, l, 'u:', k, ts, aI, k]).
entry('Flugzeiten', [f, l, 'u:', k, ts, aI, t, @, n]).
entry('Flugzeit', [f, l, 'u:', k, ts, aI, t]).
entry('Flugzeu', [f, l, 'u:', k, ts, 'OY']).
entry('Flugzeugabstürzen', [f, l, 'u:', k, ts, 'OY', k, a, p, 'S', t, 'Y', '6', ts, @, n]).
entry('Flugzeuge', [f, l, 'u:', k, ts, 'OY', g, @]).
entry('Flugzeugen', [f, l, 'u:', k, ts, 'OY', g, @, n]).
entry('Flugzeug-Essen', [f, l, 'u:', k, ts, 'OY', k, 'E', s, @, n]).
entry('Flugzeug', [f, l, 'u:', k, ts, 'OY', k]).
entry('Flugzeugverbindung', [f, l, 'u:', k, ts, 'OY', k, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('Flugzeut', [f, l, 'u:', k, ts, 'OY', t]).
entry('Fluß', [f, l, 'U', s]).
entry(flüssig, [f, l, 'Y', s, 'I', 'C']).
entry(flyer, [f, l, aI, '6']).
entry('Fody', [f, 'o:', d, i]).
entry('Fohrer', [f, 'o:', r, '6']).
entry('Folge', [f, 'O', l, g, @]).
entry(folgende, [f, 'O', l, g, @, n, d, @]).
entry(folgenden, [f, 'O', l, g, @, n, d, @, n]).
entry(folgender, [f, 'O', l, g, @, n, d, '6']).
entry(folgendermaßen, [f, 'O', l, g, @, n, d, '6', m, 'a:', s, @, n]).
entry(folgendes, [f, 'O', l, g, @, n, d, @, s]).
entry(folgen, [f, 'O', l, g, @, n]).
entry('Folgetermin', [f, 'O', l, g, @, t, 'E', '6', m, 'i:', n]).
entry('Folgewoche', [f, 'O', l, g, @, v, 'O', x, @]).
entry('Folgezeit', [f, 'O', l, g, @, ts, aI, t]).
entry(folglich, [f, 'O', l, k, l, 'I', 'C']).
entry(folgt, [f, 'O', l, k, t]).
entry(föls, [f, '9', l, s]).
entry('Fora', [f, 'o:', r, a]).
entry('Fora-Hotel', [f, 'o:', r, a, h, o, t, 'E', l]).
entry('Formalia', [f, 'O', '6', m, 'a:', l, j, a]).
entry('Formalitäten', [f, 'O', '6', m, a, l, i, t, 'E:', t, @, n]).
entry('Formel', [f, 'O', '6', m, @, l]).
entry('Form', [f, 'O', '6', m]).
entry(förmlich, [f, '9', '6', m, l, 'I', 'C']).
entry('Formulare', [f, 'O', '6', m, u, l, 'a:', r, @]).
entry('Fortbildung', [f, 'O', '6', t, b, 'I', l, d, 'U', 'N']).
entry('Fortbildungsreihe', [f, 'O', '6', t, b, 'I', l, d, 'U', 'N', s, r, aI, @]).
entry('Fortbildungsreise', [f, 'O', '6', t, b, 'I', l, d, 'U', 'N', s, r, aI, z, @]).
entry(fortfahren, [f, 'O', '6', t, f, 'a:', r, @, n]).
entry(fort, [f, 'O', '6', t]).
entry(fortlaufenden, [f, 'O', '6', t, l, aU, f, @, n, d, @, n]).
entry(fortsetze, [f, 'O', '6', t, z, 'E', ts, @]).
entry('Fotos', [f, 'o:', t, o, s]).
entry('Foyer', [f, o, a, j, 'e:']).
entry(fra, [f, r, a]).
entry('Fra', [f, r, 'a:']).
entry(frage, [f, r, 'a:', g, @]).
entry(fragen, [f, r, 'a:', g, @, n]).
entry(fraglich, [f, r, 'a:', k, l, 'I', 'C']).
entry(fragte, [f, r, 'a:', k, t, @]).
entry(fragten, [f, r, 'a:', k, t, @, n]).
entry(fragt, [f, r, 'a:', k, t]).
entry('Fran', [f, r, a, n]).
entry('Frank', [f, r, a, 'N', k]).
entry('Frankfurk', [f, r, a, 'N', k, f, 'U', '6', k]).
entry('Frankfurter', [f, r, a, 'N', k, f, 'U', '6', t, '6']).
entry('Frankfurt', [f, r, a, 'N', k, f, 'U', '6', t]).
entry('Frauen', [f, r, aU, @, n]).
entry('Frau', [f, r, aU]).
entry('Fräulein', [f, r, 'OY', l, aI, n]).
entry('Fred', [f, r, 'e:', t]).
entry('Freiburg', [f, r, aI, b, 'U', '6', k]).
entry(freie, [f, r, aI, @]).
entry(freien, [f, r, aI, @, n]).
entry(freier, [f, r, aI, '6']).
entry(freies, [f, r, aI, @, s]).
entry('Freiflüge', [f, r, aI, f, l, 'y:', g, @]).
entry('Freiflug', [f, r, aI, f, l, 'u:', k]).
entry(frei, [f, r, aI]).
entry(freigehalten, [f, r, aI, g, @, h, a, l, t, @, n]).
entry(freigestellt, [f, r, aI, g, @, 'S', t, 'E', l, t]).
entry(freihabe, [f, r, aI, h, 'a:', b, @]).
entry(freihaben, [f, r, aI, h, 'a:', b, @, n]).
entry(freihalten, [f, r, aI, h, a, l, t, @, n]).
entry(freihätten, [f, r, aI, h, 'E', t, @, n]).
entry('Freiheit', [f, r, aI, h, aI, t]).
entry(freilassen, [f, r, aI, l, a, s, @, n]).
entry(freilich, [f, r, aI, l, 'I', 'C']).
entry('Freilichtkino', [f, r, aI, l, 'I', 'C', t, k, 'i:', n, o]).
entry('Freilichttheater', [f, r, aI, l, 'I', 'C', t, t, e, 'a:', t, '6']).
entry(freimache, [f, r, aI, m, a, x, @]).
entry(freimachen, [f, r, aI, m, a, x, @, n]).
entry('Freiräume', [f, r, aI, r, 'OY', m, @]).
entry('Freiraum', [f, r, aI, r, aU, m]).
entry('Freisinger', [f, r, aI, z, 'I', 'N', '6']).
entry(freita, [f, r, aI, t, a]).
entry('Freitag', [f, r, aI, t, 'a:', k]).
entry(freitags, [f, r, aI, t, 'a:', k, s]).
entry('Freitagstermin', [f, r, aI, t, 'a:', k, s, t, 'E', '6', m, 'i:', n]).
entry(freiwilligen, [f, r, aI, v, 'I', l, 'I', g, @, n]).
entry('Freizeitangebot', [f, r, aI, ts, aI, t, a, n, g, @, b, 'o:', t]).
entry('Freizeit', [f, r, aI, ts, aI, t]).
entry('Freizeitgestaltung', [f, r, aI, ts, aI, t, g, @, 'S', t, a, l, t, 'U', 'N']).
entry('Freizeitmöglichkeiten', [f, r, aI, ts, aI, t, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Freizeitraum', [f, r, aI, ts, aI, t, r, aU, m]).
entry(freizuhalten, [f, r, aI, ts, u, h, a, l, t, @, n]).
entry(fremde, [f, r, 'E', m, d, @]).
entry('Fremdenverkehrsamt', [f, r, 'E', m, d, @, n, f, 'E', '6', k, 'e:', '6', s, a, m, t]).
entry('Fremdenverkehrsverein', [f, r, 'E', m, d, @, n, f, 'E', '6', k, 'e:', '6', s, f, 'E', '6', aI, n]).
entry('Fremdfirmen', [f, r, 'E', m, t, f, 'I', '6', m, @, n]).
entry('Fremdsprache', [f, r, 'E', m, tS, p, r, 'a:', x, @]).
entry('Fremdsprachen', [f, r, 'E', m, tS, p, r, 'a:', x, @, n]).
entry(freque, [f, r, e, k, @]).
entry(frequent, [f, r, 'i:', k, v, 'E', n, t]).
entry(frequentieren, [f, r, e, k, v, 'E', n, t, 'i:', r, @, n]).
entry('Freude', [f, r, 'OY', d, @]).
entry('Freudichkeit', [f, r, 'OY', d, 'I', 'C', k, aI, t]).
entry(freudiges, [f, r, 'OY', d, 'I', g, @, s]).
entry(freue, [f, r, 'OY', @]).
entry(freuen, [f, r, 'OY', @, n]).
entry('Freunde', [f, r, 'OY', n, d, @]).
entry('Freunden', [f, r, 'OY', n, d, @, n]).
entry('Freund', [f, r, 'OY', n, t]).
entry('Freundin', [f, r, 'OY', n, d, 'I', n]).
entry('Freundinnen', [f, r, 'OY', n, d, 'I', n, @, n]).
entry(freundliche, [f, r, 'OY', n, t, l, 'I', 'C', @]).
entry(freundlichen, [f, r, 'OY', n, t, l, 'I', 'C', @, n]).
entry(freundlicher, [f, r, 'OY', n, t, l, 'I', 'C', '6']).
entry(freundlich, [f, r, 'OY', n, t, l, 'I', 'C']).
entry(freut, [f, r, 'OY', t]).
entry(fr, [f, r]).
entry('Frieda', [f, r, 'i:', d, a]).
entry('Friedensplatz', [f, r, 'i:', d, @, n, s, p, l, a, ts]).
entry('Friedrich', [f, r, 'i:', d, r, 'I', 'C']).
entry('Friedrichshafen', [f, r, 'i:', d, r, 'I', 'C', s, h, 'a:', f, @, n]).
entry('Fries', [f, r, 'i:', s]).
entry('Fringes', [f, r, 'I', 'N', @, s]).
entry(frische, [f, r, 'I', 'S', @]).
entry(frisch, [f, r, 'I', 'S']).
entry('Friseur', [f, r, i, z, '2:', '6']).
entry('Friseurtermin', [f, r, i, z, '2:', '6', t, 'E', '6', m, 'i:', n]).
entry('Frist', [f, r, 'I', s, t]).
entry('Frittenbude', [f, r, 'I', t, @, n, b, 'u:', d, @]).
entry('Fritzen', [f, r, 'I', ts, @, n]).
entry('Fro', [f, r, 'o:']).
entry(frohe, [f, r, 'o:', @]).
entry(frohes, [f, r, 'o:', @, s]).
entry('Fronleichnam', [f, r, 'o:', n, l, aI, 'C', n, 'a:', m]).
entry('Fronleichnamwoche', [f, r, 'o:', n, l, aI, 'C', n, 'a:', m, v, 'O', x, @]).
entry(fruchtbarer, [f, r, 'U', x, t, b, 'a:', r, '6']).
entry('Früchte', [f, r, 'Y', 'C', t, @]).
entry('Frü', [f, r, 'Y']).
entry('Frühaufsteher', [f, r, 'y:', aU, f, 'S', t, 'e:', '6']).
entry('Frühaufsteherin', [f, r, 'y:', aU, f, 'S', t, 'e:', @, r, 'I', n]).
entry('Frühaufsteh-Typ', [f, r, 'y:', aU, f, 'S', t, 'e:', '6', t, 'y:', p]).
entry(frühe, [f, r, 'y:', @]).
entry(frühen, [f, r, 'y:', @, n]).
entry(frühere, [f, r, 'y:', @, r, @]).
entry(früheren, [f, r, 'y:', @, r, @, n]).
entry(früherer, [f, r, 'y:', @, r, '6']).
entry(früher, [f, r, 'y:', '6']).
entry('Frühes', [f, r, 'y:', @, s]).
entry(früheste, [f, r, 'y:', @, s, t, @]).
entry(frühesten, [f, r, 'y:', @, s, t, @, n]).
entry(frühestens, [f, r, 'y:', @, s, t, @, n, s]).
entry(frühestmögliche, [f, r, 'y:', @, s, t, m, '2:', k, l, 'I', 'C', @]).
entry(früh, [f, r, y]).
entry('Früh', [f, r, 'y:']).
entry('Frühjahr', [f, r, 'y:', j, 'a:', r]).
entry('Frühling', [f, r, 'y:', l, 'I', 'N']).
entry('Frühlingsanfang', [f, r, 'y:', l, 'I', 'N', s, a, n, f, a, 'N']).
entry(frühmorgens, [f, r, 'y:', m, 'O', '6', g, @, n, s]).
entry(frühste, [f, r, 'y:', s, t, @]).
entry(frühsten, [f, r, 'y:', s, t, @, n]).
entry(frühstens, [f, r, 'y:', s, t, @, n, s]).
entry(frühst, [f, r, 'y:', 'S', t]).
entry('Frühstückchen', [f, r, 'y:', 'S', t, 'Y', k, 'C', @, n]).
entry(frühstücke, [f, r, 'y:', 'S', t, 'Y', k, @]).
entry(frühstücken, [f, r, y, 'S', t, 'Y', k, @, n]).
entry('Frühstücken', [f, r, 'y:', 'S', t, 'Y', k, @, n]).
entry('Frühstück', [f, r, 'y:', 'S', t, 'Y', k]).
entry('Frühstücksbuffet', [f, r, 'y:', 'S', t, 'Y', k, s, b, 'Y', f, 'e:']).
entry('Frühstücks', [f, r, 'y:', 'S', t, 'Y', k, s]).
entry(frühzeitig, [f, r, 'y:', ts, aI, t, 'I', 'C']).
entry(füf, [f, 'Y', f]).
entry(fu, [f, u]).
entry('Fu', [f, 'u:']).
entry(fü, [f, y]).
entry('Fü', [f, 'y:']).
entry(fügt, [f, 'y:', k, t]).
entry(fühle, [f, 'y:', l, @]).
entry(führende, [f, 'y:', r, @, n, d, @]).
entry(fuhren, [f, 'u:', r, @, n]).
entry(führen, [f, 'y:', r, @, n]).
entry('Führer', [f, 'y:', r, '6']).
entry('Führerschein', [f, 'y:', r, '6', 'S', aI, n]).
entry(fuhr, [f, 'u:', '6']).
entry(führt, [f, 'y:', '6', t]).
entry('Führungskräfte', [f, 'y:', r, 'U', 'N', s, k, r, 'E', f, t, @]).
entry('Fulda', [f, 'U', l, d, a]).
entry(füm, [f, 'Y', m]).
entry(fündig, [f, 'Y', n, d, 'I', 'C']).
entry(fünfeinhalb, [f, 'Y', n, f, aI, n, h, a, l, p]).
entry(fünf, [f, 'Y', n, f]).
entry(fünfhundert, [f, 'Y', n, f, h, 'U', n, d, '6', t]).
entry(fünftädi, [f, 'Y', n, f, t, 'E:', d, 'I']).
entry(fünftädigen, [f, 'Y', n, f, t, 'E:', d, 'I', g, @, n]).
entry('Fünf-Tages-Blöcke', [f, 'Y', n, f, t, 'a:', g, @, s, b, l, '9', k, @]).
entry(fünftäges, [f, 'Y', n, f, t, 'E:', g, @, s]).
entry('Fünf-Tages-Seminar', [f, 'Y', n, f, t, 'a:', g, @, s, z, e, m, i, n, 'a:', r]).
entry('Fünf-Tages-Spanne', [f, 'Y', n, f, t, 'a:', g, @, 'S', p, a, n, @]).
entry('Fünf-Tages-Zeitraum', [f, 'Y', n, f, t, 'a:', g, @, s, ts, aI, t, r, aU, m]).
entry(fünftägies, [f, 'Y', n, f, t, 'E:', g, 'I', @, s]).
entry(fünftägi, [f, 'Y', n, f, t, 'E:', g, 'I']).
entry(fünftägige, [f, 'Y', n, f, t, 'E:', g, 'I', g, @]).
entry(fünftägigen, [f, 'Y', n, f, t, 'E:', g, 'I', g, @, n]).
entry(fünftägiger, [f, 'Y', n, f, t, 'E:', g, 'I', g, '6']).
entry(fünftägiges, [f, 'Y', n, f, t, 'E:', g, 'I', g, @, s]).
entry(fünftägig, [f, 'Y', n, f, t, 'E:', g, 'I', 'C']).
entry(fünftägiriges, [f, 'Y', n, f, t, 'E:', g, 'I', r, 'I', g, @, s]).
entry(fünftäige, [f, 'Y', n, f, t, 'E:', 'I', g, @]).
entry(fünfte, [f, 'Y', n, f, t, @]).
entry(fünften, [f, 'Y', n, f, t, @, n]).
entry(fünfter, [f, 'Y', n, f, t, '6']).
entry('Fünfter-Sechster-Termin', [f, 'Y', n, f, t, '6', z, 'E', k, s, t, '6', t, 'E', '6', m, 'i:', n]).
entry('Fünf-Uhr-Zug', [f, 'Y', n, f, 'u:', '6', ts, 'u:', k]).
entry(fünfundachtzig, [f, 'Y', n, f, 'U', n, t, a, x, ts, 'I', 'C']).
entry(fünfunddreißig, [f, 'Y', n, f, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(fünfundfünfzig, [f, 'Y', n, f, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(fünfund, [f, 'Y', n, f, 'U', n, t]).
entry(fünfundneunzig, [f, 'Y', n, f, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(fünfundneunzigsten, [f, 'Y', n, f, 'U', n, t, n, 'OY', n, ts, 'I', 'C', s, t, @, n]).
entry(fünfundsechzig, [f, 'Y', n, f, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(fünfundsiebzig, [f, 'Y', n, f, 'U', n, t, z, 'i:', p, ts, 'I', 'C']).
entry(fünfundv, [f, 'Y', n, f, 'U', n, t, f]).
entry(fünfundvierzi, [f, 'Y', n, f, 'U', n, t, f, 'I', '6', ts, 'I']).
entry(fünfundvierzig, [f, 'Y', n, f, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(fünfundzwanzig, [f, 'Y', n, f, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(fünfundzwanzigs, [f, 'Y', n, f, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s]).
entry(fünfundzwanzigste, [f, 'Y', n, f, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(fünfundzwanzigsten, [f, 'Y', n, f, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(fünfundzwanzigster, [f, 'Y', n, f, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(fünfun, [f, 'Y', n, f, 'U', n]).
entry(fün, [f, 'Y', n]).
entry(fünfzehn, [f, 'Y', n, f, ts, 'e:', n]).
entry(fünfzehnte, [f, 'Y', n, f, ts, 'e:', n, t, @]).
entry(fünfzehnten, [f, 'Y', n, f, ts, 'e:', n, t, @, n]).
entry(fünfzehnter, [f, 'Y', n, f, ts, 'e:', n, t, '6']).
entry(fünfz, [f, 'Y', n, f, ts]).
entry(fünfzig, [f, 'Y', n, f, ts, 'I', 'C']).
entry(fünfzigste, [f, 'Y', n, f, ts, 'I', 'C', s, t, @]).
entry(funktionell, [f, 'U', 'N', k, ts, j, o, n, 'E', l]).
entry('Funktionen', [f, 'U', 'N', k, ts, j, 'o:', n, @, n]).
entry(funktionieren, [f, 'U', 'N', k, ts, j, o, n, 'i:', r, @, n]).
entry(funktioniert, [f, 'U', 'N', k, ts, j, o, n, 'i:', '6', t]).
entry('Funkturm', [f, 'U', 'N', k, t, 'U', '6', m]).
entry(furch, [f, 'U', '6', 'C']).
entry(furchtbare, [f, 'U', '6', 'C', t, b, 'a:', r, @]).
entry(furchtbar, [f, 'U', '6', 'C', t, b, 'a:', r]).
entry(fürchte, [f, 'Y', '6', 'C', t, @]).
entry(fürchterlichen, [f, 'Y', '6', 'C', t, '6', l, 'I', 'C', @, n]).
entry(fürchterlich, [f, 'Y', '6', 'C', t, '6', l, 'I', 'C']).
entry(füreinander, [f, 'y:', '6', aI, n, a, n, d, '6']).
entry(für, [f, 'y:', '6']).
entry(fürn, [f, 'y:', '6', n]).
entry(fürs, [f, 'y:', '6', s]).
entry('Fürth', [f, 'Y', '6', t]).
entry('Füße', [f, 'y:', s, @]).
entry('Fuß', [f, 'u:', s]).
entry('Fußgängerzone', [f, 'u:', s, g, 'E', 'N', '6', ts, 'o:', n, @]).
entry('Fußweg', [f, 'u:', s, v, 'e:', k]).
entry(fzehnten, [f, ts, 'e:', n, t, @, n]).
entry('Gabe', [g, 'a:', b, @]).
entry(gäbe, [g, 'E:', b, @]).
entry(gab, [g, 'a:', p]).
entry(ga, [g, a]).
entry(gä, [g, 'E:']).
entry(gähnende, [g, 'E:', n, @, n, d, @]).
entry('Galerie-am-Stift', [g, 'E', l, @, r, 'I', a, m, s, t, 'I', f, t]).
entry('Galerien', [g, a, l, @, r, 'i:', @, n]).
entry(game, [g, e, 'I', m]).
entry(gan, [g, a, n]).
entry('Gänge', [g, 'E', 'N', @]).
entry('Gang', [g, a, 'N']).
entry(gängigen, [g, 'E', 'N', 'I', g, @, n]).
entry('Gans', [g, a, n, s]).
entry(ganze, [g, a, n, ts, @]).
entry('Gänze', [g, 'E', n, ts, @]).
entry(ganzen, [g, a, n, ts, @, n]).
entry(ganzer, [g, a, n, ts, '6']).
entry(ganzes, [g, a, n, ts, @, s]).
entry(ganz, [g, a, n, ts]).
entry(gänzlich, [g, 'E', n, ts, l, 'I', 'C']).
entry(ganztägige, [g, a, n, ts, t, 'E:', g, 'I', g, @]).
entry(ganztägiger, [g, a, n, ts, t, 'E:', g, 'I', g, '6']).
entry(ganztägiges, [g, a, n, ts, t, 'E:', g, 'I', g, @, s]).
entry(ganztagig, [g, a, n, ts, t, 'a:', g, 'I', 'C']).
entry(ganztägig, [g, a, n, ts, t, 'E:', g, 'I', 'C']).
entry(ganztags, [g, a, n, ts, t, 'a:', k, s]).
entry('Ganztagsseminare', [g, a, n, ts, t, 'a:', k, s, z, e, m, i, n, 'a:', r, @]).
entry('Garage', [g, a, r, 'a:', 'Z', @]).
entry('Garagen', [g, a, r, 'a:', 'Z', @, n]).
entry(garantieren, [g, a, r, a, n, t, 'i:', r, @, n]).
entry(garantiert, [g, a, r, a, n, t, 'i:', '6', t]).
entry(gar, [g, 'a:', r]).
entry(garten, [g, a, r, t, @, n]).
entry('Gäste', [g, 'E', s, t, @]).
entry('Gasthaus', [g, a, s, t, h, aU, s]).
entry('Gastspiele', [g, a, s, tS, p, 'i:', l, @]).
entry('Gastspiel', [g, a, s, tS, p, 'i:', l]).
entry('Gattin', [g, a, t, 'I', n]).
entry('Gauß', [g, aU, s]).
entry('Gavlas', [g, a, f, l, a, s]).
entry('Gayst', [g, aI, s, t]).
entry(geahnt, [g, @, 'a:', n, t]).
entry(gearbeitet, [g, @, a, r, b, aI, t, @, t]).
entry(geäußert, [g, @, 'OY', s, '6', t]).
entry('Gebäude', [g, @, b, 'OY', d, @]).
entry('Gebäudes', [g, @, b, 'OY', d, @, s]).
entry(gebaut, [g, @, b, aU, t]).
entry(gebe, [g, 'e:', b, @]).
entry(geben, [g, 'e:', b, @, n]).
entry(gebeten, [g, @, b, 'e:', t, @, n]).
entry('Gebieten', [g, @, b, 'i:', t, @, n]).
entry(geblättert, [g, @, b, l, 'E', t, '6', t]).
entry(geblieben, [g, @, b, l, 'i:', b, @, n]).
entry(gebong, [g, @, b, 'O', 'N']).
entry(gebongt, [g, @, b, 'O', 'N', t]).
entry(geboten, [g, @, b, 'o:', t, @, n]).
entry(gebrach, [g, @, b, r, a, x]).
entry(gebracht, [g, @, b, r, a, x, t]).
entry(gebrauchen, [g, @, b, r, aU, x, @, n]).
entry(gebraut, [g, @, b, r, aU, t]).
entry(gebucht, [g, @, b, 'u:', x, t]).
entry(gebunden, [g, @, b, 'U', n, d, @, n]).
entry(gebürtiger, [g, @, b, 'Y', '6', t, 'I', g, '6']).
entry('Geburtsstadt', [g, @, b, 'u:', '6', ts, 'S', t, a, t]).
entry('Geburtstag', [g, @, b, 'u:', '6', ts, t, 'a:', k]).
entry('Geburtstagsgeschenke', [g, @, b, 'u:', '6', ts, t, 'a:', k, s, g, @, 'S', 'E', 'N', k, @]).
entry('Geburtstagskuchen', [g, @, b, 'u:', '6', ts, t, 'a:', k, s, k, 'u:', x, @, n]).
entry(gedachten, [g, @, d, a, x, t, @, n]).
entry(gedacht, [g, @, d, a, x, t]).
entry('Gedächtnis', [g, @, d, 'E', 'C', t, n, 'I', s]).
entry('Gedanke', [g, @, d, a, 'N', k, @]).
entry('Gedanken', [g, @, d, a, 'N', k, @, n]).
entry(gedauert, [g, @, d, aU, '6', t]).
entry(geden, [g, @, d, 'E', 'N']).
entry(gedenken, [g, @, d, 'E', 'N', k, @, n]).
entry(gediegen, [g, @, d, 'i:', g, @, n]).
entry(gedient, [g, @, d, 'i:', n, t]).
entry(gedrängt, [g, @, d, r, 'E', 'N', t]).
entry(gedruckt, [g, @, d, r, 'U', k, t]).
entry(geeignete, [g, @, aI, g, n, @, t, @]).
entry(geeigneten, [g, @, aI, g, n, @, t, @, n]).
entry(geeignetes, [g, @, aI, g, n, @, t, @, s]).
entry(geeignet, [g, @, aI, g, n, @, t]).
entry(geeinigt, [g, @, aI, n, 'I', 'C', t]).
entry(gefahren, [g, @, f, 'a:', r, @, n]).
entry('Gefahr', [g, @, f, 'a:', r]).
entry(gefährlich, [g, @, f, 'E:', '6', l, 'I', 'C']).
entry(gefallen, [g, @, f, a, l, @, n]).
entry(gefällt, [g, @, f, 'E', l, t]).
entry(gefaßt, [g, @, f, a, s, t]).
entry(gefaxt, [g, @, f, a, k, s, t]).
entry(gefeiert, [g, @, f, aI, '6', t]).
entry(geflext, [g, @, f, l, 'E', k, s, t]).
entry(geflogen, [g, @, f, l, 'o:', g, @, n]).
entry(gefragt, [g, @, f, r, 'a:', k, t]).
entry(gefreut, [g, @, f, r, 'OY', t]).
entry('Gefühl', [g, @, f, 'y:', l]).
entry(geführten, [g, @, f, 'y:', '6', t, @, n]).
entry(gefüllten, [g, @, f, 'Y', l, t, @, n]).
entry(gefunden, [g, @, f, 'U', n, d, @, n]).
entry(ge, [g, @]).
entry(gegangen, [g, @, g, a, 'N', @, n]).
entry('Ge', [g, 'e:']).
entry(gegebenenfalls, [g, @, g, 'e:', b, @, n, @, n, f, a, l, s]).
entry(gegebenen, [g, @, g, 'e:', b, @, n, @, n]).
entry(gegeben, [g, @, g, 'e:', b, @, n]).
entry('Gegebenheiten', [g, @, g, 'e:', b, @, n, h, aI, t, @, n]).
entry('Gegenangebot', [g, 'e:', g, @, n, a, n, g, @, b, 'o:', t]).
entry('Gegend', [g, 'e:', g, @, n, t]).
entry(gegen, [g, 'e:', g, @, n]).
entry(gegenhalten, [g, 'e:', g, @, n, h, a, l, t, @, n]).
entry('Gegenleistung', [g, 'e:', g, @, n, l, aI, s, t, 'U', 'N']).
entry('Gegensatz', [g, 'e:', g, @, n, z, a, ts]).
entry(gegenseitig, [g, 'e:', g, @, n, z, aI, t, 'I', 'C']).
entry('Gegenteil', [g, 'e:', g, @, n, t, aI, l]).
entry(gegenüber, [g, 'e:', g, @, n, 'y:', b, '6']).
entry('Gegenvorschlag', [g, 'e:', g, @, n, f, 'o:', '6', 'S', l, 'a:', k]).
entry('Gegenzug', [g, 'e:', g, @, n, ts, 'u:', k]).
entry(gegessen, [g, @, g, 'E', s, @, n]).
entry(geguckt, [g, @, g, 'U', k, t]).
entry(gehabt, [g, @, h, 'a:', p, t]).
entry(gehalten, [g, @, h, a, l, t, @, n]).
entry('Gehaltserhöhung', [g, @, h, a, l, ts, 'E', '6', h, '2:', 'U', 'N']).
entry(gehandhabt, [g, @, h, a, n, t, h, 'a:', p, t]).
entry(gehe, [g, 'e:', @]).
entry(geheißen, [g, @, h, aI, s, @, n]).
entry(gehend, [g, 'e:', @, n, t]).
entry(gehen, [g, 'e:', @, n]).
entry(geh, [g, @, h]).
entry(gehobene, [g, @, h, 'o:', b, @, n, @]).
entry(gehobenen, [g, @, h, 'o:', b, @, n, @, n]).
entry(gehobenere, [g, @, h, 'o:', b, @, n, @, r, @]).
entry('Gehobeneres', [g, @, h, 'o:', b, @, n, @, r, @, s]).
entry(gehobener, [g, @, h, 'o:', b, @, n, '6']).
entry(gehofft, [g, @, h, 'O', f, t]).
entry(geholt, [g, @, h, 'o:', l, t]).
entry(gehöre, [g, @, h, '2:', r, @]).
entry(gehört, [g, @, h, '2:', '6', t]).
entry('Gehrmann', [g, 'e:', '6', m, a, n]).
entry(geht, [g, 'e:', t]).
entry(geirrt, [g, @, 'I', '6', t]).
entry('Geistern', [g, aI, s, t, '6', n]).
entry(gekauft, [g, @, k, aU, f, t]).
entry(geklappt, [g, @, k, l, a, p, t]).
entry(geklärt, [g, @, k, l, 'E:', '6', t]).
entry(gekommen, [g, @, k, 'O', m, @, n]).
entry(gekonnt, [g, @, k, 'O', n, t]).
entry(gekostet, [g, @, k, 'O', 'S', t, @, t]).
entry(gekriegt, [g, @, k, r, 'i:', k, t]).
entry(gekuckt, [g, @, k, 'U', k, t]).
entry(gekümmert, [g, @, k, 'Y', m, '6', t]).
entry(gelacht, [g, @, l, a, x, t]).
entry(gelangen, [g, @, l, a, 'N', @, n]).
entry(gelassen, [g, @, l, a, s, @, n]).
entry(gelaufen, [g, @, l, aU, f, @, n]).
entry(geläufig, [g, @, l, 'OY', f, 'I', 'C']).
entry(gelaunt, [g, @, l, aU, n, t]).
entry('Geld', [g, 'E', l, t]).
entry(gelegene, [g, @, l, 'e:', g, @, n, @]).
entry(gelegenen, [g, @, l, 'e:', g, @, n, @, n]).
entry(gelegen, [g, @, l, 'e:', g, @, n]).
entry('Gelegenheiten', [g, @, l, 'e:', g, @, n, h, aI, t, @, n]).
entry('Gelegenheit', [g, @, l, 'e:', g, @, n, h, aI, t]).
entry(gelegensten, [g, @, l, 'e:', g, @, n, s, t, @, n]).
entry(gelegentlich, [g, @, l, 'e:', g, @, n, t, l, 'I', 'C']).
entry(gelegt, [g, @, l, 'e:', k, t]).
entry(gelernt, [g, @, l, 'E', '6', n, t]).
entry(gelesen, [g, @, l, 'e:', z, @, n]).
entry(gelie, [g, @, l, 'i:']).
entry('Gelingen', [g, @, l, 'I', 'N', @, n]).
entry('Geller', [g, 'E', l, '6']).
entry(gell, [g, 'E', l]).
entry(gelockert, [g, @, l, 'O', k, '6', t]).
entry(gelöst, [g, @, l, '2:', s, t]).
entry(gelten, [g, 'E', l, t, @, n]).
entry(gelungener, [g, @, l, 'U', 'N', @, n, '6']).
entry(gemächlich, [g, @, m, 'E:', 'C', l, 'I', 'C']).
entry(gemacht, [g, @, m, a, x, t]).
entry('GEMA', [g, 'e:', m, a]).
entry(gema, [g, @, m, a]).
entry('Gemeinde', [g, @, m, aI, n, d, @]).
entry(gemeinsame, [g, @, m, aI, n, z, 'a:', m, @]).
entry(gemeinsamen, [g, @, m, aI, n, z, 'a:', m, @, n]).
entry(gemeinsamer, [g, @, m, aI, n, z, 'a:', m, '6']).
entry(gemeinsames, [g, @, m, aI, n, z, 'a:', m, @, s]).
entry(gemeinsam, [g, @, m, aI, n, z, 'a:', m]).
entry(gemeint, [g, @, m, aI, n, t]).
entry(gemeldet, [g, @, m, 'E', l, d, @, t]).
entry(gemer, [g, @, m, 'E', '6']).
entry(gemerkt, [g, @, m, 'E', '6', k, t]).
entry(gemes, [g, @, m, 'E', s]).
entry(gemessene, [g, @, m, 'E', s, @, n, @]).
entry(gem, [g, @, m]).
entry('Gemüte', [g, @, m, 'y:', t, @]).
entry(gemütlichen, [g, @, m, 'y:', t, l, 'I', 'C', @, n]).
entry('Gemütlicheres', [g, @, m, 'y:', t, l, 'I', 'C', @, r, @, s]).
entry(gemütlicher, [g, @, m, 'y:', t, l, 'I', 'C', '6']).
entry(gemütliches, [g, @, m, 'y:', t, l, 'I', 'C', @, s]).
entry(gemütlich, [g, @, m, 'y:', t, l, 'I', 'C']).
entry(gemütlichste, [g, @, m, 'y:', t, l, 'I', 'C', s, t, @]).
entry(genächtigt, [g, @, n, 'E', 'C', t, 'I', 'C', t]).
entry(gena, [g, @, n, a]).
entry(genannte, [g, @, n, a, n, t, @]).
entry(genannt, [g, @, n, a, n, t]).
entry(genaue, [g, @, n, aU, @]).
entry(genauen, [g, @, n, aU, @, n]).
entry(genauere, [g, @, n, aU, @, r, @]).
entry(genaueren, [g, @, n, aU, @, r, @, n]).
entry(genaueres, [g, @, n, aU, @, r, @, s]).
entry(genauer, [g, @, n, aU, '6']).
entry('Genaues', [g, @, n, aU, @, s]).
entry(genauesten, [g, @, n, aU, @, s, t, @, n]).
entry(genaugenommen, [g, @, n, aU, g, @, n, 'O', m, @, n]).
entry(genau, [g, @, n, aU]).
entry(genauso, [g, @, n, aU, z, 'o:']).
entry(genauten, [g, @, n, aU, t, @, n]).
entry(genehme, [g, @, n, 'e:', m, @]).
entry(genehmer, [g, @, n, 'e:', m, '6']).
entry(genehm, [g, @, n, 'e:', m]).
entry(genehmigen, [g, @, n, 'e:', m, 'I', g, @, n]).
entry(generell, [g, e, n, @, r, 'E', l]).
entry(genervt, [g, @, n, 'E', '6', f, t]).
entry('Genf', [g, 'E', n, f]).
entry(gen, [g, @, n]).
entry(genial, [g, e, n, j, 'a:', l]).
entry(genieße, [g, @, n, 'i:', s, @]).
entry(genießen, [g, @, n, 'i:', s, @, n]).
entry(genommen, [g, @, n, 'O', m, @, n]).
entry(genossenschaftliche, [g, @, n, 'O', s, @, n, 'S', a, f, t, l, 'I', 'C', @]).
entry(genügend, [g, @, n, 'y:', g, @, n, t]).
entry(genügen, [g, @, n, 'y:', g, @, n]).
entry(genug, [g, @, n, 'u:', k]).
entry(genügt, [g, @, n, 'y:', k, t]).
entry(genutzt, [g, @, n, 'U', ts, t]).
entry(geöffnet, [g, @, '9', f, n, @, t]).
entry(geopfert, [g, @, 'O', pf, '6', t]).
entry('Georgengarten', [g, 'e:', 'O', '6', g, @, n, g, a, r, t, @, n]).
entry('Georgen', [g, 'e:', 'O', '6', g, @, n]).
entry('Georgen-Hof', [g, 'e:', 'O', '6', g, @, n, h, 'o:', f]).
entry('Georg', [g, 'e:', 'O', '6', k]).
entry('Georgiana', [d, 'Z', 'O', '6', d, 'Z', j, 'a:', n, a]).
entry('Gepäck', [g, @, p, 'E', k]).
entry(gepackt, [g, @, p, a, k, t]).
entry(gepflegt, [g, @, pf, l, 'e:', k, t]).
entry(geplante, [g, @, p, l, 'a:', n, t, @]).
entry(geplanten, [g, @, p, l, 'a:', n, t, @, n]).
entry(geplantes, [g, @, p, l, 'a:', n, t, @, s]).
entry(geplant, [g, @, p, l, 'a:', n, t]).
entry(geprallt, [g, @, p, r, a, l, t]).
entry(gepreßt, [g, @, p, r, 'E', s, t]).
entry(geprüft, [g, @, p, r, 'y:', f, t]).
entry(gerade, [g, @, r, 'a:', d, @]).
entry(gerädert, [g, @, r, 'E:', d, '6', t]).
entry(geraten, [g, @, r, 'a:', t, @, n]).
entry(geräuschempfindlich, [g, @, r, 'OY', 'S', 'E', m, pf, 'I', n, t, l, 'I', 'C']).
entry(gerechnet, [g, @, r, 'E', 'C', n, @, t]).
entry(geredet, [g, @, r, 'e:', d, @, t]).
entry(geregelt, [g, @, r, 'e:', g, @, l, t]).
entry(gereist, [g, @, r, aI, s, t]).
entry(gereizt, [g, @, r, aI, ts, t]).
entry(gerettet, [g, @, r, 'E', t, @, t]).
entry(ger, [g, @, r]).
entry('Gerhard', [g, 'e:', '6', h, a, r, t]).
entry('Gerholzer', [g, 'e:', '6', h, 'O', l, ts, '6']).
entry('Gerholzner', [g, 'e:', '6', h, 'O', l, ts, n, '6']).
entry(gerichtet, [g, @, r, 'I', 'C', t, @, t]).
entry('Gerichtstermin', [g, @, r, 'I', 'C', ts, t, 'E', '6', m, 'i:', n]).
entry(geringen, [g, @, r, 'I', 'N', @, n]).
entry(geringer, [g, @, r, 'I', 'N', '6']).
entry(geritzt, [g, @, r, 'I', ts, t]).
entry(gerne, [g, 'E', '6', n, @]).
entry(gern, [g, 'E', '6', n]).
entry('Gero', [g, 'e:', r, o]).
entry('Gertraud', [g, 'E', '6', t, r, aU, t]).
entry(geruhsam, [g, @, r, 'u:', z, 'a:', m]).
entry(gerüstet, [g, @, r, 'Y', s, t, @, t]).
entry(gesagt, [g, @, z, 'a:', k, t]).
entry(gesa, [g, @, z, a]).
entry(gesalzene, [g, @, z, a, l, ts, @, n, @]).
entry(gesammelt, [g, @, z, a, m, 'E', l, t]).
entry(gesamte, [g, @, z, a, m, t, @]).
entry(gesamten, [g, @, z, a, m, t, @, n]).
entry('Gesamtkonferenz', [g, @, z, a, m, t, k, 'O', n, f, e, r, 'E', n, ts]).
entry('Gesamtliste', [g, @, z, a, m, t, l, 'I', s, t, @]).
entry('Gesang', [g, @, z, a, 'N']).
entry(gesät, [g, @, z, 'E:', t]).
entry(geschafft, [g, @, 'S', a, f, t]).
entry('Geschäftchen', [g, @, 'S', 'E', f, t, 'C', @, n]).
entry('Geschäfte', [g, @, 'S', 'E', f, t, @]).
entry('Geschäft', [g, @, 'S', 'E', f, t]).
entry(geschäftliche, [g, @, 'S', 'E', f, t, l, 'I', 'C', @]).
entry(geschäftlichen, [g, @, 'S', 'E', f, t, l, 'I', 'C', @, n]).
entry(geschäftlicher, [g, @, 'S', 'E', f, t, l, 'I', 'C', '6']).
entry(geschäftlich, [g, @, 'S', 'E', f, t, l, 'I', 'C']).
entry('Geschäftsauto', [g, @, 'S', 'E', f, ts, aU, t, o]).
entry('Geschäftsbesuch', [g, @, 'S', 'E', f, ts, b, @, z, 'u:', x]).
entry('Geschäftsdinge', [g, @, 'S', 'E', f, ts, d, 'I', 'N', @]).
entry('Geschäftsessen', [g, @, 'S', 'E', f, ts, 'E', s, @, n]).
entry('Geschäftsessens', [g, @, 'S', 'E', f, ts, 'E', s, @, n, s]).
entry('Geschäftsfahrt', [g, @, 'S', 'E', f, ts, f, 'a:', r, t]).
entry('Geschäftsfrau', [g, @, 'S', 'E', f, ts, f, r, aU]).
entry('Geschäftsfreunde', [g, @, 'S', 'E', f, ts, f, r, 'OY', n, d, @]).
entry('Geschäftsfreunden', [g, @, 'S', 'E', f, ts, f, r, 'OY', n, d, @, n]).
entry('Geschäftsfreund', [g, @, 'S', 'E', f, ts, f, r, 'OY', n, t]).
entry('Geschäftsführer', [g, @, 'S', 'E', f, ts, f, 'y:', r, '6']).
entry('Geschäftsgespräch', [g, @, 'S', 'E', f, ts, g, @, 'S', p, r, 'E:', 'C']).
entry('Geschäfts', [g, @, 'S', 'E', f, ts]).
entry('Geschäftsinteressen', [g, @, 'S', 'E', f, ts, 'I', n, t, @, r, 'E', s, @, n]).
entry('Geschäftskarte', [g, @, 'S', 'E', f, ts, k, a, r, t, @]).
entry('Geschäftskonferenz', [g, @, 'S', 'E', f, ts, k, 'O', n, f, e, r, 'E', n, ts]).
entry('Geschäftskosten', [g, @, 'S', 'E', f, ts, k, 'O', s, t, @, n]).
entry('Geschäftsleitung', [g, @, 'S', 'E', f, ts, l, aI, t, 'U', 'N']).
entry('Geschäftsleute', [g, @, 'S', 'E', f, ts, l, 'OY', t, @]).
entry('Geschäftsleuten', [g, @, 'S', 'E', f, ts, l, 'OY', t, @, n]).
entry('Geschäftsmann', [g, @, 'S', 'E', f, ts, m, a, n]).
entry('Geschäftsordnung', [g, @, 'S', 'E', f, ts, 'O', '6', d, n, 'U', 'N']).
entry('Geschäftspartner', [g, @, 'S', 'E', f, ts, p, a, r, t, n, '6']).
entry('Geschäftspartnern', [g, @, 'S', 'E', f, ts, p, a, r, t, n, '6', n]).
entry('Geschäftsrei', [g, @, 'S', 'E', f, ts, r, aI]).
entry('Geschäftsreise', [g, @, 'S', 'E', f, ts, r, aI, z, @]).
entry('Geschäftsreisende', [g, @, 'S', 'E', f, ts, r, aI, z, @, n, d, @]).
entry('Geschäftsreisen', [g, @, 'S', 'E', f, ts, r, aI, z, @, n]).
entry('Geschäftsr', [g, @, 'S', 'E', f, ts, r]).
entry('Geschäftsstelle', [g, @, 'S', 'E', f, ts, 'S', t, 'E', l, @]).
entry('Geschäftstage', [g, @, 'S', 'E', f, ts, t, 'a:', g, @]).
entry('Geschäftstag', [g, @, 'S', 'E', f, ts, t, 'a:', k]).
entry('Geschäftstermine', [g, @, 'S', 'E', f, ts, t, 'E', '6', m, 'i:', n, @]).
entry('Geschäftstermin', [g, @, 'S', 'E', f, ts, t, 'E', '6', m, 'i:', n]).
entry('Geschäftstreffen', [g, @, 'S', 'E', f, ts, t, r, 'E', f, @, n]).
entry('Geschäftswagen', [g, @, 'S', 'E', f, ts, v, 'a:', g, @, n]).
entry(geschätzte, [g, @, 'S', 'E', ts, t, @]).
entry(geschätzt, [g, @, 'S', 'E', ts, t]).
entry(geschaut, [g, @, 'S', aU, t]).
entry(geschehen, [g, @, 'S', 'e:', @, n]).
entry(gescheiter, [g, @, 'S', aI, t, '6']).
entry(gescheites, [g, @, 'S', aI, t, @, s]).
entry('Gescheiteste', [g, @, 'S', aI, t, @, s, t, @]).
entry('Geschenke', [g, @, 'S', 'E', 'N', k, @]).
entry(gesch, [g, @, 'S']).
entry('Geschichte', [g, @, 'S', 'I', 'C', t, @]).
entry(geschichtger, [g, @, 'S', 'I', 'C', t, g, '6']).
entry(geschickter, [g, @, 'S', 'I', k, t, '6']).
entry(geschicktesten, [g, @, 'S', 'I', k, t, @, s, t, @, n]).
entry(geschickt, [g, @, 'S', 'I', k, t]).
entry(geschlagen, [g, @, 'S', l, 'a:', g, @, n]).
entry(geschlossener, [g, @, 'S', l, 'O', s, @, n, '6']).
entry(geschlossen, [g, @, 'S', l, 'O', s, @, n]).
entry('Geschmack', [g, @, 'S', m, a, k]).
entry(geschneit, [g, @, 'S', n, aI, t]).
entry(geschöpft, [g, @, 'S', '9', pf, t]).
entry(geschrieben, [g, @, 'S', r, 'i:', b, @, n]).
entry('Gescht', [g, e, 'S', t]).
entry('Geschwindigkeit', [g, @, 'S', v, 'I', n, d, 'I', 'C', k, aI, t]).
entry(gesehen, [g, @, z, 'e:', @, n]).
entry(geselligen, [g, @, z, 'E', l, 'I', g, @, n]).
entry(geselliger, [g, @, z, 'E', l, 'I', g, '6']).
entry(geselliges, [g, @, z, 'E', l, 'I', g, @, s]).
entry('Gesellschaft', [g, @, z, 'E', l, 'S', a, f, t]).
entry(gesellschaftliche, [g, @, z, 'E', l, 'S', a, f, t, l, 'I', 'C', @]).
entry(gesessen, [g, @, z, 'E', s, @, n]).
entry(gesetzt, [g, @, z, 'E', ts, t]).
entry(gesichert, [g, @, z, 'I', 'C', '6', t]).
entry('Gesicht', [g, @, z, 'I', 'C', t]).
entry('Gesichtspunkt', [g, @, z, 'I', 'C', ts, p, 'U', 'N', k, t]).
entry(gesondert, [g, @, z, 'O', n, d, '6', t]).
entry(gesorgt, [g, @, z, 'O', '6', k, t]).
entry(gespannt, [g, @, 'S', p, a, n, t]).
entry(gespart, [g, @, 'S', p, 'a:', r, t]).
entry(gespeichert, [g, @, 'S', p, aI, 'C', '6', t]).
entry(gespeist, [g, @, 'S', p, aI, s, t]).
entry(gespielt, [g, @, 'S', p, 'i:', l, t]).
entry('Gespräche', [g, @, 'S', p, r, 'E:', 'C', @]).
entry('Gesprächen', [g, @, 'S', p, r, 'E:', 'C', @, n]).
entry('Gespräches', [g, @, 'S', p, r, 'E:', 'C', @, s]).
entry('Gespräch', [g, @, 'S', p, r, 'E:', 'C']).
entry(gesprächig, [g, @, 'S', p, r, 'E:', 'C', 'I', 'C']).
entry('Gesprächsleitung', [g, @, 'S', p, r, 'E:', 'C', s, l, aI, t, 'U', 'N']).
entry('Gesprächstermine', [g, @, 'S', p, r, 'E:', 'C', s, t, 'E', '6', m, 'i:', n, @]).
entry('Gesprächstermin', [g, @, 'S', p, r, 'E:', 'C', s, t, 'E', '6', m, 'i:', n]).
entry('Gesprächts', [g, @, 'S', p, r, 'E:', 'C', ts]).
entry('Gesprä', [g, @, 'S', p, r, 'E:']).
entry(gesprochen, [g, @, 'S', p, r, 'O', x, @, n]).
entry(gesse, [g, 'E', s, @]).
entry(gestaffelt, [g, @, 'S', t, a, f, 'E', l, t]).
entry(gestalten, [g, @, 'S', t, a, l, t, @, n]).
entry(gestaltet, [g, @, 'S', t, a, l, t, @, t]).
entry(gestaltung, [g, @, 'S', t, a, l, t, 'U', 'N']).
entry(gestaltungsmöglichkeiten, [g, @, 'S', t, a, l, t, 'U', 'N', s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry(gestanden, [g, @, 'S', t, a, n, d, @, n]).
entry(gestärkt, [g, @, 'S', t, 'E', '6', k, t]).
entry(gestatten, [g, @, 'S', t, a, t, @, n]).
entry(gesteckt, [g, @, 'S', t, 'E', k, t]).
entry(gestehen, [g, @, 'S', t, 'e:', @, n]).
entry(gestellt, [g, @, 'S', t, 'E', l, t]).
entry(gestern, [g, 'E', s, t, '6', n]).
entry(gestolpert, [g, @, 'S', t, 'O', l, p, '6', t]).
entry(gestopft, [g, @, 'S', t, 'O', pf, t]).
entry(gestorben, [g, @, 'S', t, 'O', '6', b, @, n]).
entry(gestraffte, [g, @, 'S', t, r, a, f, t, @]).
entry(gestrafft, [g, @, 'S', t, r, a, f, t]).
entry(gestreßt, [g, @, 'S', t, r, 'E', s, t]).
entry(gestrichen, [g, @, 'S', t, r, 'I', 'C', @, n]).
entry(gesucht, [g, @, z, 'u:', x, t]).
entry(gesug, [g, @, z, 'u:', k]).
entry(gesüks, [g, @, z, 'Y', k, s]).
entry(gesunden, [g, @, z, 'U', n, d, @, n]).
entry(getaner, [g, @, t, 'a:', n, '6']).
entry(getan, [g, @, t, 'a:', n]).
entry(getäuscht, [g, @, t, 'OY', 'S', t]).
entry(getragen, [g, @, t, r, 'a:', g, @, n]).
entry('Getränken', [g, @, t, r, 'E', 'N', k, @, n]).
entry(getrennte, [g, @, t, r, 'E', n, t, @]).
entry(getrennt, [g, @, t, r, 'E', n, t]).
entry(getroffen, [g, @, t, r, 'O', f, @, n]).
entry(getrost, [g, @, t, r, 'o:', s, t]).
entry(gewan, [g, @, v, a, n]).
entry(gewartet, [g, @, v, a, r, t, @, t]).
entry(gewaschen, [g, @, v, a, 'S', @, n]).
entry(gewesen, [g, @, v, 'e:', z, @, n]).
entry('Gewinn', [g, @, v, 'I', n]).
entry(gewisse, [g, @, v, 'I', s, @]).
entry(gewissen, [g, @, v, 'I', s, @, n]).
entry(gewissermaßen, [g, @, v, 'I', s, '6', m, 'a:', s, @, n]).
entry(gewöhnen, [g, @, v, '2:', n, @, n]).
entry(gewohnheitsgemäß, [g, @, v, 'o:', n, h, aI, ts, g, @, m, 'E:', s]).
entry(gewöhnliche, [g, @, v, '2:', n, l, 'I', 'C', @]).
entry(gewöhnlich, [g, @, v, '2:', n, l, 'I', 'C']).
entry(gewöhnt, [g, @, v, '2:', n, t]).
entry(gewohnt, [g, @, v, 'o:', n, t]).
entry(gewonnen, [g, @, v, 'O', n, @, n]).
entry(geworden, [g, @, v, 'O', '6', d, @, n]).
entry(gewußt, [g, @, v, 'U', s, t]).
entry(gezahlt, [g, @, ts, 'a:', l, t]).
entry(gezapftes, [g, @, ts, a, pf, t, @, s]).
entry(gezeigt, [g, @, ts, aI, k, t]).
entry(gf, [g, f]).
entry('Ghelp', [g, 'E', l, p]).
entry(gib, [g, 'i:', p]).
entry(gibt, [g, 'i:', p, t]).
entry('Gießen', [g, 'i:', s, @, n]).
entry('Gildehof', [g, 'I', l, d, @, h, 'o:', f]).
entry('Gildenhof', [g, 'I', l, d, @, n, h, 'o:', f]).
entry(gilt, [g, 'I', l, t]).
entry(ginge, [g, 'I', 'N', @]).
entry(gingen, [g, 'I', 'N', @, n]).
entry(ging, [g, 'I', 'N']).
entry('Giora', [d, 'Z', o, r, a]).
entry(gla, [g, l, a]).
entry(glänzende, [g, l, 'E', n, ts, @, n, d, @]).
entry('Gläschen', [g, l, 'E:', s, 'C', @, n]).
entry('Glas', [g, l, 'a:', s]).
entry('Glassenhart', [g, l, a, s, @, n, h, a, r, t]).
entry(glatt, [g, l, a, t]).
entry(glaube, [g, l, aU, b, @]).
entry(glauben, [g, l, aU, b, @, n]).
entry(glaubst, [g, l, aU, p, s, t]).
entry(gleiche, [g, l, aI, 'C', @]).
entry(gleichen, [g, l, aI, 'C', @, n]).
entry(gleicher, [g, l, aI, 'C', '6']).
entry(gleiches, [g, l, aI, 'C', @, s]).
entry(gleichfalls, [g, l, aI, 'C', f, a, l, s]).
entry(gleich, [g, l, aI, 'C']).
entry(gleichgültig, [g, l, aI, 'C', g, 'Y', l, t, 'I', 'C']).
entry(gleichmäßig, [g, l, aI, 'C', m, 'E:', s, 'I', 'C']).
entry(gleichnamige, [g, l, aI, 'C', n, 'a:', m, 'I', g, @]).
entry(gleichwertig, [g, l, aI, 'C', v, 'e:', '6', t, 'I', 'C']).
entry(gleichzeitig, [g, l, aI, 'C', ts, aI, t, 'I', 'C']).
entry('Gleis', [g, l, aI, s]).
entry('Gleittag', [g, l, aI, t, t, 'a:', k]).
entry(gl, [g, l]).
entry(gliedern, [g, l, 'i:', d, '6', n]).
entry('Glöde', [g, l, '2:', d, @]).
entry('Glögner', [g, l, '9', k, n, '6']).
entry('Glotze', [g, l, 'O', ts, @]).
entry('Glück', [g, l, 'Y', k]).
entry('Glückliche', [g, l, 'Y', k, l, 'I', 'C', @]).
entry(glücklicherweise, [g, l, 'Y', k, l, 'I', 'C', '6', v, aI, z, @]).
entry(glücklich, [g, l, 'Y', k, l, 'I', 'C']).
entry('Glückwünsche', [g, l, 'Y', k, v, 'Y', n, 'S', @]).
entry('Glückwunsch', [g, l, 'Y', k, v, 'U', n, 'S']).
entry('Goethestraße', [g, '2:', t, @, 'S', t, r, 'a:', s, @]).
entry('Go', [g, 'o:']).
entry(goldene, [g, 'O', l, d, @, n, @]).
entry('Goldenen-Adler', [g, 'O', l, d, @, n, @, n, 'a:', d, l, '6']).
entry(golden, [g, 'O', l, d, @, n]).
entry('Golfspielen', [g, 'O', l, f, 'S', p, 'i:', l, @, n]).
entry(gönnen, [g, '9', n, @, n]).
entry(gönnt, [g, '9', n, t]).
entry(good, [g, 'u:', d]).
entry('Görgen', [g, '9', '6', g, @, n]).
entry('Goslar', [g, 'O', s, l, a, r]).
entry('Gospelkonzert', [g, 'O', s, p, @, l, k, 'O', n, ts, 'E', '6', t]).
entry('Gossens', [g, 'O', s, @, n, s]).
entry('Gotha', [g, 'o:', t, a]).
entry('Gottchen', [g, 'O', t, 'C', @, n]).
entry('Gottes', [g, 'O', t, @, s]).
entry('Gott', [g, 'O', t]).
entry('Göttingen', [g, '9', t, 'I', 'N', @, n]).
entry('Gottschalk', [g, 'O', tS, a, l, k]).
entry('Grab', [g, r, 'a:', p]).
entry(grade, [g, r, 'a:', d, @]).
entry('Gramlich', [g, r, a, m, l, 'I', 'C']).
entry('Gran-Canaria', [g, r, a, n, k, a, n, a, r, j, a]).
entry('Grand-Canyon', [g, r, 'E', n, t, k, 'E', n, j, @, n]).
entry('Grand-Hotel', [g, r, 'a~', h, o, t, 'E', l]).
entry(granteln, [g, r, a, n, t, @, l, n]).
entry(grauenvoll, [g, r, aU, @, n, f, 'O', l]).
entry('Graus', [g, r, aU, s]).
entry('Grausliges', [g, r, aU, s, l, 'I', g, @, s]).
entry(graut, [g, r, aU, t]).
entry(greife, [g, r, aI, f, @]).
entry(greifen, [g, r, aI, f, @, n]).
entry('Greim', [g, r, aI, m]).
entry('Grenze', [g, r, 'E', n, ts, @]).
entry('Grenzen', [g, r, 'E', n, ts, @, n]).
entry(gr, [g, r]).
entry('Griechen', [g, r, 'i:', 'C', @, n]).
entry('Griechstrasse', [g, r, 'i:', k, s, t, r, 'a:', s, @]).
entry('Griff', [g, r, 'I', f]).
entry('Grobe', [g, r, 'o:', b, @]).
entry(grob, [g, r, 'o:', p]).
entry(großartiges, [g, r, 'o:', s, 'a:', r, t, 'I', g, @, s]).
entry(großartig, [g, r, 'o:', s, 'a:', r, t, 'I', 'C']).
entry(größe, [g, r, '2:', s, @]).
entry(große, [g, r, 'o:', s, @]).
entry(großen, [g, r, 'o:', s, @, n]).
entry(größere, [g, r, '2:', s, @, r, @]).
entry(größeren, [g, r, '2:', s, @, r, @, n]).
entry(größeres, [g, r, '2:', s, @, r, @, s]).
entry(größer, [g, r, '2:', s, '6']).
entry(großer, [g, r, 'o:', s, '6']).
entry(großes, [g, r, 'o:', s, @, s]).
entry(groß, [g, r, 'o:', s]).
entry('Großheim', [g, r, 'o:', s, h, aI, m]).
entry('Großmutter', [g, r, 'o:', s, m, 'U', t, '6']).
entry('Großraumwagen', [g, r, 'o:', s, r, aU, m, v, 'a:', g, @, n]).
entry(großspurig, [g, r, 'o:', s, 'S', p, 'u:', r, 'I', 'C']).
entry('Großstadt', [g, r, 'o:', s, 'S', t, a, t]).
entry(größte, [g, r, '2:', s, t, @]).
entry('Großteil', [g, r, 'o:', s, t, aI, l]).
entry(größten, [g, r, '2:', s, t, @, n]).
entry(größtenteils, [g, r, '2:', s, t, @, n, t, aI, l, s]).
entry(größtes, [g, r, '2:', s, t, @, s]).
entry('Großvater', [g, r, 'o:', s, f, 'a:', t, '6']).
entry('Großwagen', [g, r, 'o:', s, v, 'a:', g, @, n]).
entry(großzügig, [g, r, 'o:', s, ts, 'y:', g, 'I', 'C']).
entry('Groth', [g, r, 'o:', t]).
entry('Grubenhagen', [g, r, 'u:', b, @, n, h, 'a:', g, @, n]).
entry('Gruber', [g, r, 'u:', b, '6']).
entry(grud, [g, r, 'u:', t]).
entry(grü, [g, r, y]).
entry('Grunde', [g, r, 'U', n, d, @]).
entry('Gründen', [g, r, 'Y', n, d, @, n]).
entry('Grund', [g, r, 'U', n, t]).
entry(grundlegend, [g, r, 'U', n, t, l, 'e:', g, @, n, t]).
entry(gründlich, [g, r, 'Y', n, t, l, 'I', 'C']).
entry('Grundsatz', [g, r, 'U', n, t, z, a, ts]).
entry(grundsätzlich, [g, r, 'U', n, t, z, 'E', ts, l, 'I', 'C']).
entry(grünen, [g, r, 'y:', n, @, n]).
entry('Gruppe', [g, r, 'U', p, @]).
entry('Gruppentreffen', [g, r, 'U', p, @, n, t, r, 'E', f, @, n]).
entry(grüße, [g, r, 'y:', s, @]).
entry(gruselt, [g, r, 'u:', z, @, l, t]).
entry(grüßen, [g, r, 'y:', s, @, n]).
entry(grüß, [g, r, 'y:', s]).
entry('Grusitsch', [g, r, 'u:', z, 'I', tS]).
entry(gsch, [k, 'S']).
entry(gs, [k, s]).
entry(gsten, [k, s, t, @, n]).
entry(gucke, [g, 'U', k, @]).
entry(gucken, [g, 'U', k, @, n]).
entry('Gudrun', [g, 'u:', d, r, 'u:', n]).
entry(gu, [g, 'U']).
entry('Guido', [g, 'i:', d, o]).
entry(gul, [g, 'U', l]).
entry(gültig, [g, 'Y', l, t, 'I', 'C']).
entry('Gültigkeitsdauer', [g, 'Y', l, t, 'I', 'C', k, aI, ts, d, aU, '6']).
entry(gün, [g, 'y:', n]).
entry('Gunreben', [g, 'U', n, r, 'e:', b, @, n]).
entry(günstige, [g, 'Y', n, s, t, 'I', g, @]).
entry(günstigen, [g, 'Y', n, s, t, 'I', g, @, n]).
entry(günstigere, [g, 'Y', n, s, t, 'I', g, @, r, @]).
entry(günstigeren, [g, 'Y', n, s, t, 'I', g, @, r, @, n]).
entry(günstigeres, [g, 'Y', n, s, t, 'I', g, @, r, @, s]).
entry(günstiger, [g, 'Y', n, s, t, 'I', g, '6']).
entry(günstigerweise, [g, 'Y', n, s, t, 'I', g, '6', v, aI, z, @]).
entry(günstiges, [g, 'Y', n, s, t, 'I', g, @, s]).
entry(günstig, [g, 'Y', n, s, t, 'I', 'C']).
entry(günstigste, [g, 'Y', n, s, t, 'I', 'C', s, t, @]).
entry(günstigsten, [g, 'Y', n, s, t, 'I', 'C', s, t, @, n]).
entry('Günte', [g, 'Y', n, t, @]).
entry('Günter', [g, 'Y', n, t, '6']).
entry('Gunzenhäuser', [g, 'U', n, ts, @, n, h, 'OY', z, '6']).
entry('Gürtler', [g, 'Y', '6', t, l, '6']).
entry('Gürtner', [g, 'Y', '6', t, n, '6']).
entry('Gustav', [g, 'U', s, t, a, f]).
entry(gust, [g, 'U', s, t]).
entry(gute, [g, 'u:', t, @]).
entry('Güte', [g, 'y:', t, @]).
entry(gutem, [g, 'u:', t, @, m]).
entry('Guten-Flug', [g, 'u:', t, @, n, f, l, 'u:', k]).
entry(guten, [g, 'u:', t, @, n]).
entry(guter, [g, 'u:', t, '6']).
entry('Gütersloh', [g, 'y:', t, '6', s, l, 'o:']).
entry(gutes, [g, 'u:', t, @, s]).
entry(gutgehen, [g, 'u:', t, g, 'e:', @, n]).
entry(gutgeschrieben, [g, 'u:', t, g, @, 'S', r, 'i:', b, @, n]).
entry(gut, [g, 'u:', t]).
entry(haarig, [h, 'a:', r, 'I', 'C']).
entry(haarknapp, [h, 'a:', r, k, n, a, p]).
entry(haarscharf, [h, 'a:', r, 'S', a, r, f]).
entry('Haas', [h, 'a:', s]).
entry(habe, [h, 'a:', b, @]).
entry(haben, [h, 'a:', b, @, n]).
entry(hab, [h, 'a:', p]).
entry(habm, [h, 'a:', b, m]).
entry(habt, [h, 'a:', p, t]).
entry(hach, [h, a, x]).
entry('Hackenberg', [h, a, k, @, n, b, 'E', '6', k]).
entry('Häfelin', [h, 'E', f, @, l, i, n]).
entry(hafen, [h, 'a:', f, @, n]).
entry('Häflin', [h, 'E', f, l, i, n]).
entry('Hagen', [h, 'a:', g, @, n]).
entry(ha, [h, a]).
entry(hä, [h, 'E']).
entry(hakt, [h, 'a:', k, t]).
entry(halbe, [h, a, l, b, @]).
entry(halben, [h, a, l, b, @, n]).
entry(halber, [h, a, l, b, '6']).
entry('Halbes', [h, a, l, b, @, s]).
entry(halb, [h, a, l, p]).
entry('Halbpension', [h, a, l, p, p, 'E', n, z, j, 'o:', n]).
entry('Halbtag', [h, a, l, p, t, 'a:', k]).
entry(halbtägig, [h, a, l, p, t, 'E:', g, 'I', 'C']).
entry(halbwegs, [h, a, l, p, v, 'e:', k, s]).
entry(hälfte, [h, 'E', l, f, t, @]).
entry('Hälften', [h, 'E', l, f, t, @, n]).
entry(hal, [h, a, l]).
entry(halle, [h, a, l, @]).
entry('Hallenbad', [h, a, l, @, n, b, 'a:', t]).
entry('Hallen', [h, a, l, @, n]).
entry('Haller', [h, a, l, '6']).
entry(halli, [h, a, l, 'i:']).
entry('Hallmut', [h, a, l, m, 'u:', t]).
entry(hallo, [h, a, l, o]).
entry('Halser', [h, a, l, z, '6']).
entry(halte, [h, a, l, t, @]).
entry(halten, [h, a, l, t, 'E', n]).
entry('Haltestelle', [h, a, l, t, @, 'S', t, 'E', l, @]).
entry(halt, [h, a, l, t]).
entry(hält, [h, 'E', l, t]).
entry(hältst, [h, 'E', l, ts, t]).
entry('Hamburger', [h, a, m, b, 'U', '6', g, '6']).
entry('Hamburg', [h, a, m, b, 'U', '6', k]).
entry('Ham', [h, a, m]).
entry(ham, [h, 'E', m]).
entry('Hamlet', [h, a, m, l, @, t]).
entry('Hammerstein', [h, a, m, '6', 'S', t, aI, n]).
entry('Hammerstingl', [h, a, m, '6', 'S', t, 'I', 'N', l]).
entry('Hammwi', [h, a, m, v, i]).
entry(hanashite, [h, a, n, a, s, 'i:', t, @]).
entry(handeln, [h, a, n, d, @, l, n]).
entry(handelt, [h, a, n, d, @, l, t]).
entry(handhaben, [h, a, n, t, h, 'a:', b, @, n]).
entry(hand, [h, a, n, t]).
entry('Han', [h, a, n]).
entry('Handke', [h, a, n, t, k, @]).
entry('Handy', [h, 'E', n, d, i]).
entry(hängenden, [h, 'E', 'N', @, n, d, @, n]).
entry(hängt, [h, 'E', 'N', k, t]).
entry('Hannes', [h, a, n, @, s]).
entry('Hanno', [h, a, n, 'o:']).
entry('Hannoveraner', [h, a, n, o, v, @, r, 'a:', n, '6']).
entry('Hannoveranern', [h, a, n, o, v, @, r, 'a:', n, '6', n]).
entry('Hannover-Aufenthalt', [h, a, n, 'o:', f, '6', aU, f, 'E', n, t, h, a, l, t]).
entry('Hannoverer', [h, a, n, o, f, @, r, '6']).
entry('Hannover-Fahrt', [h, a, n, 'o:', f, '6', f, 'a:', r, t]).
entry('Hannover', [h, a, n, 'o:', f, '6']).
entry('Hannoverische', [h, a, n, 'o:', f, @, r, 'I', 'S', @]).
entry(hannoverischen, [h, a, n, 'o:', f, @, r, 'I', 'S', @, n]).
entry('Hannover-Kunsthalle', [h, a, n, 'o:', f, '6', k, 'U', n, s, t, h, a, l, @]).
entry('Hannover-Messe', [h, a, n, 'o:', f, '6', m, 'E', s, @]).
entry('Hannover-Reise', [h, a, n, 'o:', f, '6', r, aI, z, @]).
entry('Hannover-Sache', [h, a, n, 'o:', f, '6', z, a, x, @]).
entry('Hannoversche', [h, a, n, 'o:', f, 'E', '6', 'S', @]).
entry('Hannovers', [h, a, n, 'o:', f, '6', s]).
entry('Hannover-Termin', [h, a, n, 'o:', f, '6', t, 'E', '6', m, 'i:', n]).
entry('Hannover-Theater', [h, a, n, 'o:', f, '6', t, e, 'a:', t, '6']).
entry('Hannov', [h, a, n, 'o:', f]).
entry('Hansebäcker', [h, a, n, z, @, b, 'E', k, '6']).
entry('Hanseles', [h, a, n, z, @, l, @, s]).
entry('Hans', [h, a, n, s]).
entry('Hans-Jörg', [h, a, n, s, j, '9', '6', k]).
entry('Happen', [h, a, p, @, n]).
entry(happig, [h, a, p, 'I', 'C']).
entry(hare, [h, 'a:', r, @]).
entry(harte, [h, a, r, t, @]).
entry(harten, [h, a, r, t, @, n]).
entry(hart, [h, a, r, t]).
entry('Härtl', [h, 'E', '6', t, l]).
entry('Hartmann', [h, a, r, t, m, a, n]).
entry('Haselbeck', [h, 'a:', z, @, l, b, 'E', k]).
entry('Hasel', [h, 'a:', z, @, l]).
entry(häßlich, [h, 'E', s, l, 'I', 'C']).
entry(hasse, [h, a, s, @]).
entry('Hassenrik', [h, a, s, @, n, r, 'I', k]).
entry(hast, [h, a, s, t]).
entry(hat, [h, a, t]).
entry(hät, [h, 'E', t]).
entry(hatsen, [h, a, ts, @, n]).
entry(hatte, [h, a, t, @]).
entry(hätte, [h, 'E', t, @]).
entry(hatten, [h, a, t, @, n]).
entry(hätten, [h, 'E', t, @, n]).
entry(hättes, [h, 'E', t, @, s]).
entry(hattest, [h, a, t, @, s, t]).
entry(hättest, [h, 'E', t, @, s, t]).
entry(hauen, [h, aU, @, n]).
entry(häufiger, [h, 'OY', f, 'I', g, '6']).
entry(häufig, [h, 'OY', f, 'I', 'C']).
entry('Hau', [h, aU]).
entry('Haumacher', [h, aU, m, a, x, '6']).
entry('Hauptbah', [h, @, 'U', p, t, b, @]).
entry('Hauptbahnhofes', [h, aU, p, t, b, 'a:', n, h, 'o:', f, @, s]).
entry('Hauptbahnhof-Gegend', [h, aU, p, t, b, 'a:', n, h, 'o:', f, g, 'e:', g, @, n, t]).
entry('Hauptbahnhof', [h, aU, p, t, b, 'a:', n, h, 'o:', f]).
entry('Hauptbahnhofnähe', [h, aU, p, t, b, 'a:', n, h, 'o:', f, n, 'E:', @]).
entry('Hauptbahnhofs', [h, aU, p, t, b, 'a:', n, h, 'o:', f, s]).
entry('Haupteingang', [h, aU, p, t, aI, n, g, a, 'N']).
entry('Hauptfiliale', [h, aU, p, t, f, i, l, j, 'a:', l, @]).
entry('Hauptgebäude', [h, aU, p, t, g, @, b, 'OY', d, @]).
entry('Haupt', [h, aU, p, t]).
entry('Haupthaus', [h, aU, p, t, h, aU, s]).
entry('Hauptinformation', [h, aU, p, t, 'I', n, f, 'O', '6', m, a, ts, j, 'o:', n]).
entry('Hauptkalender', [h, aU, p, t, k, a, l, 'E', n, d, '6']).
entry('Hauptreisezeit', [h, aU, p, t, r, aI, z, @, ts, aI, t]).
entry('Hauptsache', [h, aU, p, t, z, a, x, @]).
entry(hauptsächlich, [h, aU, p, t, z, 'E', 'C', l, 'I', 'C']).
entry(hauptsä, [h, aU, p, t, z, 'E']).
entry('Hauptschule', [h, aU, p, tS, 'u:', l, @]).
entry('Hauptstraße', [h, aU, p, tS, t, r, 'a:', s, @]).
entry('Haupttermin', [h, aU, p, t, t, 'E', '6', m, 'i:', n]).
entry('Hausaufgaben', [h, aU, s, aU, f, g, 'a:', b, @, n]).
entry('Hause', [h, aU, z, @]).
entry('Hauser', [h, aU, z, '6']).
entry('Häuser', [h, 'OY', z, '6']).
entry('Hauses', [h, aU, z, @, s]).
entry('Hausfrau', [h, aU, s, f, r, aU]).
entry('Haushalt', [h, aU, s, h, a, l, t]).
entry('Haus', [h, aU, s]).
entry('Hausmeister', [h, aU, s, m, aI, s, t, '6']).
entry('Hausseminar', [h, aU, s, z, e, m, 'I', n, 'a:', r]).
entry('Haustür', [h, aU, s, t, 'y:', '6']).
entry(haut, [h, aU, t]).
entry(hechele, [h, 'E', 'C', @, l, @]).
entry('Heck', [h, 'E', k]).
entry('Hector', [h, 'E', k, t, 'o:', '6']).
entry('Heftchen', [h, 'E', f, t, 'C', @, n]).
entry(heftig, [h, 'E', f, t, 'I', 'C']).
entry('Hegener', [h, 'e:', g, @, n, '6']).
entry('Heidelberg', [h, aI, d, @, l, b, 'E', '6', k]).
entry('Heiduschke', [h, aI, d, 'U', 'S', k, @]).
entry(heijeijei, [h, aI, j, aI, j, aI]).
entry('Heiko', [h, aI, k, o]).
entry('Heilbronn', [h, aI, l, b, r, 'O', n]).
entry('Heiligabend', [h, aI, l, 'I', 'C', 'a:', b, @, n, t]).
entry('Heilige-Abend', [h, aI, l, 'I', g, @, 'a:', b, @, n, t]).
entry('Heilige-Drei-Könige', [h, aI, l, 'I', g, @, d, r, aI, k, '2:', n, 'I', g, @]).
entry('Heiligen-Abend', [h, aI, l, 'I', g, @, n, 'a:', b, @, n, t]).
entry('Heiligen-Drei-Königen', [h, aI, l, 'I', g, @, n, d, r, aI, k, '2:', n, 'I', g, @, n]).
entry(heiliger, [h, aI, l, 'I', g, '6']).
entry(heilig, [h, aI, l, 'I', 'C']).
entry('Heilmann', [h, aI, l, m, a, n]).
entry('Heimat', [h, aI, m, 'a:', t]).
entry('Heimatstadt', [h, aI, m, 'a:', tS, t, a, t]).
entry('Heimberg', [h, aI, m, b, 'E', '6', k]).
entry(heimfahren, [h, aI, m, f, 'a:', r, @, n]).
entry('Heimfahrt', [h, aI, m, f, 'a:', r, t]).
entry(heimfliegen, [h, aI, m, f, l, 'i:', g, @, n]).
entry(heim, [h, aI, m]).
entry(heimkomme, [h, aI, m, k, 'O', m, @]).
entry(heimkommen, [h, aI, m, k, 'O', m, @, n]).
entry(heimlichen, [h, aI, m, l, 'I', 'C', @, n]).
entry('Heimreise', [h, aI, m, r, aI, z, @]).
entry('Heinbach', [h, aI, n, b, a, x]).
entry('Heinicke', [h, aI, n, 'I', k, @]).
entry('Heinlein', [h, aI, n, l, aI, n]).
entry('Heinrich', [h, aI, n, r, 'I', 'C']).
entry('Heinrich-von-Kleists', [h, aI, n, r, 'I', 'C', f, 'O', n, k, l, aI, s, ts]).
entry(heiratet, [h, aI, r, a, t, @, t]).
entry(heiße, [h, aI, s, @]).
entry(heißen, [h, aI, s, @, n]).
entry(heiß, [h, aI, s]).
entry(heißt, [h, aI, s, t]).
entry('Hektik', [h, 'E', k, t, 'I', k]).
entry(hektisch, [h, 'E', k, t, 'I', 'S']).
entry(helfen, [h, 'E', l, f, @, n]).
entry('Helfer', [h, 'E', l, f, '6']).
entry(hello, [h, @, l, @, 'U']).
entry('Helsinki', [h, 'E', l, z, 'I', 'N', k, i]).
entry('Hempel', [h, 'E', m, p, @, l]).
entry(hende, [h, 'E', n, d, @]).
entry('Henrici', [h, 'E', n, r, i, tS, i]).
entry('Henri', [h, 'E', n, r, i]).
entry(herannaht, [h, 'E', r, a, n, n, 'a:', t]).
entry(heranziehen, [h, 'E', r, a, n, ts, 'i:', @, n]).
entry(herausfinden, [h, 'E', r, aU, s, f, 'I', n, d, @, n]).
entry(herausgefunden, [h, 'E', r, aU, s, g, @, f, 'U', n, d, @, n]).
entry(herausgestellt, [h, 'E', r, aU, s, g, @, 'S', t, 'E', l, t]).
entry(herausgesucht, [h, 'E', r, aU, s, g, @, z, 'u:', x, t]).
entry(heraus, [h, 'E', r, aU, s]).
entry(herausragende, [h, 'E', r, aU, s, r, 'a:', g, @, n, d, @]).
entry(heraussuchen, [h, 'E', r, aU, s, z, 'u:', x, @, n]).
entry(herauszusuchen, [h, 'E', r, aU, s, ts, 'u:', z, 'u:', x, @, n]).
entry('Herbstferien', [h, 'E', '6', p, s, t, f, 'e:', '6', j, @, n]).
entry('Herbst', [h, 'E', '6', p, s, t]).
entry('Herbsturlaub', [h, 'E', '6', p, s, t, 'u:', '6', l, aU, p]).
entry('Herfahrerei', [h, 'e:', '6', f, 'a:', r, @, r, aI]).
entry(hergerichtet, [h, 'e:', '6', g, @, r, 'I', 'C', t, @, t]).
entry(hergestellt, [h, 'e:', '6', g, @, 'S', t, 'E', l, t]).
entry(her, [h, 'e:', '6']).
entry(hernach, [h, 'E', '6', n, 'a:', x]).
entry(hernehmen, [h, 'e:', '6', n, 'e:', m, @, n]).
entry('Herrenhausen', [h, 'E', r, @, n, h, aU, z, @, n]).
entry('Herren', [h, 'E', r, @, n]).
entry('Herr', [h, 'E', '6']).
entry(herrje, [h, 'E', '6', j, 'e:']).
entry(herrliches, [h, 'E', '6', l, 'I', 'C', @, s]).
entry('Herrn', [h, 'E', '6', n]).
entry('Herrschaftszeiten', [h, 'E', '6', 'S', a, f, ts, ts, aI, t, @, n]).
entry(herrscht, [h, 'E', '6', 'S', t]).
entry(herschieben, [h, 'e:', '6', 'S', 'i:', b, @, n]).
entry(herstellen, [h, 'e:', '6', 'S', t, 'E', l, @, n]).
entry(herüber, [h, 'E', r, 'y:', b, '6']).
entry(herumärgern, [h, 'E', r, 'U', m, 'E', '6', g, '6', n]).
entry(herum, [h, 'E', r, 'U', m]).
entry(herumkommen, [h, 'E', r, 'U', m, k, 'O', m, @, n]).
entry(herumkutschen, [h, 'E', r, 'U', m, k, 'U', tS, @, n]).
entry(hervor, [h, 'E', '6', f, 'o:', '6']).
entry(hervorholen, [h, 'E', '6', f, 'o:', '6', h, 'o:', l, @, n]).
entry(hervorragende, [h, 'E', '6', f, 'o:', '6', r, 'a:', g, @, n, d, @]).
entry(hervorragendes, [h, 'E', '6', f, 'o:', '6', r, 'a:', g, @, n, d, @, s]).
entry(hervorragend, [h, 'E', '6', f, 'o:', '6', r, 'a:', g, @, n, t]).
entry('Herzen', [h, 'E', '6', ts, @, n]).
entry('Herz', [h, 'E', '6', ts]).
entry(herzliche, [h, 'E', '6', ts, l, 'I', 'C', @]).
entry(herzlichen, [h, 'E', '6', ts, l, 'I', 'C', @, n]).
entry(herzlich, [h, 'E', '6', ts, l, 'I', 'C']).
entry(herzukommen, [h, 'e:', '6', ts, u, k, 'O', m, @, n]).
entry('Hessler', [h, 'E', s, l, '6']).
entry('Hessner', [h, 'E', s, n, '6']).
entry('Hetze', [h, 'E', ts, @]).
entry(hetzen, [h, 'E', ts, @, n]).
entry(heute, [h, 'OY', t, @]).
entry(heutige, [h, 'OY', t, 'I', g, @]).
entry(heutigen, [h, 'OY', t, 'I', g, @, n]).
entry(heutzutage, [h, 'OY', t, ts, u, t, 'a:', g, @]).
entry(hey, [h, 'e:']).
entry(h, [h, 'a:']).
entry(hib, [h, 'I', p]).
entry('Hickhack', [h, 'I', k, h, a, k]).
entry(hie, [h, 'i:']).
entry(hielten, [h, 'i:', l, t, @, n]).
entry(hierbei, [h, 'i:', '6', b, aI]).
entry(hierfür, [h, 'i:', '6', f, 'y:', '6']).
entry(hierher, [h, 'i:', '6', h, 'e:', '6']).
entry(hier, [h, 'i:', '6']).
entry(hierlasse, [h, 'i:', '6', l, a, s, @]).
entry(hiermit, [h, 'i:', '6', m, 'I', t]).
entry(hieße, [h, 'i:', s, @]).
entry(hießen, [h, 'i:', s, @, n]).
entry(hieß, [h, 'i:', s]).
entry(hiesigen, [h, 'i:', z, 'I', g, @, n]).
entry(hi, [h, i]).
entry('Hild', [h, 'I', l, t]).
entry('Hilfe', [h, 'I', l, f, @]).
entry('Hilfeschrei', [h, 'I', l, f, @, 'S', r, aI]).
entry(hilfreich, [h, 'I', l, f, r, aI, 'C']).
entry(hilfsbereit, [h, 'I', l, f, s, b, @, r, aI, t]).
entry(hilft, [h, 'I', l, f, t]).
entry('Hillers', [h, 'I', l, '6', s]).
entry('Hillmeyer', [h, 'I', l, m, aI, '6']).
entry('Himmelfahrt', [h, 'I', m, @, l, f, 'a:', r, t]).
entry('Himmel', [h, 'I', m, @, l]).
entry('Himmels', [h, 'I', m, @, l, s]).
entry(hinaus, [h, 'I', n, aU, s]).
entry('Hinausschieben', [h, 'I', n, aU, s, 'S', 'i:', b, @, n]).
entry(hinausverlegen, [h, 'I', n, aU, s, f, 'E', '6', l, 'e:', g, @, n]).
entry(hinbegeben, [h, 'I', n, b, @, g, 'e:', b, @, n]).
entry(hinbekomme, [h, 'I', n, b, @, k, 'O', m, @]).
entry(hinbekommen, [h, 'I', n, b, @, k, 'O', m, @, n]).
entry(hinbiegen, [h, 'I', n, b, 'i:', g, @, n]).
entry(hinbringe, [h, 'I', n, b, r, 'I', 'N', @]).
entry(hinbringen, [h, 'I', n, b, r, 'I', 'N', @, n]).
entry(hindurch, [h, 'I', n, d, 'U', '6', 'C']).
entry(hindurchzuführen, [h, 'I', n, d, 'U', '6', 'C', ts, u, f, 'y:', r, @, n]).
entry(hineingehen, [h, 'I', n, aI, n, g, 'e:', @, n]).
entry(hineingeht, [h, 'I', n, aI, n, g, 'e:', t]).
entry(hinein, [h, 'I', n, aI, n]).
entry(hineinlegen, [h, 'I', n, aI, n, l, 'e:', g, @, n]).
entry(hineinnehmen, [h, 'I', n, aI, n, n, 'e:', m, @, n]).
entry(hineinpassen, [h, 'I', n, aI, n, p, a, s, @, n]).
entry(hineinquetschen, [h, 'I', n, aI, n, k, v, 'E', tS, @, n]).
entry(hineinschauen, [h, 'I', n, aI, n, 'S', aU, @, n]).
entry(hineinstopft, [h, 'I', n, aI, n, 'S', t, 'O', pf, t]).
entry(hinfahren, [h, 'I', n, f, 'a:', r, @, n]).
entry(hinfahr, [h, 'I', n, f, 'a:', r]).
entry('Hinfahrt', [h, 'I', n, f, 'a:', r, t]).
entry(hinfährt, [h, 'I', n, f, 'E:', r, t]).
entry(hinf, [h, 'I', n, f]).
entry(hinfliegen, [h, 'I', n, f, l, 'i:', g, @, n]).
entry('Hinflüge', [h, 'I', n, f, l, 'y:', g, @]).
entry('Hinflug', [h, 'I', n, f, l, 'u:', k]).
entry(hinführen, [h, 'I', n, f, 'y:', r, @, n]).
entry(hingefahren, [h, 'I', n, g, @, f, 'a:', r, @, n]).
entry(hingegen, [h, 'I', n, g, 'e:', g, @, n]).
entry(hingehe, [h, 'I', n, g, 'e:', @]).
entry(hingehen, [h, 'I', n, g, 'e:', @, n]).
entry(hingeht, [h, 'I', n, g, 'e:', t]).
entry(hingekriegt, [h, 'I', n, g, @, k, r, 'i:', k, t]).
entry(hingelangen, [h, 'I', n, g, @, l, a, 'N', @, n]).
entry(hingelegt, [h, 'I', n, g, @, l, 'e:', k, t]).
entry(hing, [h, 'I', 'N']).
entry(hinhauen, [h, 'I', n, h, aU, @, n]).
entry(hinhaut, [h, 'I', n, h, aU, t]).
entry(hin, [h, 'I', n]).
entry(hinke, [h, 'I', 'N', k, @]).
entry(hinkomme, [h, 'I', n, k, 'O', m, @]).
entry(hinkommen, [h, 'I', n, k, 'O', m, @, n]).
entry(hinkommt, [h, 'I', n, k, 'O', m, t]).
entry(hinkriege, [h, 'I', n, k, r, 'i:', g, @]).
entry(hinkriegen, [h, 'I', n, k, r, 'i:', g, @, n]).
entry(hinlegen, [h, 'I', n, l, 'e:', g, @, n]).
entry(hinmüssen, [h, 'I', n, m, 'Y', s, @, n]).
entry(hinnehmen, [h, 'I', n, n, 'e:', m, @, n]).
entry('Hinreise', [h, 'I', n, r, aI, z, @]).
entry(hinreisen, [h, 'I', n, r, aI, z, @, n]).
entry(hinschauen, [h, 'I', n, 'S', aU, @, n]).
entry('Hinsicht', [h, 'I', n, z, 'I', 'C', t]).
entry(hinten, [h, 'I', n, t, @, n]).
entry(hintereinander, [h, 'I', n, t, '6', aI, n, a, n, d, '6']).
entry(hintereinandernehmen, [h, 'I', n, t, '6', aI, n, a, n, d, '6', n, 'e:', m, @, n]).
entry(hintereinan, [h, 'I', n, t, '6', aI, n, a, n]).
entry(hinterher, [h, 'I', n, t, '6', h, 'e:', '6']).
entry(hinter, [h, 'I', n, t, '6']).
entry(hinterlassen, [h, 'I', n, t, '6', l, a, s, @, n]).
entry(hint, [h, 'I', n, t]).
entry('Hin-und-Her', [h, 'I', n, 'U', n, t, h, 'e:', '6']).
entry(hinweg, [h, 'I', n, v, 'E', k]).
entry('Hinweise', [h, 'I', n, v, aI, z, @]).
entry(hinweisen, [h, 'I', n, v, aI, z, @, n]).
entry('Hinweis', [h, 'I', n, v, aI, s]).
entry(hinwollten, [h, 'I', n, v, 'O', l, t, @, n]).
entry(hinziehen, [h, 'I', n, ts, 'i:', @, n]).
entry(hinzufahren, [h, 'I', n, ts, u, f, 'a:', r, @, n]).
entry(hinzufügen, [h, 'I', n, ts, 'u:', f, 'y:', g, @, n]).
entry(hinzu, [h, 'I', n, ts, 'u:']).
entry(hinzukommen, [h, 'I', n, ts, u, k, 'O', m, @, n]).
entry(hinzukommt, [h, 'I', n, ts, 'u:', k, 'O', m, t]).
entry(hinzulegen, [h, 'I', n, ts, u, l, 'e:', g, @, n]).
entry('Hirschberger', [h, 'I', '6', 'S', b, 'E', '6', g, '6']).
entry(historischen, [h, 'I', s, t, 'o:', r, 'I', 'S', @, n]).
entry(hm, [h, m]).
entry('Hobby', [h, 'O', b, i]).
entry(hochfahren, [h, 'o:', x, f, 'a:', r, @, n]).
entry(hochfliegen, [h, 'o:', x, f, l, 'i:', g, @, n]).
entry(hoch, [h, 'o:', x]).
entry(hochkommen, [h, 'o:', x, k, 'O', m, @, n]).
entry('Hochmu', [h, 'o:', x, m, 'u:']).
entry('Hochmut', [h, 'o:', x, m, 'u:', t]).
entry(hoc, [h, 'O', k]).
entry('Hochsommer', [h, 'o:', x, z, 'O', m, '6']).
entry('Höchstätter', [h, '2:', 'C', 'S', t, 'E', t, '6']).
entry(höchste, [h, '2:', 'C', s, t, @]).
entry(höchsten, [h, '2:', 'C', s, t, @, n]).
entry(höchstens, [h, '2:', 'C', s, t, @, n, s]).
entry('Hochstetter', [h, 'o:', x, 'S', t, 'E', t, '6']).
entry('Höchstfall', [h, '2:', 'C', s, t, f, a, l]).
entry(höchstwahrscheinlich, [h, '2:', 'C', s, t, v, 'a:', r, 'S', aI, n, l, 'I', 'C']).
entry('Hochzeit', [h, 'O', x, ts, aI, t]).
entry(hochzukommen, [h, 'o:', x, ts, u, k, 'O', m, @, n]).
entry('Hockenheim', [h, 'O', k, @, n, h, aI, m]).
entry('Hofbauer', [h, 'o:', f, b, aU, '6']).
entry(hoffe, [h, 'O', f, @]).
entry(hoffen, [h, 'O', f, @, n]).
entry(hoffentlich, [h, 'O', f, @, n, t, l, 'I', 'C']).
entry(hoff, [h, 'O', f]).
entry('Hoffner', [h, 'O', f, n, '6']).
entry('Hoffnung', [h, 'O', f, n, 'U', 'N']).
entry(hoffnungslos, [h, 'O', f, n, 'U', 'N', s, l, 'o:', s]).
entry(hoffte, [h, 'O', f, t, @]).
entry('Hof', [h, 'o:', f]).
entry('hof-Hotel', [h, 'o:', f, h, 'O', t, 'E', l]).
entry(hö, [h, '2:']).
entry('Höhe', [h, '2:', @]).
entry(hohe, [h, 'o:', @]).
entry('Höhenangst', [h, '2:', @, n, a, 'N', s, t]).
entry(hohen, [h, 'o:', @, n]).
entry(höhere, [h, '2:', @, r, @]).
entry(höheren, [h, '2:', @, r, @, n]).
entry(höherer, [h, '2:', @, r, '6']).
entry(höher, [h, '2:', '6']).
entry(hoher, [h, 'o:', '6']).
entry(ho, [h, o]).
entry(hole, [h, 'o:', l, @]).
entry(holen, [h, 'o:', l, @, n]).
entry('Holger', [h, 'O', l, g, '6']).
entry('Hol', [h, 'o:', l]).
entry('Holiday-Inn', [h, 'O', l, i, d, 'e:', 'I', n]).
entry(holi, [h, 'o:', l, i]).
entry('Holland', [h, 'O', l, a, n, t]).
entry('Hölle', [h, '9', l, @]).
entry('Holle', [h, 'O', l, @]).
entry('Holstein', [h, 'O', l, 'S', t, aI, n]).
entry('Hölzer', [h, '9', l, ts, '6']).
entry(holzig, [h, 'O', l, ts, 'I', 'C']).
entry('Homburg', [h, 'O', m, b, 'U', '6', k]).
entry(hön, [h, '2:', n]).
entry('Honolulu', [h, o, n, o, l, 'u:', l, u]).
entry(hoppla, [h, 'O', p, l, a]).
entry(höre, [h, '2:', r, @]).
entry(hören, [h, '2:', r, @, n]).
entry('Horoskop', [h, o, r, o, s, k, 'o:', p]).
entry(hörst, [h, '2:', '6', s, t]).
entry(hörte, [h, '2:', '6', t, @]).
entry(hört, [h, '2:', '6', t]).
entry(hort, [h, 'O', '6', t]).
entry('Hos', [h, o, s]).
entry('Hote', [h, o, t, 'E']).
entry('Hotelangebote', [h, o, t, 'E', l, a, n, g, @, b, 'o:', t, @]).
entry('Hotelangeboten', [h, o, t, 'E', l, a, n, g, @, b, 'o:', t, @, n]).
entry('Hotelaufenthalt', [h, o, t, 'E', l, aU, f, 'E', n, t, h, a, l, t]).
entry('Hotelbar', [h, o, t, @, l, b, 'a:', r]).
entry('Hotelbeschreibung', [h, o, t, @, l, b, @, 'S', r, aI, b, 'U', 'N']).
entry('Hotelbuchen', [h, o, t, 'E', l, b, 'u:', x, @, n]).
entry('Hotelbuchungen', [h, o, t, 'E', l, b, 'u:', x, 'U', 'N', @, n]).
entry('Hotelbuchung', [h, o, t, 'E', l, b, 'u:', x, 'U', 'N']).
entry('Hoteldirektion', [h, o, t, 'E', l, d, i, r, 'E', k, ts, j, 'o:', n]).
entry(hoteleigene, [h, o, t, 'E', l, aI, g, @, n, @]).
entry('Hotelessen', [h, o, t, 'E', l, 'E', s, @, n]).
entry(hotel, [h, o, t, 'E', l]).
entry('Hotel-Lage', [h, o, t, 'E', l, l, 'a:', g, @]).
entry('Hotelliste', [h, o, t, 'E', l, l, 'I', s, t, @]).
entry('Hotellisten', [h, o, t, 'E', l, l, 'I', s, t, @, n]).
entry('Hotellobby', [h, o, t, 'E', l, l, 'O', b, i]).
entry(hotelmäßig, [h, o, t, 'E', l, m, 'E:', s, 'I', 'C']).
entry('Hoteln', [h, o, t, 'E', l, n]).
entry('Hotelpläne', [h, o, t, 'E', l, p, l, 'E:', n, @]).
entry('Hotelplan', [h, o, t, 'E', l, p, l, 'a:', n]).
entry('Hotelreser', [h, o, t, 'E', l, r, @, z, '6']).
entry('Hotelreservierungen', [h, o, t, 'E', l, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', @, n]).
entry('Hotelreservierung', [h, o, t, 'E', l, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N']).
entry('Hotelrezeption', [h, o, t, 'E', l, r, e, ts, 'E', p, ts, j, 'o:', n]).
entry('Hotels', [h, o, t, 'E', l, s]).
entry('Hotelübernachtung', [h, o, t, 'E', l, 'y:', b, '6', n, a, x, t, 'U', 'N']).
entry('Hotelunterbringung', [h, o, t, 'E', l, 'U', n, t, '6', b, r, 'I', 'N', 'U', 'N']).
entry('Hotelunter', [h, o, t, 'E', l, 'U', n, t, '6']).
entry('Hotelunterkunft', [h, o, t, 'E', l, 'U', n, t, '6', k, 'U', n, f, t]).
entry('Hotelunterlagen', [h, o, t, 'E', l, 'U', n, t, '6', l, 'a:', g, @, n]).
entry('Hotelverzeichnis', [h, o, t, 'E', l, f, 'E', '6', ts, aI, 'C', n, 'I', s]).
entry('Hotelvorschläge', [h, o, t, 'E', l, f, 'o:', '6', 'S', l, 'E:', g, @]).
entry('Hotelwunsch', [h, o, t, 'E', l, v, 'U', n, 'S']).
entry('Hotelz', [h, o, t, 'E', l, ts]).
entry('Hotelzimmer', [h, o, t, 'E', l, ts, 'I', m, '6']).
entry('Hot', [h, o, t]).
entry(hour, [aU, '6']).
entry('Huber', [h, 'u:', b, '6']).
entry('Hubert', [h, 'u:', b, 'E', '6', t]).
entry(hübsche, [h, 'Y', p, 'S', @]).
entry(hübschen, [h, 'Y', p, 'S', @, n]).
entry(hübsches, [h, 'Y', p, 'S', @, s]).
entry(hübsch, [h, 'Y', p, 'S']).
entry('Hubschrauber', [h, 'u:', p, 'S', r, aU, b, '6']).
entry(huch, [h, 'u:', x]).
entry('Huf', [h, 'u:', f]).
entry('Hügel', [h, 'y:', g, @, l]).
entry(huh, [h, 'u:']).
entry(hui, [h, u, i]).
entry(hul, [h, 'U', l]).
entry(human, [h, u, m, 'a:', n]).
entry('Hummel', [h, 'U', m, @, l]).
entry('Hummer', [h, 'U', m, '6']).
entry(hunder, [h, 'U', n, d, '6']).
entry(hundert, [h, 'U', n, d, '6', t]).
entry('Hundskugel', [h, 'U', n, ts, k, 'u:', g, @, l]).
entry('Hunger', [h, 'U', 'N', '6']).
entry(hungrig, [h, 'U', 'N', r, 'I', 'C']).
entry(hun, [h, 'U', n]).
entry('Hut', [h, 'u:', t]).
entry('Hutmann', [h, 'u:', t, m, a, n]).
entry('Ibis', ['i:', b, 'I', s]).
entry('Ibrahim', ['i:', b, r, a, h, 'i:', m]).
entry(ichido, [i, 'S', i, d, o]).
entry(ich, ['I', 'C']).
entry('Icks', ['I', k, s]).
entry(ic, ['i:', ts, 'e:']).
entry('Ida', ['i:', d, a]).
entry(idealer, [i, d, e, 'a:', l, '6']).
entry(ideal, [i, d, e, 'a:', l]).
entry(idealsten, [i, d, e, 'a:', l, s, t, @, n]).
entry('Ideen', [i, d, 'e:', @, n]).
entry('Idee', [i, d, 'e:']).
entry(iebenundzwanzigsten, ['i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(iebten, ['i:', p, t, @, n]).
entry(iederhören, ['i:', d, '6', h, '2:', r, @, n]).
entry(iedersehen, ['i:', d, '6', z, 'e:', @, n]).
entry(ie, ['i:']).
entry(ienstag, ['i:', n, s, t, 'a:', k]).
entry(ieviel, ['i:', f, 'i:', l]).
entry(if, ['I', f]).
entry(igste, ['I', 'C', s, t, @]).
entry(ihm, ['i:', m]).
entry(ihnen, ['i:', n, @, n]).
entry('Ihne', ['i:', n, @]).
entry(ihn, ['i:', n]).
entry('Ihnten', ['i:', n, t, @, n]).
entry('Ihrem', ['i:', r, @, m]).
entry(ihren, ['i:', r, @, n]).
entry(ihre, ['i:', r, @]).
entry('Ihrer', ['i:', r, '6']).
entry('Ihrerseits', ['i:', r, '6', z, aI, ts]).
entry('Ihres', ['i:', r, @, s]).
entry(ihr, ['i:', '6']).
entry('Imax', [aI, m, 'E', k, s]).
entry('Imbiß', ['I', m, b, 'I', s]).
entry(immerhin, ['I', m, '6', h, 'I', n]).
entry(immer, ['I', m, '6']).
entry(im, ['I', m]).
entry('Ina', ['i:', n, a]).
entry(inbegriffen, ['I', n, b, @, g, r, 'I', f, @, n]).
entry(indem, ['I', n, d, 'e:', m]).
entry('Indesivpraktikum', ['I', n, d, e, s, 'i:', f, p, r, a, k, t, i, k, 'U', m]).
entry(indet, ['I', n, d, @, t]).
entry(indifferent, ['I', n, d, 'I', f, @, r, 'E', n, t]).
entry(indiskret, ['I', n, d, 'I', s, k, r, 'e:', t]).
entry(indisponi, ['I', n, d, 'I', s, p, o, n, i]).
entry(individueller, ['I', n, d, i, v, i, d, u, 'E', l, '6']).
entry(individuell, ['I', n, d, i, v, i, d, u, 'E', l]).
entry('Indonesisch', ['I', n, d, o, n, 'e:', z, 'I', 'S']).
entry('Infobau', ['I', n, f, o, b, aU]).
entry(informale, ['I', n, f, 'O', '6', m, 'a:', l, @]).
entry('Informatikbibliothek', ['I', n, f, 'O', '6', m, 'a:', t, 'I', k, b, i, b, l, i, o, t, 'e:', k]).
entry('Informatikgebäude', ['I', n, f, 'O', '6', m, 'a:', t, 'I', k, g, @, b, 'OY', d, @]).
entry('Informatikneubau', ['I', n, f, 'O', '6', m, 'a:', t, 'I', k, n, 'OY', b, aU]).
entry('Informatik', ['I', n, f, 'O', '6', m, 'a:', t, 'I', k]).
entry('Informationen', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n, @, n]).
entry('Information', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n]).
entry('Informationsbüro', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n, s, b, y, r, 'o:']).
entry('Informationsmaterial', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n, s, m, a, t, e, '6', j, 'a:', l]).
entry('Informationsmöglichkeiten', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n, s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Informationszettel', ['I', n, f, 'O', '6', m, a, ts, j, 'o:', n, s, ts, 'E', t, @, l]).
entry(informieren, ['I', n, f, 'O', '6', m, 'i:', r, @, n]).
entry(informiere, ['I', n, f, 'O', '6', m, 'i:', r, @]).
entry(informiert, ['I', n, f, 'O', '6', m, 'i:', '6', t]).
entry('Infos', ['I', n, f, o, s]).
entry('Ingelfinger', ['I', 'N', @, l, f, 'I', 'N', '6']).
entry('Ingenieurin', ['I', n, 'Z', e, n, j, '2:', r, 'I', n]).
entry('Ingo', ['I', 'N', g, o]).
entry('Inhalte', ['I', n, h, a, l, t, @]).
entry(inhaltlich, ['I', n, h, a, l, t, l, 'I', 'C']).
entry(inklusive, ['I', n, k, l, u, z, 'i:', v, @]).
entry(inkommodiert, ['I', n, k, 'O', m, o, d, 'i:', '6', t]).
entry('Inkompatibilitäten', ['I', n, k, 'O', m, p, a, t, i, b, i, l, i, t, 'E:', t, @, n]).
entry('Inlandsflügen', ['I', n, l, a, n, ts, f, l, 'y:', g, @, n]).
entry('Inlandsflug', ['I', n, l, a, n, ts, f, l, 'u:', k]).
entry('Innenstadtnähe', ['I', n, @, n, 'S', t, a, t, n, 'E:', @]).
entry('Innenstadt', ['I', n, @, n, 'S', t, a, t]).
entry(innerabteilige, ['I', n, '6', a, p, t, aI, l, 'I', g, @]).
entry(innerhalb, ['I', n, '6', h, a, l, p]).
entry(inner, ['I', n, '6']).
entry(insbesondere, ['I', n, s, b, @, z, 'O', n, d, @, r, @]).
entry(insgesamt, ['I', n, s, g, @, z, a, m, t]).
entry(insofern, ['I', n, z, 'o:', f, 'E', '6', n]).
entry(inso, ['I', n, z, o]).
entry(insoweit, ['I', n, z, 'o:', v, aI, t]).
entry('Inspektionsreise', ['I', n, s, p, 'E', k, ts, j, 'o:', n, s, r, aI, z, @]).
entry(ins, ['I', n, s]).
entry('Institutionen', ['I', n, s, t, i, t, u, ts, j, 'o:', n, @, n]).
entry('Institut', ['I', n, s, t, i, t, 'u:', t]).
entry(inszeniert, ['I', n, s, ts, e, n, 'i:', '6', t]).
entry('Inszenierung', ['I', n, s, ts, e, n, 'i:', r, 'U', 'N']).
entry(integriert, ['I', n, t, e, g, r, 'i:', '6', t]).
entry(intelligenter, ['I', n, t, 'E', l, i, g, 'E', n, t, '6']).
entry('Intelligenzforschung', ['I', n, t, 'E', l, 'I', g, 'E', n, ts, f, 'O', '6', 'S', 'U', 'N']).
entry('Intelligenz', ['I', n, t, 'E', l, i, g, 'E', n, ts]).
entry('Intensivpraktikum', ['I', n, t, 'E', n, z, 'i:', f, p, r, a, k, t, i, k, 'U', m]).
entry(intensiv, ['I', n, t, 'E', n, z, 'i:', f]).
entry('Intensivtatorium', ['I', n, t, 'E', n, z, 'i:', f, t, a, t, 'o:', r, j, 'U', m]).
entry('Intercity-Hotel', ['I', n, t, '6', s, 'I', t, i, h, o, t, 'E', l]).
entry('Intercity', ['I', n, t, '6', s, 'I', t, i]).
entry(interessanten, ['I', n, t, @, r, 'E', s, a, n, t, @, n]).
entry(interessante, ['I', n, t, @, r, 'E', s, a, n, t, @]).
entry(interessanter, ['I', n, t, @, r, 'E', s, a, n, t, '6']).
entry(interessanterweise, ['I', n, t, @, r, 'E', s, a, n, t, '6', v, aI, z, @]).
entry(interessantes, ['I', n, t, @, r, 'E', s, a, n, t, @, s]).
entry(interessantesten, ['I', n, t, @, r, 'E', s, a, n, t, @, s, t, @, n]).
entry(interessant, ['I', n, t, @, r, 'E', s, a, n, t]).
entry('Interessen', ['I', n, t, @, r, 'E', s, @, n]).
entry('Interesse', ['I', n, t, @, r, 'E', s, @]).
entry(interessieren, ['I', n, t, @, r, 'E', s, 'i:', '6', r, @, n]).
entry(interessiere, ['I', n, t, @, r, 'E', s, 'i:', r, @]).
entry(interessierst, ['I', n, t, @, r, 'E', s, 'i:', '6', s, t]).
entry(interessiert, ['I', n, t, @, r, 'E', s, 'i:', '6', t]).
entry(interess, ['I', n, t, @, r, 'E', s]).
entry('International-Congress', ['I', n, t, '6', n, 'E', 'S', @, n, @, l, k, 'O', n, g, r, 'E', s]).
entry('Internationale', ['I', n, t, '6', n, a, ts, j, o, n, 'a:', l, @]).
entry(interne, ['I', n, t, 'E', '6', n, @]).
entry('Internet', ['I', n, t, '6', n, 'E', t]).
entry('Interregio', ['I', n, t, '6', r, 'e:', g, i, 'o:']).
entry('Intervalle', ['I', n, t, '6', v, a, l, @]).
entry('Intervall', ['I', n, t, '6', v, a, l]).
entry('Interviewer', ['I', n, t, '6', v, j, 'u:', '6']).
entry('Interview', ['I', n, t, '6', v, j, 'u:']).
entry('Inventarisierung', ['I', n, v, 'E', n, t, a, r, i, z, 'i:', r, 'U', 'N']).
entry(investieren, ['I', n, v, 'E', s, t, 'i:', r, @, n]).
entry(inwiefern, ['I', n, v, i, f, 'E', '6', n]).
entry(inwieweit, ['I', n, v, i, v, aI, t]).
entry(inzwischen, ['I', n, ts, v, 'I', 'S', @, n]).
entry(irgendan, ['I', '6', g, @, n, t, a, n]).
entry(irgendeinem, ['I', '6', g, @, n, t, aI, n, @, m]).
entry(irgendeinen, ['I', '6', g, @, n, t, aI, n, @, n]).
entry(irgendeine, ['I', '6', g, @, n, t, aI, n, @]).
entry(irgendeiner, ['I', '6', g, @, n, t, aI, n, '6']).
entry(irgendeines, ['I', '6', g, @, n, t, aI, n, @, s]).
entry(irgendein, ['I', '6', g, @, n, t, aI, n]).
entry(irgend, ['I', '6', g, @, n, t]).
entry(irgendwann, ['I', '6', g, @, n, t, v, a, n]).
entry(irgendwa, ['I', '6', g, @, n, t, v, a]).
entry(irgendwas, ['I', '6', g, @, n, t, v, a, s]).
entry(irgendwelchen, ['I', '6', g, @, n, t, v, 'E', l, 'C', @, n]).
entry(irgendwelche, ['I', '6', g, @, n, t, v, 'E', l, 'C', @]).
entry(irgendwie, ['I', '6', g, @, n, t, v, 'i:']).
entry(irgendwohin, ['I', '6', g, @, n, t, v, 'o:', h, 'I', n]).
entry(irgendwo, ['I', '6', g, @, n, t, v, 'o:']).
entry(irgen, ['I', '6', g, @, n]).
entry(ir, ['I', '6']).
entry(irrelevant, ['I', r, e, l, e, v, a, n, t]).
entry(irren, ['I', r, @, n]).
entry('Irrtum', ['I', '6', t, 'u:', m]).
entry('Islands', ['i:', s, l, a, n, ts]).
entry(is, ['I', s]).
entry(ist, ['I', s, t]).
entry('Italiener', [i, t, a, l, j, 'e:', n, '6']).
entry(italienische, [i, t, a, l, j, 'e:', n, 'I', 'S', @]).
entry(italienisches, [i, t, a, l, j, 'e:', n, 'I', 'S', @, s]).
entry(italienisch, [i, t, a, l, j, 'e:', n, 'I', 'S']).
entry('Italien', [i, t, 'a:', l, j, @, n]).
entry(jacuzzis, [d, 'Z', @, k, 'u:', z, 'I', z]).
entry(jagt, [j, 'a:', k, t]).
entry('Jaguar', [j, 'a:', g, u, 'a:', r]).
entry('Jahre', [j, 'a:', r, @]).
entry('Jahren', [j, 'a:', r, @, n]).
entry('Jahresabschluß', [j, 'a:', r, @, s, a, p, 'S', l, 'U', s]).
entry('Jahresanfang', [j, 'a:', r, @, s, a, n, f, a, 'N']).
entry('Jahresende', [j, 'a:', r, @, s, 'E', n, d, @]).
entry('Jahres', [j, 'a:', r, @, s]).
entry('Jahresplanung', [j, 'a:', r, @, s, p, l, 'a:', n, 'U', 'N']).
entry('Jahresquartal', [j, 'a:', r, @, s, k, v, a, r, t, 'a:', l]).
entry('Jahressoll', [j, 'a:', r, @, s, z, 'O', l]).
entry('Jahrestagung', [j, 'a:', r, @, s, t, 'a:', g, 'U', 'N']).
entry('Jahresurlaub', [j, 'a:', r, @, s, 'u:', '6', l, aU, p]).
entry('Jahreswechsel', [j, 'a:', r, @, s, v, 'E', k, s, @, l]).
entry(jahreszeitenabhängig, [j, 'a:', r, @, s, ts, aI, t, @, n, a, p, h, 'E', 'N', 'I', 'C']).
entry('Jahreszeit', [j, 'a:', r, @, s, ts, aI, t]).
entry('Jahr', [j, 'a:', r]).
entry(jährlich, [j, 'E:', '6', l, 'I', 'C']).
entry('Jahrzehnten', [j, 'a:', r, ts, 'e:', n, t, @, n]).
entry(ja, [j, 'a:']).
entry('Ja', [j, a]).
entry('Jalcin', [j, a, l, ts, 'I', n]).
entry('James', [d, 'Z', e, 'I', m, z]).
entry(jam, [j, a, m]).
entry('Jänisch', [j, 'E:', n, 'I', 'S']).
entry('Jänner', [j, 'E', n, '6']).
entry('Jänsch', [j, 'E', n, 'S']).
entry('Jansen', [j, a, n, z, @, n]).
entry('Januarhälfte', [j, a, n, u, 'a:', r, h, 'E', l, f, t, @]).
entry('Januar', [j, a, n, u, 'a:', r]).
entry(januarmäßig, [j, a, n, u, 'a:', r, m, 'E:', s, 'I', 'C']).
entry('Januars', [j, a, n, u, 'a:', r, s]).
entry('Januartage', [j, a, n, u, 'a:', r, t, 'a:', g, @]).
entry('Januarwoche', [j, a, n, u, 'a:', r, v, 'O', x, @]).
entry('Januarwochenende', [j, a, n, u, 'a:', r, v, 'O', x, @, n, 'E', n, d, @]).
entry('Janu', [j, a, n, 'u:']).
entry('Januwar', [j, a, n, u, v, 'a:', r]).
entry('Japan', [j, 'a:', p, a, n]).
entry(japanische, [j, a, p, 'a:', n, 'I', 'S', @]).
entry(japanischen, [j, a, p, 'a:', n, 'I', 'S', @, n]).
entry(japanisches, [j, a, p, 'a:', n, 'I', 'S', @, s]).
entry(japanisch, [j, a, p, 'a:', n, 'I', 'S']).
entry(jawohl, [j, a, v, 'o:', l]).
entry('Jazzbar', [d, 'Z', 'E', s, b, 'a:', r]).
entry('Jazz', [d, 'Z', 'E', s]).
entry('Jazzkneipe', [d, 'Z', 'E', s, k, n, aI, p, @]).
entry('Jazzmusik', [d, 'Z', 'E', s, m, u, z, 'i:', k]).
entry(jede, [j, 'e:', d, @]).
entry(jedem, [j, 'e:', d, @, m]).
entry(jedenfalls, [j, 'e:', d, @, n, f, a, l, s]).
entry(jeden, [j, 'e:', d, @, n]).
entry(jeder, [j, 'e:', d, '6']).
entry(jederzeit, [j, 'e:', d, '6', ts, aI, t]).
entry(jedes, [j, 'e:', d, @, s]).
entry(jedesmal, [j, 'e:', d, @, s, m, 'a:', l]).
entry(jedoch, [j, e, d, 'O', x]).
entry('Jehniken', [j, 'e:', n, 'I', k, @, n]).
entry(jeht, [j, 'e:', t]).
entry(jei, [j, aI]).
entry(je, [j, 'e:']).
entry(jemals, [j, 'e:', m, 'a:', l, s]).
entry(jemandem, [j, 'e:', m, a, n, d, @, m]).
entry(jemanden, [j, 'e:', m, a, n, d, @, n]).
entry(jemand, [j, 'e:', m, a, n, t]).
entry(jeman, [j, 'e:', m, a, n]).
entry('Jena', [j, 'e:', n, a]).
entry(jenem, [j, 'e:', n, @, m]).
entry(jenseits, [j, 'e:', n, z, aI, ts]).
entry('Jensen', [j, 'E', n, z, @, n]).
entry('Jens', [j, 'E', n, s]).
entry('Jerusalem', [j, e, r, 'u:', z, a, l, 'E', m]).
entry('Jessica', [d, 'Z', e, s, 'I', k, @]).
entry(jet, [d, 'Z', 'E', t]).
entry(jetten, [d, 'Z', 'E', t, @, n]).
entry(jetzt, [j, 'E', ts, t]).
entry(jeweiligen, [j, 'e:', v, aI, l, 'I', g, @, n]).
entry(jeweils, [j, 'e:', v, aI, l, s]).
entry(jew, [j, 'e:', v]).
entry('Jim', [d, 'Z', 'I', m]).
entry(j, [j, 'O', t]).
entry('Job', [d, 'Z', 'O', p]).
entry('Jochen', [j, 'O', x, @, n]).
entry('Joe', [d, 'Z', @, 'U']).
entry('Johann', [j, 'o:', h, a, n]).
entry('Johne', [j, 'o:', n, @]).
entry(jonglieren, ['Z', 'O', 'N', l, 'i:', r, @, n]).
entry('Joswig', [j, 'O', s, v, 'I', 'C']).
entry(jour, ['Z', 'u:', r]).
entry('Ju', [j, 'u:']).
entry('Julei', [j, u, l, aI]).
entry('Jules', [d, 'Z', 'y:', l, z]).
entry('Julia', [d, 'Z', 'u:', l, i, @]).
entry('Jülich', [j, 'y:', l, 'I', 'C']).
entry('Julidrittel', [j, 'u:', l, i, d, r, 'I', t, @, l]).
entry('Julihälfte', [j, 'u:', l, i, h, 'E', l, f, t, @]).
entry('Juli', [j, 'u:', l, i]).
entry('Julis', [j, 'u:', l, i, s]).
entry('Juliß', [j, 'u:', l, 'I', s]).
entry('Julitage', [j, 'u:', l, i, t, 'a:', g, @]).
entry('Julitagen', [j, 'u:', l, i, t, 'a:', g, @, n]).
entry('Julius', [j, 'u:', l, j, 'U', s]).
entry('Juliwoche', [j, 'u:', l, i, v, 'O', x, @]).
entry('Juliwochenende', [j, 'u:', l, i, v, 'O', x, @, n, 'E', n, d, @]).
entry('Juliwochen', [j, 'u:', l, i, v, 'O', x, @, n]).
entry('Jul', [j, 'u:', l]).
entry('Julu', [j, 'u:', l, 'u:']).
entry(junge, [j, 'U', 'N', @]).
entry('Jungs', [j, 'U', 'N', s]).
entry('Juni', [j, 'u:', n, i]).
entry('Junis', [j, 'u:', n, i, s]).
entry('Junitage', [j, 'u:', n, i, t, 'a:', g, @]).
entry('Junitermine', [j, 'u:', n, i, t, 'E', '6', m, 'i:', n, @]).
entry('Junitermin', [j, 'u:', n, i, t, 'E', '6', m, 'i:', n]).
entry('Juniwoche', [j, 'u:', n, i, v, 'O', x, @]).
entry('Juniwochenende', [j, 'u:', n, i, v, 'O', x, @, n, 'E', n, d, @]).
entry('Jun', [j, 'u:', n]).
entry('Junker', [j, 'U', 'N', k, '6']).
entry('Juno', [j, 'u:', n, 'o:']).
entry('Jürgen', [j, 'Y', '6', g, @, n]).
entry('Jurkeit', [j, 'U', '6', k, aI, t]).
entry(just, [j, 'U', s, t]).
entry('Kabale-und-Liebe', [k, a, b, 'a:', l, @, 'U', n, t, l, 'i:', b, @]).
entry('Kabale-un', [k, a, b, 'a:', l, @, 'U', n]).
entry('Kabarettprogramm', [k, a, b, a, r, 'E', t, p, r, o, g, r, a, m]).
entry('Kachelrieß', [k, a, x, @, l, r, 'i:', s]).
entry('Kaduthanam', [k, a, d, 'U', t, h, a, n, 'a:', m]).
entry('Käfer', [k, 'E:', f, '6']).
entry('Käfer-Plex', [k, 'E:', f, '6', p, l, 'E', k, s]).
entry('Kaffeetrinken', [k, a, f, e, t, r, 'I', 'N', k, @, n]).
entry('Kaff', [k, a, f]).
entry('Kaiser-Cafe', [k, aI, z, '6', k, a, f, 'e:']).
entry('Kaiserhof', [k, aI, z, '6', h, 'o:', f]).
entry('Kaiserslautern', [k, aI, z, '6', s, l, aU, t, '6', n]).
entry(ka, [k, a]).
entry('Kalbe', [k, a, l, b, @]).
entry(kalender, [k, a, l, 'E', n, d, '6']).
entry('Kalendermonat', [k, a, l, 'E', n, d, '6', m, 'o:', n, a, t]).
entry('Kalenders', [k, a, l, 'E', n, d, '6', s]).
entry('Kalenderspalten', [k, a, l, 'E', n, d, '6', 'S', p, a, l, t, @, n]).
entry('Kalenderwoche', [k, a, l, 'E', n, d, '6', v, 'O', x, @]).
entry('Kalenderwochen', [k, a, l, 'E', n, d, '6', v, 'O', x, @, n]).
entry(kalen, [k, a, l, 'E', n]).
entry('Kaleschke', [k, a, l, 'E', 'S', k, @]).
entry('Kal', [k, a, l]).
entry(kalkulieren, [k, a, l, k, u, l, 'i:', r, @, n]).
entry('Kaltenkirchen', [k, a, l, t, @, n, k, 'I', '6', 'C', @, n]).
entry(kalt, [k, a, l, t]).
entry(käme, [k, 'E:', m, @]).
entry(kamen, [k, 'a:', m, @, n]).
entry(kämen, [k, 'E:', m, @, n]).
entry('Kamenz', [k, 'a:', m, 'E', n, ts]).
entry('Kaminkalender', [k, a, m, 'i:', n, k, a, l, 'E', n, d, '6']).
entry('Kamin', [k, a, m, 'i:', n]).
entry('Kaminsky', [k, a, m, 'I', n, s, k, i]).
entry(kam, [k, 'a:', m]).
entry('Kammerspiele', [k, a, m, '6', 'S', p, 'i:', l, @]).
entry('Kammerspielen', [k, a, m, '6', 'S', p, 'i:', l, @, n]).
entry('Kanada', [k, a, n, a, d, a]).
entry(kann, [k, a, n]).
entry(kannst, [k, a, n, s, t]).
entry('Kante', [k, a, n, t, @]).
entry('Kantine', [k, a, n, t, 'i:', n, @]).
entry('Kapazitäten', [k, a, p, a, ts, i, t, 'E:', t, @, n]).
entry(kapiere, [k, a, p, 'i:', r, @]).
entry(kapieren, [k, a, p, 'i:', r, @, n]).
entry('Kappe', [k, a, p, @]).
entry(kaputt, [k, a, p, 'U', t]).
entry('Karfreitag', [k, 'a:', r, f, r, aI, t, 'a:', k]).
entry('Karger', [k, a, r, g, '6']).
entry('Karibik', [k, a, r, 'i:', b, 'I', k]).
entry('Karin', [k, 'a:', r, 'I', n]).
entry('Karla', [k, a, r, l, a]).
entry('Karl', [k, a, r, l]).
entry('Karlsruhe', [k, a, r, l, s, r, 'u:', @]).
entry('Karlsruher', [k, a, r, l, s, r, 'u:', '6']).
entry('Karneval', [k, a, r, n, @, v, a, l]).
entry('Karnevals', [k, a, r, n, @, v, a, l, s]).
entry('Karnevalsnarr', [k, a, r, n, @, v, a, l, s, n, a, r]).
entry('Karnevals-Sache', [k, a, r, n, @, v, a, l, s, z, a, x, @]).
entry('Karnevalswoche', [k, a, r, n, @, v, a, l, s, v, 'O', x, @]).
entry('Karnevalswochenende', [k, a, r, n, @, v, a, l, s, v, 'O', x, @, n, 'E', n, d, @]).
entry('Karsamstag', [k, 'a:', r, z, a, m, s, t, 'a:', k]).
entry('Kartal', [k, a, r, t, 'a:', l]).
entry('Karte', [k, a, r, t, @]).
entry('Karten', [k, a, r, t, @, n]).
entry('Kartenreservierungen', [k, a, r, t, @, n, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', @, n]).
entry('Käse', [k, 'E:', z, @]).
entry('Kasse', [k, a, s, @]).
entry('Kassel', [k, a, s, @, l]).
entry('Kassen', [k, a, s, @, n]).
entry('Kästner-Gesellschaft', [k, 'E', s, t, n, '6', g, @, z, 'E', l, 'S', a, f, t]).
entry('Kataloge', [k, a, t, a, l, 'o:', g, @]).
entry(katastrophal, [k, a, t, a, s, t, r, o, f, 'a:', l]).
entry('Katastrophe', [k, a, t, a, s, t, r, 'o:', f, @]).
entry('Katego', [k, a, t, e, g, o]).
entry(kategorie, [k, a, t, e, g, o, r, 'i:']).
entry(kategorien, [k, a, t, e, g, o, r, 'i:', @, n]).
entry(kate, [k, a, t, e]).
entry(katholisch, [k, a, t, 'o:', l, 'I', 'S']).
entry('Kathrin', [k, a, t, r, 'i:', n]).
entry('Kattorie', [k, a, t, o, r, 'i:']).
entry('Katze', [k, a, ts, @]).
entry(kaufe, [k, aU, f, @]).
entry(kaufen, [k, aU, f, @, n]).
entry('Kauf', [k, aU, f]).
entry('Kaugummi', [k, aU, g, 'U', m, i]).
entry(kau, [k, aU]).
entry(kaum, [k, aU, m]).
entry('Kauz', [k, aU, ts]).
entry('Kaviar', [k, 'a:', v, j, a, r]).
entry(kay, [k, 'e:']).
entry(kea, [k, 'e:', a]).
entry('Kegelbrüdern', [k, 'e:', g, @, l, b, r, 'y:', d, '6', n]).
entry('Kegel', [k, 'e:', g, @, l]).
entry('Kehlheim', [k, 'e:', l, h, aI, m]).
entry('Kehl', [k, 'e:', l]).
entry(kehren, [k, 'e:', r, @, n]).
entry(kei, [k, aI]).
entry(keine, [k, aI, n, @]).
entry(keinem, [k, aI, n, @, m]).
entry(keinen, [k, aI, n, @, n]).
entry(keiner, [k, aI, n, '6']).
entry(keinerlei, [k, aI, n, '6', l, aI]).
entry(keines, [k, aI, n, @, s]).
entry(kein, [k, aI, n]).
entry(keins, [k, aI, n, s]).
entry(keinster, [k, aI, n, s, t, '6']).
entry(keit, [k, aI, t]).
entry(ke, [k, @]).
entry('Ke', [k, e]).
entry('Keller', [k, 'E', l, '6']).
entry('Kellertheater', [k, 'E', l, '6', t, e, 'a:', t, '6']).
entry('Kellner', [k, 'E', l, n, '6']).
entry('Kemp', [k, 'E', m, p]).
entry('Kempten', [k, 'E', m, p, t, @, n]).
entry('Kenia', [k, 'e:', n, j, a]).
entry('Keniareise', [k, 'e:', n, j, a, r, aI, z, @]).
entry('Ken', [k, 'E', n]).
entry(kenne, [k, 'E', n, @]).
entry(kennengelernt, [k, 'E', n, @, n, g, @, l, 'E', '6', n, t]).
entry(kennen, [k, 'E', n, @, n]).
entry(kennenlernen, [k, 'E', n, @, n, l, 'E', '6', n, @, n]).
entry(kennenzulernen, [k, 'E', n, @, n, ts, u, l, 'E', '6', n, @, n]).
entry(kennst, [k, 'E', n, s, t]).
entry(kennt, [k, 'E', n, t]).
entry(kennzeichnen, [k, 'E', n, ts, aI, 'C', n, @, n]).
entry(kep, [k, 'E', p]).
entry('Kern', [k, 'E', '6', n]).
entry('Kerstin', [k, 'E', '6', s, t, 'i:', n]).
entry('Kette', [k, 'E', t, @]).
entry('Kevin', [k, 'E', v, 'I', n]).
entry('Kewitz', [k, 'e:', v, 'I', ts]).
entry('Khojasteh', [k, 'o:', j, a, s, t, 'e:']).
entry('Kieler', [k, 'i:', l, '6']).
entry('Kiel', [k, 'i:', l]).
entry('Kiesling', [k, 'i:', s, l, 'I', 'N']).
entry('Kietzer', [k, 'i:', ts, '6']).
entry('Kilometer', [k, i, l, o, m, 'e:', t, '6']).
entry('Kindergartenfest', [k, 'I', n, d, '6', g, a, r, t, @, n, f, 'E', s, t]).
entry('Kindergarten', [k, 'I', n, d, '6', g, a, r, t, @, n]).
entry('Kinder', [k, 'I', n, d, '6']).
entry('Kindern', [k, 'I', n, d, '6', n]).
entry('Kind', [k, 'I', n, t]).
entry('king-size', [k, 'I', 'N', s, aI, z]).
entry('Kinobesuch', [k, 'i:', n, o, b, @, z, 'u:', x]).
entry('Kinofan', [k, 'i:', n, o, f, 'E:', n]).
entry('Kinofilm', [k, 'i:', n, o, f, 'I', l, m]).
entry('Kinokarten', [k, 'i:', n, o, k, a, r, t, @, n]).
entry('Kino', [k, 'i:', n, o]).
entry('Kinos', [k, 'i:', n, o, s]).
entry('Kipp', [k, 'I', p]).
entry('Kirche', [k, 'I', '6', 'C', @]).
entry('Kircher', [k, 'I', '6', 'C', '6']).
entry(kirchlicher, [k, 'I', '6', 'C', l, 'I', 'C', '6']).
entry('Kir-Royal', [k, 'i:', '6', r, 'O', j, 'a:', l]).
entry('Kiso', [k, 'i:', z, 'o:']).
entry('Kissen', [k, 'I', s, @, n]).
entry('Kitzer', [k, 'I', ts, '6']).
entry('Kiyonada', [k, 'I', j, 'o:', n, 'a:', d, a]).
entry('Kiyonaga', [k, 'I', j, 'o:', n, a, g, a]).
entry(k, [k, 'a:']).
entry('Klaassen', [k, l, 'a:', s, @, n]).
entry(klagen, [k, l, 'a:', g, @, n]).
entry('Klammern', [k, l, a, m, '6', n]).
entry('Klamotte', [k, l, a, m, 'O', t, @]).
entry(klang, [k, l, a, 'N']).
entry('Klappe', [k, l, a, p, @]).
entry(klappen, [k, l, a, p, @, n]).
entry(klappt, [k, l, a, p, t]).
entry(kläre, [k, l, 'E:', r, @]).
entry(klaren, [k, l, 'a:', r, @, n]).
entry(klären, [k, l, 'E:', r, @, n]).
entry(klares, [k, l, 'a:', r, @, s]).
entry(klargehen, [k, l, 'a:', r, g, 'e:', @, n]).
entry(klargemacht, [k, l, 'a:', r, g, @, m, a, x, t]).
entry('Klarheit', [k, l, a, r, h, aI, t]).
entry('Klarinette', [k, l, a, r, i, n, 'E', t, @]).
entry(klar, [k, l, 'a:', r]).
entry(klarkommen, [k, l, 'a:', r, k, 'O', m, @, n]).
entry(klarmachen, [k, l, 'a:', r, m, a, x, @, n]).
entry(klarzumachen, [k, l, 'a:', r, ts, u, m, a, x, @, n]).
entry(klasse, [k, l, a, s, @]).
entry('Klassikfan', [k, l, a, s, 'I', k, f, 'E:', n]).
entry('Klassik', [k, l, a, s, 'I', k]).
entry(klassische, [k, l, a, s, 'I', 'S', @]).
entry(klassisches, [k, l, a, s, 'I', 'S', @, s]).
entry(klauen, [k, l, aU, @, n]).
entry('Kläuser', [k, l, 'OY', z, '6']).
entry('Klaus', [k, l, aU, s]).
entry('Klausur', [k, l, aU, z, 'u:', '6']).
entry('Klavier', [k, l, a, v, 'i:', '6']).
entry('Klavierkonzerten', [k, l, a, v, 'i:', '6', k, 'O', n, ts, 'E', '6', t, @, n]).
entry(klebt, [k, l, 'e:', p, t]).
entry('Kleefelder-Hof', [k, l, 'e:', f, 'E', l, d, '6', h, 'o:', f]).
entry('Kleefelder', [k, l, 'e:', f, 'E', l, d, '6']).
entry('Klee', [k, l, 'e:']).
entry(kleine, [k, l, aI, n, @]).
entry(kleinen, [k, l, aI, n, @, n]).
entry(kleinere, [k, l, aI, n, @, r, @]).
entry(kleineren, [k, l, aI, n, @, r, @, n]).
entry(kleineres, [k, l, aI, n, @, r, @, s]).
entry(kleiner, [k, l, aI, n, '6']).
entry(kleines, [k, l, aI, n, @, s]).
entry('Kleinigkeit', [k, l, aI, n, 'I', 'C', k, aI, t]).
entry('Klein', [k, l, aI, n]).
entry('Kleinkunst', [k, l, aI, n, k, 'U', n, s, t]).
entry('Klemens', [k, l, 'e:', m, @, n, s]).
entry('Klemer', [k, l, 'e:', m, '6']).
entry('Klentzky', [k, l, 'E', n, ts, k, 'i:']).
entry(klick, [k, l, 'I', k]).
entry('Klima', [k, l, 'i:', m, a]).
entry(klingeling, [k, l, 'I', 'N', @, l, 'I', 'N']).
entry(kling, [k, l, 'I', 'N']).
entry(klingt, [k, l, 'I', 'N', t]).
entry('Klitsche', [k, l, 'I', tS, @]).
entry('Klitscher', [k, l, 'I', tS, '6']).
entry(kl, [k, l]).
entry('Klöbner', [k, l, '2:', p, n, '6']).
entry('Klub', [k, l, 'U', p]).
entry('Klubmitglied', [k, l, 'U', p, m, 'I', t, g, l, 'i:', t]).
entry('Klübner', [k, l, 'y:', p, n, '6']).
entry(knallvoll, [k, n, a, l, f, 'O', l]).
entry(knappe, [k, n, a, p, @]).
entry(knappen, [k, n, a, p, @, n]).
entry(knapp, [k, n, a, p]).
entry('Knecht', [k, n, 'E', 'C', t]).
entry('Knei', [k, n, aI]).
entry('Kneipe', [k, n, aI, p, @]).
entry('Kneipenbesuch', [k, n, aI, p, @, n, b, @, z, 'u:', x]).
entry('Kneipen', [k, n, aI, p, @, n]).
entry('Kneipen-Kultur', [k, n, aI, p, @, n, k, 'U', l, t, 'u:', '6']).
entry('Kneipentour', [k, n, aI, p, @, n, t, 'u:', '6']).
entry(knickrig, [k, n, 'I', k, r, 'I', 'C']).
entry(knifflig, [k, n, 'I', f, l, 'I', 'C']).
entry('Knipp', [k, n, 'I', p]).
entry('Knodt', [k, n, 'O', t]).
entry('Knopf', [k, n, 'O', pf]).
entry('Koblenz', [k, 'o:', b, l, 'E', n, ts]).
entry(kochen, [k, 'O', x, @, n]).
entry('Koch', [k, 'O', x]).
entry('Kochkunst', [k, 'O', x, k, 'U', n, s, t]).
entry('Koffer', [k, 'O', f, '6']).
entry('Koffertragen', [k, 'O', f, '6', t, r, 'a:', g, @, n]).
entry('Kofferträger', [k, 'O', f, '6', t, r, 'E:', g, '6']).
entry('Kohl', [k, 'o:', l]).
entry(kö, [k, '9']).
entry(ko, [k, 'O']).
entry('Köl', [k, '9', l]).
entry('Kollege', [k, 'O', l, 'e:', g, @]).
entry('Kollegen', [k, 'O', l, 'e:', g, @, n]).
entry('Kollegin', [k, 'O', l, 'e:', g, 'I', n]).
entry('Kollek', [k, 'O', l, 'E', k]).
entry('Kolloquium', [k, 'O', l, 'o:', k, v, i, 'U', m]).
entry('Köln-Bonner', [k, '9', l, n, b, 'O', n, '6']).
entry('Köln-Bonn', [k, '9', l, n, b, 'O', n]).
entry('Kölner-Gasse', [k, '9', l, n, '6', g, a, s, @]).
entry('Kölner', [k, '9', l, n, '6']).
entry('Köln', [k, '9', l, n]).
entry('Kombination', [k, 'O', m, b, i, n, a, ts, j, 'o:', n]).
entry(kombinieren, [k, 'O', m, b, i, n, 'i:', r, @, n]).
entry(kombiniert, [k, 'O', m, b, i, n, 'i:', '6', t]).
entry(komfortabel, [k, 'O', m, f, 'O', '6', t, 'a:', b, @, l]).
entry(komfortabelste, [k, 'O', m, f, 'O', '6', t, 'a:', b, @, l, s, t, @]).
entry(komfortable, [k, 'O', m, f, 'O', '6', t, 'a:', b, l, @]).
entry(komfortabler, [k, 'O', m, f, 'O', '6', t, 'a:', b, l, '6']).
entry('Komfort', [k, 'O', m, f, 'o:', '6']).
entry(komfort, [k, 'O', m, f, 'O', '6', t]).
entry(komischen, [k, 'o:', m, 'I', 'S', @, n]).
entry(komischer, [k, 'o:', m, 'I', 'S', '6']).
entry(komisch, [k, 'o:', m, 'I', 'S']).
entry(köm, [k, '9', m]).
entry(kom, [k, 'O', m]).
entry('Komma', [k, 'O', m, a]).
entry('Kommaprojekt', [k, 'O', m, a, p, r, o, j, 'E', k, t]).
entry(komme, [k, 'O', m, @]).
entry(kommende, [k, 'O', m, @, n, d, @]).
entry(kommenden, [k, 'O', m, @, n, d, @, n]).
entry(kommender, [k, 'O', m, @, n, d, '6']).
entry(kommendes, [k, 'O', m, @, n, d, @, s]).
entry(kommen, [k, 'O', m, @, n]).
entry('Kommilitonen', [k, 'O', m, i, l, i, t, 'o:', n, @, n]).
entry(kommst, [k, 'O', m, s, t]).
entry(kommt, [k, 'O', m, t]).
entry(kompakt, [k, 'O', m, p, a, k, t]).
entry('Kompaktkurs', [k, 'O', m, p, a, k, t, k, 'U', '6', s]).
entry('Kompaktseminar', [k, 'O', m, p, a, k, t, z, e, m, 'I', n, 'a:', r]).
entry('Kompakttermin', [k, 'O', m, p, a, k, t, t, 'E', '6', m, 'i:', n]).
entry(kompensieren, [k, 'O', m, p, 'E', n, z, 'i:', r, @, n]).
entry(kompetent, [k, 'O', m, p, @, t, 'E', n, t]).
entry(kompletten, [k, 'O', m, p, l, 'E', t, @, n]).
entry(kompletter, [k, 'O', m, p, l, 'E', t, '6']).
entry(komplett, [k, 'O', m, p, l, 'E', t]).
entry('Komplettpaket', [k, 'O', m, p, l, 'E', t, p, a, k, 'e:', t]).
entry(komplisch, [k, 'O', m, p, l, 'I', 'S']).
entry(kompliziert, [k, 'O', m, p, l, i, ts, 'i:', '6', t]).
entry(komprimieren, [k, 'O', m, p, r, i, m, 'i:', r, @, n]).
entry(komprimierte, [k, 'O', m, p, r, i, m, 'i:', '6', t, @]).
entry(komprimiert, [k, 'O', m, p, r, i, m, 'i:', '6', t]).
entry(kompromißbereit, [k, 'O', m, p, r, o, m, 'I', s, b, @, r, aI, t]).
entry('Kompromiß', [k, 'O', m, p, r, o, m, 'I', s]).
entry('Kompromisse', [k, 'O', m, p, r, o, m, 'I', s, @]).
entry('Kompromißvorschlag', [k, 'O', m, p, r, o, m, 'I', s, f, 'o:', '6', 'S', l, 'a:', k]).
entry('Konferenz-Abteile', [k, 'O', n, f, e, r, 'E', n, ts, a, p, t, aI, l, @]).
entry('Konferenzabteil', [k, 'O', n, f, e, r, 'E', n, ts, a, p, t, aI, l]).
entry('Konferenzen', [k, 'O', n, f, e, r, 'E', n, ts, @, n]).
entry('Konferenz', [k, 'O', n, f, e, r, 'E', n, ts]).
entry('Konferenzrauma', [k, 'O', n, f, e, r, 'E', n, ts, r, aU, m, a]).
entry('Konferenzräumen', [k, 'O', n, f, e, r, 'E', n, ts, r, 'OY', m, @, n]).
entry('Konferenzraum', [k, 'O', n, f, e, r, 'E', n, ts, r, aU, m]).
entry('Konferenzraum-Zuschlag', [k, 'O', n, f, e, r, 'E', n, ts, r, aU, m, ts, 'u:', 'S', l, 'a:', k]).
entry('Konferenzsaal', [k, 'O', n, f, e, r, 'E', n, ts, z, 'a:', l]).
entry('Konferenzvorbereitung', [k, 'O', n, f, e, r, 'E', n, ts, f, 'o:', '6', b, @, r, aI, t, 'U', 'N']).
entry('Konferenzzentrum', [k, 'O', n, f, e, r, 'E', n, ts, ts, 'E', n, t, r, 'U', m]).
entry(konferieren, [k, 'O', n, f, e, r, 'i:', r, @, n]).
entry(konfrontiert, [k, 'O', n, f, r, 'O', n, t, 'i:', '6', t]).
entry('Kongreßhotel', [k, 'O', 'N', g, r, 'E', s, h, o, t, 'E', l]).
entry('Kongreß', [k, 'O', 'N', g, r, 'E', s]).
entry('Kongressen', [k, 'O', 'N', g, r, 'E', s, @, n]).
entry('Kongreßunternehmungen', [k, 'O', 'N', g, r, 'E', s, 'U', n, t, '6', n, 'e:', m, 'U', 'N', @, n]).
entry(kongruent, [k, 'O', 'N', g, r, u, 'E', n, t]).
entry('Königshof', [k, '2:', n, 'I', 'C', s, h, 'o:', f]).
entry('Königsstraße', [k, '2:', n, 'I', 'C', 'S', t, r, 'a:', s, @]).
entry('Königstein', [k, '2:', n, 'I', 'C', 'S', t, aI, n]).
entry('Konitzer', [k, 'o:', n, 'I', ts, '6']).
entry('Konjunktur', [k, 'O', n, j, 'U', 'N', k, t, 'u:', '6']).
entry('Kön', [k, '2:', n]).
entry(kön, [k, '9', n]).
entry(konkete, [k, 'O', 'N', k, 'e:', t, @]).
entry(kon, [k, 'O', n]).
entry(konkrete, [k, 'O', 'N', k, r, 'e:', t, @]).
entry(konkreten, [k, 'O', 'N', k, r, 'e:', t, @, n]).
entry(konkreter, [k, 'O', 'N', k, r, 'e:', t, '6']).
entry(konkretes, [k, 'O', 'N', k, r, 'e:', t, @, s]).
entry('Konkretes', [k, 'O', n, k, r, 'e:', t, @, s]).
entry(konkretisieren, [k, 'O', 'N', k, r, e, t, i, z, 'i:', r, @, n]).
entry(konkret, [k, 'O', 'N', k, r, 'e:', t]).
entry(können, [k, '9', n, @, n]).
entry('Konnertz', [k, 'O', n, 'E', '6', ts]).
entry(könnte, [k, '9', n, t, @]).
entry(konnte, [k, 'O', n, t, @]).
entry(könnten, [k, '9', n, t, @, n]).
entry(konnten, [k, 'O', n, t, @, n]).
entry(könntest, [k, '9', n, t, @, s, t]).
entry(konntest, [k, 'O', n, t, @, s, t]).
entry(könnt, [k, '9', n, t]).
entry('Konrad', [k, 'O', n, r, 'a:', t]).
entry('Konsequenzen', [k, 'O', n, z, e, k, v, 'E', n, ts, @, n]).
entry('Konstanz', [k, 'O', n, s, t, a, n, ts]).
entry(konstruktiv, [k, 'O', n, s, t, r, 'U', k, t, 'i:', f]).
entry(konsultiert, [k, 'O', n, z, 'U', l, t, 'i:', '6', t]).
entry('Kontakte', [k, 'O', n, t, a, k, t, @]).
entry(kontaktieren, [k, 'O', n, t, a, k, t, 'i:', r, @, n]).
entry('Kontakt', [k, 'O', n, t, a, k, t]).
entry('Kontonummer', [k, 'O', n, t, o, n, 'U', m, '6']).
entry('Kontrollen', [k, 'O', n, t, r, 'O', l, @, n]).
entry(konzentrieren, [k, 'O', n, ts, 'E', n, t, r, 'i:', r, @, n]).
entry(konzentrierter, [k, 'O', n, ts, 'E', n, t, r, 'i:', '6', t, '6']).
entry('Konzept', [k, 'O', n, ts, 'E', p, t]).
entry('Konzern', [k, 'O', n, ts, 'E', '6', n]).
entry('Konzerte', [k, 'O', n, ts, 'E', '6', t, @]).
entry('Konzerten', [k, 'O', n, ts, 'E', '6', t, @, n]).
entry('Konzerthalle', [k, 'O', n, ts, 'E', '6', t, h, a, l, @]).
entry('Konzerthörer', [k, 'O', n, ts, 'E', '6', t, h, '2:', r, '6']).
entry('Konzertkalender', [k, 'O', n, ts, 'E', '6', t, k, a, l, 'E', n, d, '6']).
entry('Konzertkarten', [k, 'O', n, ts, 'E', '6', t, k, a, r, t, @, n]).
entry('Konzert', [k, 'O', n, ts, 'E', '6', t]).
entry('Konzertprogramm', [k, 'O', n, ts, 'E', '6', t, p, r, o, g, r, a, m]).
entry('Kooperationsbereitschaft', [k, o, o, p, e, r, a, ts, j, 'o:', n, s, b, @, r, aI, tS, a, f, t]).
entry(kooperativ, [k, o, o, p, e, r, a, t, 'i:', f]).
entry('Koordinations-Geschichten', [k, o, 'O', '6', d, i, n, a, ts, j, 'o:', n, s, g, @, 'S', 'I', 'C', t, @, n]).
entry(koordinieren, [k, o, 'O', '6', d, i, n, 'i:', r, @, n]).
entry('Kopenhagen', [k, 'o:', p, @, n, h, 'a:', g, @, n]).
entry('Köpfen', [k, '9', pf, @, n]).
entry('Köpf', [k, '9', pf]).
entry('Kopf', [k, 'O', pf]).
entry('Kopie', [k, o, p, 'i:']).
entry('Kopien', [k, o, p, 'i:', @, n]).
entry('Kopierer', [k, o, p, 'i:', r, '6']).
entry('Köpp', [k, '9', p]).
entry('Kopp', [k, 'O', p]).
entry('Körner', [k, '9', '6', n, '6']).
entry('Korn', [k, 'O', '6', n]).
entry(körperlich, [k, '9', '6', p, '6', l, 'I', 'C']).
entry(korrekt, [k, 'O', r, 'E', k, t]).
entry('Korrespondenz', [k, 'O', r, @, 'S', p, 'O', n, d, 'E', n, ts]).
entry(korrigieren, [k, 'O', r, i, g, 'i:', r, @, n]).
entry('Kose', [k, 'o:', z, @]).
entry('Kossmann', [k, 'O', s, m, a, n]).
entry('Kostenfaktor', [k, 'O', s, t, @, n, f, a, k, t, 'o:', '6']).
entry('Kostenfrage', [k, 'O', s, t, @, n, f, r, 'a:', g, @]).
entry('Kostengründen', [k, 'O', s, t, @, n, g, r, 'Y', n, d, @, n]).
entry(kostengünstig, [k, 'O', s, t, @, n, g, 'Y', n, s, t, 'I', 'C']).
entry(kosten, [k, 'O', s, t, @, n]).
entry('Kostenvoranschlag', [k, 'O', s, t, @, n, f, 'o:', '6', a, n, 'S', l, 'a:', k]).
entry(kostet, [k, 'O', s, t, @, t]).
entry(kost, [k, 'O', s, t]).
entry(köstlichen, [k, '9', s, t, l, 'I', 'C', @, n]).
entry('Köthe', [k, '2:', t, @]).
entry('Kotten', [k, 'O', t, @, n]).
entry('Kowalski', [k, o, v, a, l, s, k, i]).
entry('Krabben', [k, r, a, b, @, n]).
entry(krachen, [k, r, a, x, @, n]).
entry('Krafft', [k, r, a, f, t]).
entry(kräftigen, [k, r, 'E', f, t, 'I', g, @, n]).
entry('Kraftraum', [k, r, a, f, t, r, aU, m]).
entry('Krah', [k, r, 'a:']).
entry('Kram', [k, r, 'a:', m]).
entry('Krankheit', [k, r, a, 'N', k, h, aI, t]).
entry(krank, [k, r, a, 'N', k]).
entry(kraß, [k, r, a, s]).
entry('Krauch', [k, r, aU, x]).
entry('Kraus', [k, r, aU, s]).
entry('Kreditkarte', [k, r, e, d, 'i:', t, k, a, r, t, @]).
entry('Kreditkarten', [k, r, e, d, 'i:', t, k, a, r, t, @, n]).
entry('Kreditkartennummer', [k, r, e, d, 'i:', t, k, a, r, t, @, n, n, 'U', m, '6']).
entry('Kreise', [k, r, aI, z, @]).
entry('Kreis', [k, r, aI, s]).
entry('Krekow', [k, r, 'e:', k, 'O', f]).
entry('Kretit', [k, r, 'e:', t, 'i:', t]).
entry('Kreuze', [k, r, 'OY', ts, @]).
entry(kreuzen, [k, r, 'OY', ts, @, n]).
entry('Kreuz', [k, r, 'OY', ts]).
entry(kriege, [k, r, 'i:', g, @]).
entry(kriegen, [k, r, 'i:', g, @, n]).
entry(kriegt, [k, r, 'i:', k, t]).
entry('Krings', [k, r, 'I', 'N', s]).
entry('Kriterium', [k, r, i, t, 'e:', '6', j, 'U', m]).
entry(kritisch, [k, r, 'i:', t, 'I', 'S']).
entry('Kronenbach', [k, r, 'o:', n, @, n, b, a, x]).
entry('Kroner', [k, r, 'o:', n, '6']).
entry('Krongreß', [k, r, 'O', 'N', g, r, 'E', s]).
entry('Kronitzer', [k, r, 'o:', n, 'I', ts, '6']).
entry('Kronsberger-Hof', [k, r, 'o:', n, s, b, 'E', '6', g, '6', h, 'o:', f]).
entry('Kronsberger', [k, r, 'o:', n, s, b, 'E', '6', g, '6']).
entry('Kröpcke', [k, r, '9', p, k, @]).
entry(kru, [k, r, 'U']).
entry('Kru', [k, r, 'u:']).
entry('Krümmel', [k, r, 'Y', m, @, l]).
entry(kt, [k, t]).
entry('Küche', [k, 'Y', 'C', @]).
entry('Kuchen', [k, 'u:', x, @, n]).
entry(kucke, [k, 'U', k, @]).
entry(kucken, [k, 'U', k, @, n]).
entry(kuckt, [k, 'U', k, t]).
entry('Kuckuck', [k, 'U', k, 'U', k]).
entry(kudasai, [k, u, d, a, s, aI]).
entry('Kügler', [k, 'y:', g, l, '6']).
entry('Kühn', [k, 'y:', n]).
entry(ku, [k, 'U']).
entry('Kulturangebot', [k, 'U', l, t, 'u:', '6', a, n, g, @, b, 'o:', t]).
entry('Kulturbanause', [k, 'U', l, t, 'U', '6', b, a, n, aU, z, @]).
entry('Kulturbroschüre', [k, 'U', l, t, 'u:', '6', b, r, 'O', 'S', 'y:', r, @]).
entry('Kulturdiaspora', [k, 'U', l, t, 'u:', '6', d, i, a, s, p, o, r, a]).
entry(kulturelle, [k, 'U', l, t, u, r, 'E', l, @]).
entry('Kulturellem', [k, 'U', l, t, u, r, 'E', l, @, m]).
entry(kulturellen, [k, 'U', l, t, u, r, 'E', l, @, n]).
entry(kulturelles, [k, 'U', l, t, u, r, 'E', l, @, s]).
entry(kulturell, [k, 'U', l, t, u, r, 'E', l]).
entry('Kulturfreak', [k, 'U', l, t, 'u:', '6', f, r, 'i:', k]).
entry('Kulturhauptstadt', [k, 'U', l, t, 'u:', '6', h, aU, p, tS, t, a, t]).
entry('Kulturhochburg', [k, 'U', l, t, 'u:', '6', h, 'o:', x, b, 'U', '6', k]).
entry('Kulturkalender', [k, 'U', l, t, 'u:', '6', k, a, l, 'E', n, d, '6']).
entry('Kultur', [k, 'U', l, t, 'u:', '6']).
entry('Kulturleben', [k, 'U', l, t, 'u:', '6', l, 'e:', b, @, n]).
entry(kulturmäßig, [k, 'U', l, t, 'u:', '6', m, 'E:', s, 'I', 'C']).
entry('Kulturprogramm', [k, 'U', l, t, 'u:', '6', p, r, o, g, r, a, m]).
entry('Kulturveranstaltungen', [k, 'U', l, t, 'u:', '6', f, 'E', '6', a, n, s, t, a, l, t, 'U', 'N', @, n]).
entry('Kulturwochen', [k, 'U', l, t, 'u:', '6', v, 'O', x, @, n]).
entry(kümmere, [k, 'Y', m, @, r, @]).
entry(kümmer, [k, 'Y', m, '6']).
entry(kümmern, [k, 'Y', m, '6', n]).
entry(kümmerst, [k, 'Y', m, '6', s, t]).
entry(kümmert, [k, 'Y', m, '6', t]).
entry('Kunde', [k, 'U', n, d, @]).
entry(kunden, [k, 'U', n, d, @, n]).
entry('Kundentermin', [k, 'U', n, d, @, n, t, 'E', '6', m, 'i:', n]).
entry(kundgetan, [k, 'U', n, t, g, @, t, 'a:', n]).
entry(kundig, [k, 'U', n, d, 'I', 'C']).
entry(künftige, [k, 'Y', n, f, t, 'I', g, @]).
entry('Kunstausstellung', [k, 'U', n, s, t, aU, s, 'S', t, 'E', l, 'U', 'N']).
entry('Künste', [k, 'Y', n, s, t, @]).
entry('Kunst', [k, 'U', n, s, t]).
entry(künstliche, [k, 'Y', n, s, t, l, 'I', 'C', @]).
entry('Kunstmuseum', [k, 'U', n, s, t, m, u, z, 'e:', 'U', m]).
entry('Kunz', [k, 'U', n, ts]).
entry('Kupfer', [k, 'U', pf, '6']).
entry(kurete, [k, u, r, 'e:', t, @]).
entry(kur, [k, 'U', '6']).
entry('Kursbuch', [k, 'U', '6', s, b, 'u:', x]).
entry('Kursen', [k, 'U', '6', z, @, n]).
entry('Kurses', [k, 'U', '6', z, @, s]).
entry('Kurs', [k, 'U', '6', s]).
entry('Kursteilnehmer', [k, 'U', '6', s, t, aI, l, n, 'e:', m, '6']).
entry(kurze, [k, 'U', '6', ts, @]).
entry('Kürze', [k, 'Y', '6', ts, @]).
entry('Kürzel', [k, 'Y', '6', ts, @, l]).
entry(kurzem, [k, 'U', '6', ts, @, m]).
entry(kurzen, [k, 'U', '6', ts, @, n]).
entry(kürzere, [k, 'Y', '6', ts, @, r, @]).
entry(kürzeren, [k, 'Y', '6', ts, @, r, @, n]).
entry(kurzer, [k, 'U', '6', ts, '6']).
entry(kürzer, [k, 'Y', '6', ts, '6']).
entry(kurzes, [k, 'U', '6', ts, @, s]).
entry('Kürzeste', [k, 'Y', '6', ts, 'E', s, t, @]).
entry(kurzfristigen, [k, 'U', '6', ts, f, r, 'I', s, t, 'I', g, @, n]).
entry(kurzfristiger, [k, 'U', '6', ts, f, r, 'I', s, t, 'I', g, '6']).
entry(kurzfristig, [k, 'U', '6', ts, f, r, 'I', s, t, 'I', 'C']).
entry(kurz, [k, 'U', '6', ts]).
entry(kürzlich, [k, 'Y', '6', ts, l, 'I', 'C']).
entry(kurzschließen, [k, 'U', '6', ts, 'S', l, 'i:', s, @, n]).
entry('Kurzstrecken', [k, 'U', '6', ts, 'S', t, r, 'E', k, @, n]).
entry('Kurztermine', [k, 'U', '6', ts, t, 'E', '6', m, 'i:', n, @]).
entry('Kurztermin', [k, 'U', '6', ts, t, 'E', '6', m, 'i:', n]).
entry('Kurzurlaub', [k, 'U', '6', ts, 'u:', '6', l, aU, p]).
entry(kurzweiliger, [k, 'U', '6', ts, v, aI, l, 'I', g, '6']).
entry(kurzzeitig, [k, 'U', '6', ts, ts, aI, t, 'I', 'C']).
entry('Kustrich', [k, 'U', s, t, r, 'I', 'C']).
entry('Kuzla', [k, 'U', ts, l, a]).
entry(lachen, [l, a, x, @, n]).
entry(lächerlich, [l, 'E', 'C', '6', l, 'I', 'C']).
entry(lade, [l, 'a:', d, @]).
entry('Laden', [l, 'a:', d, @, n]).
entry('Ladies-Night', [l, 'e:', d, i, s, n, aI, t]).
entry('Lad', [l, a, t]).
entry('Ladolmy', [l, a, d, 'O', l, m, i]).
entry('Lage', [l, 'a:', g, @]).
entry(läge, [l, 'E:', g, @]).
entry(lagen, [l, 'a:', g, @, n]).
entry(lag, [l, 'a:', k]).
entry(la, [l, a]).
entry('Landau', [l, a, n, d, aU]).
entry('Lande', [l, a, n, d, @]).
entry(landen, [l, a, n, d, @, n]).
entry('Ländern', [l, 'E', n, d, '6', n]).
entry('Landes', [l, a, n, d, @, s]).
entry('Landesrechenzentrum', [l, a, n, d, @, s, r, 'E', 'C', @, n, ts, 'E', n, t, r, 'U', m]).
entry('Landesregierung', [l, a, n, d, @, s, r, e, g, 'i:', r, 'U', 'N']).
entry(landet, [l, a, n, d, @, t]).
entry('Landhaus', [l, a, n, t, h, aU, s]).
entry('Land', [l, a, n, t]).
entry('Landsberg', [l, a, n, ts, b, 'E', '6', k]).
entry('Landschaften', [l, a, n, tS, a, f, t, @, n]).
entry('Landschaft', [l, a, n, tS, a, f, t]).
entry('Landshut', [l, a, n, ts, h, 'u:', t]).
entry(lange, [l, a, 'N', @]).
entry('Länge', [l, 'E', 'N', @]).
entry('Langemann', [l, a, 'N', @, m, a, n]).
entry('Langenberg', [l, a, 'N', @, n, b, 'E', '6', k]).
entry('Langenhagen', [l, a, 'N', g, @, n, h, 'a:', g, @, n]).
entry(langen, [l, a, 'N', @, n]).
entry('Langenstein', [l, a, 'N', @, n, 'S', t, aI, n]).
entry(längere, [l, 'E', 'N', @, r, @]).
entry(längerem, [l, 'E', 'N', @, r, @, m]).
entry(längeren, [l, 'E', 'N', @, r, @, n]).
entry(längerer, [l, 'E', 'N', @, r, '6']).
entry(längeres, [l, 'E', 'N', @, r, @, s]).
entry(längerfristigen, [l, 'E', 'N', '6', f, r, 'I', s, t, 'I', g, @, n]).
entry(langer, [l, a, 'N', '6']).
entry(länger, [l, 'E', 'N', '6']).
entry(langes, [l, a, 'N', @, s]).
entry('Langeweile', [l, a, 'N', @, v, aI, l, @]).
entry(lang, [l, a, 'N']).
entry('Langlaufskier', [l, a, 'N', l, aU, f, 'S', 'i:', '6']).
entry(langsam, [l, a, 'N', z, 'a:', m]).
entry('Langschläfer', [l, a, 'N', 'S', l, 'E:', f, '6']).
entry(langstreckenerprobt, [l, a, 'N', 'S', t, r, 'E', k, @, n, 'E', '6', p, r, 'o:', p, t]).
entry(langt, [l, a, 'N', t]).
entry(langweilige, [l, a, 'N', v, aI, l, 'I', g, @]).
entry(langweilig, [l, a, 'N', v, aI, l, 'I', 'C']).
entry('Lan', [l, 'a:', n]).
entry('Laptop', [l, 'E', p, t, 'O', p]).
entry('Laptops', [l, 'E', p, t, 'O', p, s]).
entry('Lärmkulisse', [l, 'E', '6', m, k, u, l, 'I', s, @]).
entry('Lärm', [l, 'E', '6', m]).
entry(laß, [l, a, s]).
entry(las, [l, 'a:', s]).
entry(lasse, [l, a, s, @]).
entry(lassen, [l, a, s, @, n]).
entry('Lasten', [l, a, s, t, @, n]).
entry('Laster', [l, a, s, t, '6']).
entry('Lasterlöchli', [l, a, s, t, '6', l, '9', 'C', l, i]).
entry(lästige, [l, 'E', s, t, 'I', g, @]).
entry(lästig, [l, 'E', s, t, 'I', 'C']).
entry('Last', [l, a, s, t]).
entry(läßt, [l, 'E', s, t]).
entry(lateinamerikanisches, [l, a, t, aI, n, a, m, e, r, i, k, 'a:', n, 'I', 'S', @, s]).
entry(lauen, [l, aU, @, n]).
entry('Lauerbach', [l, aU, '6', b, a, x]).
entry(lauern, [l, aU, '6', n]).
entry('Laufe', [l, aU, f, @]).
entry(laufenden, [l, aU, f, @, n, d, @, n]).
entry(laufen, [l, aU, f, @, n]).
entry('Läufer-Star', [l, 'OY', f, '6', s, t, 'a:', r]).
entry('Lauf', [l, aU, f]).
entry(läuft, [l, 'OY', f, t]).
entry('Laufzeit', [l, aU, f, ts, aI, t]).
entry(lau, [l, aU]).
entry('Laune', [l, aU, n, @]).
entry(lauschen, [l, aU, 'S', @, n]).
entry(lauschiges, [l, aU, 'S', 'I', g, @, s]).
entry('Laus', [l, aU, s]).
entry(lauten, [l, aU, t, @, n]).
entry(lauter, [l, aU, t, '6']).
entry(lautet, [l, aU, t, @, t]).
entry(laut, [l, aU, t]).
entry('Lautsprecher', [l, aU, tS, p, r, 'E', 'C', '6']).
entry('Lavergne', [l, a, v, 'E', '6', n, j, @]).
entry('Lay', [l, aI]).
entry(lebe, [l, 'e:', b, @]).
entry(leben, [l, 'e:', b, @, n]).
entry('Lebensabschnittsgefährte', [l, 'e:', b, @, n, s, a, p, 'S', n, 'I', ts, g, @, f, 'E:', '6', t, @]).
entry('Lebens', [l, 'e:', b, @, n, s]).
entry('Leber', [l, 'e:', b, '6']).
entry(lebt, [l, 'e:', p, t]).
entry(leckere, [l, 'E', k, @, r, @]).
entry(leckeres, [l, 'E', k, @, r, @, s]).
entry(leden, [l, 'e:', d, @, n]).
entry(ledig, [l, 'e:', d, 'I', 'C']).
entry(lediglich, [l, 'e:', d, 'I', k, l, 'I', 'C']).
entry('Leere', [l, 'e:', r, @]).
entry(leerer, [l, 'e:', r, '6']).
entry('Leerlauf', [l, 'e:', '6', l, aU, f]).
entry(leer, [l, 'e:', '6']).
entry(lege, [l, 'e:', g, @]).
entry(legen, [l, 'e:', g, @, n]).
entry(leger, [l, e, 'Z', 'e:', '6']).
entry('Lehmann', [l, 'e:', m, a, n]).
entry(lehne, [l, 'e:', n, @]).
entry('Lehrerfortbildung', [l, 'e:', r, '6', f, 'O', '6', t, b, 'I', l, d, 'U', 'N']).
entry('Lehrveranstaltungen', [l, 'e:', '6', f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', @, n]).
entry('Lehrverpflichtungen', [l, 'e:', '6', f, 'E', '6', pf, l, 'I', 'C', t, 'U', 'N', @, n]).
entry(leichter, [l, aI, 'C', t, '6']).
entry(leicht, [l, aI, 'C', t]).
entry(leide, [l, aI, d, @]).
entry(leiden, [l, aI, d, @, n]).
entry(leidenschaftlicher, [l, aI, d, @, n, 'S', a, f, t, l, 'I', 'C', '6']).
entry(leidenschaftslos, [l, aI, d, @, n, 'S', a, f, ts, l, 'o:', s]).
entry(leider, [l, aI, d, '6']).
entry(leidige, [l, aI, d, 'I', g, @]).
entry(leidiges, [l, aI, d, 'I', g, @, s]).
entry(leidig, [l, aI, d, 'I', 'C']).
entry(leid, [l, aI, t]).
entry(leidlichen, [l, aI, d, l, 'I', 'C', @, n]).
entry(leihen, [l, aI, @, n]).
entry('Leipzig', [l, aI, p, ts, 'I', 'C']).
entry(leisten, [l, aI, s, t, @, n]).
entry(leitenden, [l, aI, t, @, n, d, @, n]).
entry(leiten, [l, aI, t, @, n]).
entry('Leiterin', [l, aI, t, @, r, 'I', n]).
entry(leiter, [l, aI, t, '6']).
entry(le, [l, e]).
entry('Lennertz', [l, 'E', n, '6', ts]).
entry('Lentzen', [l, 'E', n, ts, @, n]).
entry('Lenz', [l, 'E', n, ts]).
entry('Leonhard', [l, 'e:', 'O', n, h, a, r, t]).
entry('Leopold', [l, 'e:', o, p, 'O', l, t]).
entry('Lerch', [l, 'E', '6', 'C']).
entry('Lernen', [l, 'E', '6', n, @, n]).
entry(lernt, [l, 'E', '6', n, t]).
entry(lese, [l, 'e:', z, @]).
entry(lesen, [l, 'e:', z, @, n]).
entry(letz, [l, 'E', ts]).
entry(letzte, [l, 'E', ts, t, @]).
entry(letztem, [l, 'E', ts, t, @, m]).
entry(letztendlich, [l, 'E', ts, t, 'E', n, t, l, 'I', 'C']).
entry(letzten, [l, 'E', ts, t, @, n]).
entry(letztens, [l, 'E', ts, t, @, n, s]).
entry(letzteres, [l, 'E', ts, t, @, r, @, s]).
entry(letzter, [l, 'E', ts, t, '6']).
entry(letztes, [l, 'E', ts, t, @, s]).
entry(letztgenannte, [l, 'E', ts, t, g, @, n, a, n, t, @]).
entry(letztgenannten, [l, 'E', ts, t, g, @, n, a, n, t, @, n]).
entry(letztiges, [l, 'E', ts, t, 'I', g, @, s]).
entry('Letzt', [l, 'E', ts, t]).
entry(letztlich, [l, 'E', ts, t, l, 'I', 'C']).
entry('Leute', [l, 'OY', t, @]).
entry('Leuten', [l, 'OY', t, @, n]).
entry('Leyerweber', [l, aI, '6', v, 'e:', b, '6']).
entry('Leyweber', [l, aI, v, 'e:', b, '6']).
entry(lichtigen, [l, 'I', 'C', t, 'I', g, @, n]).
entry(liebe, [l, 'i:', b, @]).
entry(liebend, [l, 'i:', b, @, n, t]).
entry(lieben, [l, 'i:', b, @, n]).
entry(lieber, [l, 'i:', b, '6']).
entry('Lieblingshotel', [l, 'i:', p, l, 'I', 'N', s, h, o, t, 'E', l]).
entry(lieb, [l, 'i:', p]).
entry(liebste, [l, 'i:', p, s, t, @]).
entry(liebsten, [l, 'i:', p, s, t, @, n]).
entry(liegenden, [l, 'i:', g, @, n, d, @, n]).
entry(liegen, [l, 'i:', g, @, n]).
entry('Liegewagen', [l, 'i:', g, @, v, 'a:', g, @, n]).
entry(liegt, [l, 'i:', k, t]).
entry(ließe, [l, 'i:', s, @]).
entry('Liesen', [l, 'i:', z, @, n]).
entry(liiert, [l, i, 'i:', '6', t]).
entry(li, [l, 'I']).
entry('Limburg', [l, 'I', m, b, 'U', '6', k]).
entry('Limit', [l, 'I', m, 'I', t]).
entry('Limits', [l, 'I', m, 'I', ts]).
entry('Lindau', [l, 'I', n, d, aU]).
entry('Linderhof', [l, 'I', n, d, '6', h, 'o:', f]).
entry('Linguistik', [l, 'I', 'N', g, u, 'I', s, t, 'I', k]).
entry('Linie', [l, 'i:', n, j, @]).
entry('Linienflügen', [l, 'i:', n, j, @, n, f, l, 'y:', g, @, n]).
entry(links, [l, 'I', 'N', k, s]).
entry('Linn', [l, 'I', n]).
entry('Lipp', [l, 'I', p]).
entry(lis, [l, 'I', s]).
entry('Liste', [l, 'I', s, t, @]).
entry('List', [l, 'I', s, t]).
entry('Literatur', [l, 'I', t, @, r, a, t, 'u:', '6']).
entry(live, [l, aI, f]).
entry('Live-Musik', [l, aI, f, m, u, z, 'i:', k]).
entry('Lloyd-Webber', [l, 'O', 'I', d, w, e, b, '6']).
entry('Loccumer-Hof', [l, 'O', k, 'U', m, '6', h, 'o:', f]).
entry('Loccumer', [l, 'O', k, 'U', m, '6']).
entry('Locher', [l, 'O', x, '6']).
entry(lockere, [l, 'O', k, @, r, @]).
entry(lockeren, [l, 'O', k, @, r, @, n]).
entry(locker, [l, 'O', k, '6']).
entry(lockert, [l, 'O', k, '6', t]).
entry(logieren, [l, o, 'Z', 'i:', r, @, n]).
entry(lohnen, [l, 'o:', n, @, n]).
entry(lohnt, [l, 'o:', n, t]).
entry('Löhr', [l, '2:', '6']).
entry('Lokale', [l, o, k, 'a:', l, @]).
entry('Lokalitäten', [l, o, k, a, l, i, t, 'E:', t, @, n]).
entry('Lokalität', [l, o, k, a, l, i, t, 'E:', t]).
entry('Lokal', [l, o, k, 'a:', l]).
entry('Lokomotive', [l, o, k, o, m, o, t, 'i:', v, @]).
entry(lo, [l, 'O']).
entry('Lo', [l, o]).
entry('London', [l, 'O', n, d, 'O', n]).
entry('Lorgie', [l, 'O', '6', g, 'i:']).
entry(lör, [l, '9', '6']).
entry('Los-Banditos-Bar', [l, 'O', s, b, a, n, d, i, t, 'O', s, b, 'a:', r]).
entry(löschen, [l, '9', 'S', @, n]).
entry(löse, [l, '2:', z, @]).
entry(lösende, [l, '2:', z, @, n, d, @]).
entry(lösen, [l, '2:', z, @, n]).
entry(losfahren, [l, 'o:', s, f, 'a:', r, @, n]).
entry(losfährt, [l, 'o:', s, f, 'E', '6', t]).
entry(losfliege, [l, 'o:', s, f, l, 'i:', g, @]).
entry(losfliegen, [l, 'o:', s, f, l, 'i:', g, @, n]).
entry(losfliegt, [l, 'o:', s, f, l, 'i:', g, t]).
entry(losgehen, [l, 'o:', s, g, 'e:', @, n]).
entry(losgeht, [l, 'o:', s, g, 'e:', t]).
entry(losge, [l, 'o:', s, g, @]).
entry(losgeschickt, [l, 'o:', s, g, @, 'S', 'I', k, t]).
entry(loskommen, [l, 'o:', s, k, 'O', m, @, n]).
entry(loslassen, [l, 'o:', s, l, a, s, @, n]).
entry(loslegen, [l, 'o:', s, l, 'e:', g, @, n]).
entry(los, [l, 'o:', s]).
entry(losmachen, [l, 'o:', s, m, a, x, @, n]).
entry(losreisen, [l, 'o:', s, r, aI, z, @, n]).
entry(losschicken, [l, 'o:', s, 'S', 'I', k, @, n]).
entry(lössen, [l, '9', s, @, n]).
entry(losstarten, [l, 'o:', s, 'S', t, a, r, t, @, n]).
entry('Lösung', [l, '2:', z, 'U', 'N']).
entry(loswollen, [l, 'o:', s, v, 'O', l, @, n]).
entry(losziehen, [l, 'o:', s, ts, 'i:', @, n]).
entry(loszufahren, [l, 'o:', s, ts, u, f, 'a:', r, @, n]).
entry(loszufliegen, [l, 'o:', s, ts, u, f, l, 'i:', g, @, n]).
entry(loszuziehen, [l, 'o:', s, ts, u, ts, 'i:', @, n]).
entry(lotsen, [l, 'o:', ts, @, n]).
entry('Lotter', [l, 'O', t, '6']).
entry('Lotto', [l, 'O', t, o]).
entry('Lotz', [l, 'O', ts]).
entry(l, ['E', l]).
entry('Lübeck', [l, 'y:', b, 'E', k]).
entry('Lübke', [l, 'Y', p, k, @]).
entry('Lücke', [l, 'Y', k, @]).
entry('Lücken', [l, 'Y', k, @, n]).
entry('Lückentermine', [l, 'Y', k, @, n, t, 'E', '6', m, 'i:', n, @]).
entry('Lück', [l, 'Y', k]).
entry('Ludwig', [l, 'u:', t, v, 'I', 'C']).
entry('Ludwigshafen', [l, 'u:', t, v, 'I', 'C', s, h, 'a:', f, @, n]).
entry('Lüften', [l, 'Y', f, t, @, n]).
entry('Lufthansaflug', [l, 'U', f, t, h, a, n, z, a, f, l, 'u:', k]).
entry('Lufthansa', [l, 'U', f, t, h, a, n, z, a]).
entry('Luft', [l, 'U', f, t]).
entry('Luger', [l, 'u:', g, '6']).
entry('Lui', [l, u, i]).
entry('Luise', [l, u, 'i:', z, @]).
entry('Luisenhof', [l, u, 'i:', z, @, n, h, 'o:', f]).
entry(lu, [l, 'u:']).
entry('Lu', [l, 'U']).
entry('Lunch', [l, a, n, 'S']).
entry('Lüneburg', [l, 'y:', n, @, b, 'U', '6', k]).
entry('Lus', [l, 'U', s]).
entry(lustigerweise, [l, 'U', s, t, 'I', g, '6', v, aI, z, @]).
entry(lustig, [l, 'U', s, t, 'I', 'C']).
entry('Lust', [l, 'U', s, t]).
entry('Lustunternehmen', [l, 'U', s, t, 'U', n, t, '6', n, 'e:', m, @, n]).
entry('Lut', [l, u, t]).
entry('Lutz', [l, 'U', ts]).
entry(luxuriöseres, [l, 'U', k, s, u, '6', j, '2:', z, @, r, @, s]).
entry(luxuriöser, [l, 'U', k, s, u, '6', j, '2:', z, '6']).
entry(luxuriöses, [l, 'U', k, s, u, '6', j, '2:', z, @, s]).
entry(luxuriös, [l, 'U', k, s, u, '6', j, '2:', s]).
entry('Luxushotel', [l, 'U', k, s, 'U', s, h, o, t, 'E', l]).
entry('Luxus', [l, 'U', k, s, 'U', s]).
entry('Luxusrestaurant', [l, 'U', k, s, 'U', s, r, 'E', s, t, o, r, 'a~:']).
entry('Luxusrestaurants', [l, 'U', k, s, 'U', s, r, 'E', s, t, o, r, 'a~:', s]).
entry('Lyle', [l, aI, l]).
entry(machbar, [m, a, x, b, 'a:', r]).
entry(mache, [m, a, x, @]).
entry(machen, [m, a, x, @, n]).
entry(mach, [m, a, x]).
entry(machst, [m, a, x, s, t]).
entry(macht, [m, a, x, t]).
entry('Mädels', [m, 'E:', d, @, l, s]).
entry(mad, [m, 'E:', t]).
entry('Mägen', [m, 'E:', g, @, n]).
entry(mag, [m, 'a:', k]).
entry('Maier', [m, aI, '6']).
entry('Maifeiertag', [m, aI, f, aI, '6', t, 'a:', k]).
entry('Maihälfte', [m, aI, h, 'E', l, f, t, @]).
entry('Mailand', [m, aI, l, a, n, t]).
entry(mail, [m, 'e:', l]).
entry('Mai', [m, aI]).
entry('Maimoch', [m, aI, m, 'O', x]).
entry('Mainz', [m, aI, n, ts]).
entry('Mais', [m, aI, s]).
entry('Maitermine', [m, aI, t, 'E', '6', m, 'i:', n, @]).
entry('Maiwoche', [m, aI, v, 'O', x, @]).
entry('Maiwochenende', [m, aI, v, 'O', x, @, n, 'E', n, d, @]).
entry('Maiwochen', [m, aI, v, 'O', x, @, n]).
entry('Malediven', [m, a, l, e, d, 'i:', v, @, n]).
entry('Male', [m, 'a:', l, @]).
entry('Mallinckrodt', [m, a, l, 'I', 'N', k, r, 'o:', t]).
entry('Mallorca', [m, a, j, 'O', '6', k, a]).
entry(mal, [m, 'a:', l]).
entry(ma, [m, a]).
entry('Mä', [m, 'E:']).
entry('Mammet', [m, a, m, e, t]).
entry(managen, [m, 'E', n, 'I', d, 'Z', @, n]).
entry('Manager', [m, 'E', n, 'I', d, 'Z', '6']).
entry(manche, [m, a, n, 'C', @]).
entry(manchmal, [m, a, n, 'C', m, 'a:', l]).
entry(mangiare, [m, a, n, d, 'Z', 'a:', r, @]).
entry(man, [m, a, n]).
entry('Männern', [m, 'E', n, '6', n]).
entry('Männer-Selbsterfahrungs-Gruppe', [m, 'E', n, '6', z, 'E', l, p, s, t, 'E', '6', f, 'a:', r, 'U', 'N', s, g, r, 'U', p, @]).
entry('Mannheim', [m, a, n, h, aI, m]).
entry('Mannhein', [m, a, n, h, aI, n]).
entry('Manning', [m, 'E', n, 'I', 'N']).
entry('Mappe', [m, a, p, @]).
entry('Mara', [m, 'a:', r, a]).
entry('Maredo', [m, a, r, 'e:', d, o]).
entry('Maria-Himmelfah', [m, a, r, 'i:', a, h, 'I', m, @, l, f, 'a:']).
entry('Maria-Himmelfahr', [m, a, r, 'i:', a, h, 'I', m, @, l, f, a, r]).
entry('Maria-Himmelfahrt', [m, a, r, 'i:', a, h, 'I', m, @, l, f, a, r, t]).
entry('Mariä-Himmelfahrt', [m, a, r, 'i:', 'E', h, 'I', m, @, l, f, a, r, t]).
entry('Marie', [m, a, r, 'i:']).
entry('Marienplatz', [m, a, r, 'i:', @, n, p, l, a, ts]).
entry('Marionettentheater', [m, a, r, j, o, n, 'E', t, @, n, t, e, 'a:', t, '6']).
entry('Marion', [m, 'a:', r, j, 'O', n]).
entry('Maritä', [m, a, r, 'I', t, 'E:']).
entry('Maritim-Airport-Hotel', [m, a, r, i, t, 'i:', m, 'E', '6', p, 'O', '6', t, h, o, t, 'E', l]).
entry('Maritim-Grand-Hotel', [m, a, r, i, t, 'i:', m, g, r, 'a~:', h, o, t, 'E', l]).
entry('Maritim-Hotel', [m, a, r, i, t, 'i:', m, h, o, t, 'E', l]).
entry('Maritim-Hotels', [m, a, r, i, t, 'i:', m, h, o, t, 'E', l, s]).
entry('Maritim', [m, a, r, i, t, 'i:', m]).
entry('Maritim-Stadthotel', [m, a, r, i, t, 'i:', m, 'S', t, a, t, h, o, t, 'E', l]).
entry('Mariusch', [m, a, r, i, 'U', 'S']).
entry('Markgrafen', [m, a, r, k, g, r, 'a:', f, @, n]).
entry('Mark', [m, a, r, k]).
entry('Markt', [m, a, r, k, t]).
entry('Mär', [m, 'E', '6']).
entry('Marmelade', [m, a, r, m, @, l, 'a:', d, @]).
entry('Marriott-Ho', [m, 'E', r, 'I', j, 'E', t, h, o]).
entry('Marriott-Hotel', [m, 'E', r, 'I', j, 'E', t, h, o, t, 'E', l]).
entry('Marriott', [m, 'E', r, 'I', j, 'E', t]).
entry('Marriotts', [m, 'E', r, 'I', j, 'E', ts]).
entry('Martha', [m, a, r, t, a]).
entry('Martin', [m, a, r, t, 'i:', n]).
entry('März', [m, 'E', '6', ts]).
entry('Märztagen', [m, 'E', '6', ts, t, 'a:', g, @, n]).
entry('Märzwoche', [m, 'E', '6', ts, v, 'O', x, @]).
entry('Märzwochenende', [m, 'E', '6', ts, v, 'O', x, @, n, 'E', n, d, @]).
entry('Maschine', [m, a, 'S', 'i:', n, @]).
entry('Maschsee', [m, a, 'S', z, 'e:']).
entry('Maschsee-Tage', [m, a, 'S', z, 'e:', t, 'a:', g, @]).
entry(mas, [m, 'a:', s]).
entry('Master-Card', [m, 'a:', s, t, '6', k, 'a:', r, d]).
entry('Material', [m, a, t, e, '6', j, 'a:', l]).
entry('Matinin', [m, 'a:', t, i, n, 'I', n]).
entry('Matthäi', [m, a, t, 'E:', i]).
entry('Mattheis', [m, a, t, aI, s]).
entry('Matthias', [m, a, t, 'i:', a, s]).
entry('Matysiak', [m, a, t, 'I', s, j, a, k]).
entry(mau, [m, aU]).
entry(maximal, [m, a, k, s, i, m, 'a:', l]).
entry('Maxim', [m, a, k, s, 'i:', m]).
entry('Max', [m, a, k, s]).
entry('Mecklenheide', [m, 'E', k, l, @, n, h, aI, d, @]).
entry('Medien', [m, 'e:', d, j, @, n]).
entry('Meesters', [m, 'e:', s, t, '6', s]).
entry(meeting, [m, 'i:', t, 'I', 'N']).
entry(mehrere, [m, 'e:', r, @, r, @]).
entry(mehreren, [m, 'e:', r, @, r, @, n]).
entry(mehrmals, [m, 'e:', '6', m, 'a:', l, s]).
entry(mehr, [m, 'e:', '6']).
entry('Meilen', [m, aI, l, @, n]).
entry(meine, [m, aI, n, @]).
entry(meinem, [m, aI, n, @, m]).
entry(meinen, [m, aI, n, @, n]).
entry(meiner, [m, aI, n, '6']).
entry(meinerseits, [m, aI, n, '6', z, aI, ts]).
entry(meines, [m, aI, n, @, s]).
entry(meinethalben, [m, aI, n, @, t, h, a, l, b, @, n]).
entry(meinetwegen, [m, aI, n, @, t, v, 'e:', g, @, n]).
entry(mein, [m, aI, n]).
entry(meinst, [m, aI, n, s, t]).
entry(meinte, [m, aI, n, t, @]).
entry(meinten, [m, aI, n, t, @, n]).
entry(meintest, [m, aI, n, t, @, s, t]).
entry(meint, [m, aI, n, t]).
entry('Meinung', [m, aI, n, 'U', 'N']).
entry(meistbietend, [m, aI, s, t, b, 'i:', t, 'E', n, t]).
entry(meiste, [m, aI, s, t, @]).
entry(meisten, [m, aI, s, t, @, n]).
entry(meistens, [m, aI, s, t, @, n, s]).
entry('Meldau', [m, 'E', l, d, aU]).
entry(melde, [m, 'E', l, d, @]).
entry(melden, [m, 'E', l, d, @, n]).
entry(meldet, [m, 'E', l, d, @, t]).
entry(me, [m, e]).
entry('Me', [m, 'E']).
entry('Mengele', [m, 'E', 'N', @, l, @]).
entry('Menge', [m, 'E', 'N', @]).
entry('Mensa', [m, 'E', n, z, a]).
entry('Menschen', [m, 'E', n, 'S', @, n]).
entry(menschliche, [m, 'E', n, 'S', l, 'I', 'C', @]).
entry(menschlichen, [m, 'E', n, 'S', l, 'I', 'C', @, n]).
entry('Mensch', [m, 'E', n, 'S']).
entry('Mercure', [m, 'E', '6', k, 'y:', '6']).
entry('Mergentheim', [m, 'E', '6', g, @, n, t, h, aI, m]).
entry(merke, [m, 'E', '6', k, @]).
entry(merken, [m, 'E', '6', k, @, n]).
entry(merkt, [m, 'E', '6', k, t]).
entry('Merkur', [m, 'E', '6', k, 'U', '6']).
entry(merkwürdigerweise, [m, 'E', '6', k, v, 'Y', '6', d, 'I', g, '6', v, aI, z, @]).
entry(merkwürdig, [m, 'E', '6', k, v, 'Y', '6', d, 'I', 'C']).
entry('Merrettig', [m, e, r, 'E', t, 'I', 'C']).
entry('Messegelände', [m, 'E', s, @, g, @, l, 'E', n, d, @]).
entry('Messehalle', [m, 'E', s, @, h, a, l, @]).
entry('Messehallen', [m, 'E', s, @, h, a, l, @, n]).
entry('Messe', [m, 'E', s, @]).
entry('Messezeit', [m, 'E', s, @, ts, aI, t]).
entry('Mess', [m, 'E', s]).
entry(metaphysischen, [m, e, t, a, f, 'y:', z, 'I', 'S', @, n]).
entry('Meter', [m, 'e:', t, '6']).
entry('Metze', [m, 'E', ts, @]).
entry('Meurer', [m, 'OY', r, '6']).
entry('Mexikaner', [m, 'E', k, s, i, k, 'a:', n, '6']).
entry(mexikanische, [m, 'E', k, s, i, k, 'a:', n, 'I', 'S', @]).
entry(mexikanisch, [m, 'E', k, s, i, k, 'a:', n, 'I', 'S']).
entry(mhm, [m, h, m]).
entry('Michaelis', [m, 'I', 'C', a, 'e:', l, 'I', s]).
entry('Michael', [m, 'I', 'C', a, 'e:', l]).
entry(mich, [m, 'I', 'C']).
entry('Microsoft', [m, aI, k, r, o, s, 'O', f, t]).
entry(mies, [m, 'i:', s]).
entry('Mietautos', [m, 'i:', t, aU, t, o, s]).
entry(mieten, [m, 'i:', t, @, n]).
entry('Mietwagen', [m, 'i:', t, v, 'a:', g, @, n]).
entry('Mifune', [m, i, f, 'u:', n, @]).
entry(mijad, [m, i, j, a, t]).
entry('Mila', [m, 'i:', l, a]).
entry(milderes, [m, 'I', l, d, @, r, @, s]).
entry('Miles-and-More-Card', [m, aI, l, s, 'E', n, t, m, 'O', '6', k, a, r, t]).
entry('Miles-and-More-Karte', [m, aI, l, s, 'E', n, t, m, 'O', '6', k, a, r, t, @]).
entry(miles, [m, aI, l, z]).
entry(mi, [m, 'I']).
entry('Minaty', [m, i, n, a, t, i]).
entry('Minden', [m, 'I', n, d, @, n]).
entry(minder, [m, 'I', n, d, '6']).
entry(mindestens, [m, 'I', n, d, @, s, t, @, n, s]).
entry('Minibar', [m, 'I', n, i, b, 'a:', r]).
entry('Miniblock', [m, 'I', n, i, b, l, 'O', k]).
entry('Minimum', [m, 'i:', n, i, m, 'U', m]).
entry('Ministerin', [m, i, n, 'I', s, t, @, r, 'I', n]).
entry('Ministerium', [m, i, n, 'I', s, t, 'e:', '6', j, 'U', m]).
entry('Minister', [m, i, n, 'I', s, t, '6']).
entry(minkalender, [m, 'i:', n, k, a, l, 'E', n, d, '6']).
entry(min, [m, 'I', n]).
entry(minus, [m, 'i:', n, 'U', s]).
entry('Minute', [m, i, n, 'u:', t, @]).
entry('Minuten', [m, i, n, 'u:', t, @, n]).
entry('Minut', [m, i, n, 'u:', t]).
entry('Miriam', [m, i, r, i, a, m]).
entry(mir, [m, 'i:', '6']).
entry('Mister', [m, 'I', s, t, '6']).
entry(mißverstanden, [m, 'I', s, f, 'E', '6', 'S', t, a, n, d, @, n]).
entry('Mißverständnis', [m, 'I', s, f, 'E', '6', 'S', t, 'E', n, t, n, 'I', s]).
entry(mitarbeiten, [m, 'I', t, a, r, b, aI, t, @, n]).
entry('Mitarbeiterbesprechung', [m, 'I', t, a, r, b, aI, t, '6', b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Mitarbeiterin', [m, 'I', t, a, r, b, aI, t, @, r, 'I', n]).
entry('Mitarbeiter', [m, 'I', t, a, r, b, aI, t, '6']).
entry('Mitarbeitern', [m, 'I', t, a, r, b, aI, t, '6', n]).
entry('Mitarbeit', [m, 'I', t, a, r, b, aI, t]).
entry(mitbekommen, [m, 'I', t, b, @, k, 'O', m, @, n]).
entry(mitbestellen, [m, 'I', t, b, @, 'S', t, 'E', l, @, n]).
entry(mitbringe, [m, 'I', t, b, r, 'I', 'N', @]).
entry(mitbringen, [m, 'I', t, b, r, 'I', 'N', @, n]).
entry('Mitbringsel', [m, 'I', t, b, r, 'I', 'N', z, @, l]).
entry(mitbuchen, [m, 'I', t, b, 'u:', x, @, n]).
entry(miteinander, [m, 'I', t, aI, n, a, n, d, '6']).
entry(miteinbeziehen, [m, 'I', t, aI, n, b, @, ts, 'i:', @, n]).
entry('Miteinbeziehung', [m, 'I', t, aI, n, b, @, ts, 'i:', 'U', 'N']).
entry(miteinbringen, [m, 'I', t, aI, n, b, r, 'I', 'N', @, n]).
entry(mitein, [m, 'I', t, aI, n]).
entry(miteinzubeziehen, [m, 'I', t, aI, n, ts, u, b, @, ts, 'i:', @, n]).
entry(mitessen, [m, 'I', t, 'E', s, @, n]).
entry(mitfahren, [m, 'I', t, f, 'a:', r, @, n]).
entry(mitgeben, [m, 'I', t, g, 'e:', b, @, n]).
entry(mitgebracht, [m, 'I', t, g, @, b, r, a, x, t]).
entry(mitgegeben, [m, 'I', t, g, @, g, 'e:', b, @, n]).
entry(mitgehen, [m, 'I', t, g, 'e:', @, n]).
entry(mitgekriegt, [m, 'I', t, g, @, k, r, 'i:', k, t]).
entry(mitgemacht, [m, 'I', t, g, @, m, a, x, t]).
entry(mitgenommen, [m, 'I', t, g, @, n, 'O', m, @, n]).
entry(mitgeschrieben, [m, 'I', t, g, @, 'S', r, 'i:', b, @, n]).
entry(mitgeteilt, [m, 'I', t, g, @, t, aI, l, t]).
entry(mitgetei, [m, 'I', t, g, @, t, aI]).
entry('Mitglied', [m, 'I', t, g, l, 'i:', t]).
entry(mithineinnehmen, [m, 'I', t, h, 'I', n, aI, n, n, 'e:', m, @, n]).
entry(mithin, [m, 'I', t, h, 'I', n]).
entry(mitkommen, [m, 'I', t, k, 'O', m, @, n]).
entry(mitkommst, [m, 'I', t, k, 'O', m, s, t]).
entry(mitkommt, [m, 'I', t, k, 'O', m, t]).
entry(mitmache, [m, 'I', t, m, a, x, @]).
entry(mitmachen, [m, 'I', t, m, a, x, @, n]).
entry(mit, [m, 'I', t]).
entry(mitnehmen, [m, 'I', t, n, 'e:', m, @, n]).
entry(mitneh, [m, 'I', t, n, 'e:']).
entry(mitnimmt, [m, 'I', t, n, 'I', m, t]).
entry(mitnotieren, [m, 'I', t, n, o, t, 'i:', r, @, n]).
entry(mitreißen, [m, 'I', t, r, aI, s, @, n]).
entry('Mitropa', [m, i, t, r, 'o:', p, a]).
entry(mitschicken, [m, 'I', tS, 'I', k, @, n]).
entry('Mitschreiben', [m, 'I', tS, r, aI, b, @, n]).
entry(mitspielt, [m, 'I', tS, p, 'i:', l, t]).
entry(mittage, [m, 'I', t, 'a:', g, @]).
entry(mittagessen, [m, 'I', t, 'a:', k, 'E', s, @, n]).
entry(mittag, [m, 'I', t, 'a:', k]).
entry('Mittagsküche', [m, 'I', t, 'a:', k, s, k, 'Y', 'C', @]).
entry(mittags, [m, 'I', t, 'a:', k, s]).
entry('Mittagspause', [m, 'I', t, 'a:', k, s, p, aU, z, @]).
entry('Mittagstermin', [m, 'I', t, 'a:', k, s, t, 'E', '6', m, 'i:', n]).
entry('Mittagstisch', [m, 'I', t, 'a:', k, s, t, 'I', 'S']).
entry('Mittagszeit', [m, 'I', t, 'a:', k, s, ts, aI, t]).
entry(mitteilen, [m, 'I', t, t, aI, l, @, n]).
entry('Mitteilungen', [m, 'I', t, t, aI, l, 'U', 'N', @, n]).
entry('Mittelfeld', [m, 'I', t, @, l, f, 'E', l, t]).
entry('Mittelklas', [m, 'I', t, @, l, k, l, a, s]).
entry('Mittelklasse-Hotel', [m, 'I', t, @, l, k, l, a, s, @, h, o, t, 'E', l]).
entry('Mittelklasse', [m, 'I', t, @, l, k, l, a, s, @]).
entry(mittel, [m, 'I', t, @, l]).
entry('Mitteln', [m, 'I', t, @, l, n]).
entry('Mittelstands-Hotel', [m, 'I', t, @, l, 'S', t, a, n, ts, h, o, t, 'E', l]).
entry('Mitte', [m, 'I', t, @]).
entry(mittendrin, [m, 'I', t, @, n, d, r, 'I', n]).
entry(mitten, [m, 'I', t, @, n]).
entry(mitteren, [m, 'I', t, @, r, @, n]).
entry('Mitternacht', [m, 'I', t, '6', n, a, x, t]).
entry(mittlere, [m, 'I', t, l, @, r, @]).
entry(mittlerem, [m, 'I', t, l, @, r, @, m]).
entry(mittleren, [m, 'I', t, l, @, r, @, n]).
entry(mittlerer, [m, 'I', t, l, @, r, '6']).
entry('Mittler', [m, 'I', t, l, '6']).
entry(mittlerweile, [m, 'I', t, l, '6', v, aI, l, @]).
entry('Mittw', [m, 'I', t, v]).
entry('Mittwoche', [m, 'I', t, v, 'O', x, @]).
entry('Mittwochen', [m, 'I', t, v, 'O', x, @, n]).
entry('Mittwoch', [m, 'I', t, v, 'O', x]).
entry(mittwochs, [m, 'I', t, v, 'O', x, s]).
entry('Mittwochstermin', [m, 'I', t, v, 'O', x, s, t, 'E', '6', m, 'i:', n]).
entry('Mittwo', [m, 'I', t, v, 'O']).
entry(mitziehen, [m, 'I', t, ts, 'i:', @, n]).
entry(mitzub, [m, 'I', t, ts, u, b]).
entry(mitzubringen, [m, 'I', t, ts, u, b, r, 'I', 'N', @, n]).
entry(mitzugehen, [m, 'I', t, ts, u, g, 'e:', @, n]).
entry(mitzukommen, [m, 'I', t, ts, u, k, 'O', m, @, n]).
entry(mitzukriegen, [m, 'I', t, ts, u, k, r, 'i:', g, @, n]).
entry(mitzumachen, [m, 'I', t, ts, u, m, a, x, @, n]).
entry(mitzunehmen, [m, 'I', t, ts, u, n, 'e:', m, @, n]).
entry(mm, [m, m]).
entry(moa, [m, o, a]).
entry('Mobil-Box', [m, o, b, 'i:', l, b, 'O', k, s]).
entry(mobil, [m, o, b, 'i:', l]).
entry(möch, [m, '9', 'C']).
entry(moch, [m, 'O', x]).
entry(möchte, [m, '9', 'C', t, @]).
entry(möchten, [m, '9', 'C', t, @, n]).
entry(möchtest, [m, '9', 'C', t, @, s, t]).
entry('Möckl', [m, '9', k, l]).
entry(modalitäten, [m, o, d, a, l, i, t, 'E:', t, @, n]).
entry('Modell', [m, o, d, 'E', l]).
entry(moderat, [m, o, d, e, r, 'a:', t]).
entry(moderne, [m, o, d, 'E', '6', n, @]).
entry(moderner, [m, o, d, 'E', '6', n, '6']).
entry('Modus', [m, 'o:', d, 'U', s]).
entry('Moers', [m, '9', '6', s]).
entry(mögen, [m, '2:', g, @, n]).
entry('Mogen', [m, 'o:', g, @, n]).
entry(mögliche, [m, '2:', k, l, 'I', 'C', @]).
entry(möglichen, [m, '2:', k, l, 'I', 'C', @, n]).
entry(möglicher, [m, '2:', k, l, 'I', 'C', '6']).
entry(möglicherweise, [m, '2:', k, l, 'I', 'C', '6', v, aI, z, @]).
entry('Möglichkei', [m, '2:', k, l, 'I', 'C', k, aI]).
entry(möglichkeiten, [m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry(möglichkeit, [m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Möglichke', [m, '2:', k, l, 'I', 'C', k, @]).
entry(möglich, [m, '2:', k, l, 'I', 'C']).
entry(möglichst, [m, '2:', k, l, 'I', 'C', s, t]).
entry(mögli, [m, '2:', k, l, 'I']).
entry(mögl, [m, '2:', k, l]).
entry(mög, [m, '2:', k]).
entry('Mohr', [m, 'o:', '6']).
entry(moin, [m, 'OY', n]).
entry('Möllenkamp', [m, '9', l, @, n, k, a, m, p]).
entry(mö, [m, '9']).
entry('Momen', [m, o, m, 'E', n]).
entry(momentan, [m, o, m, 'E', n, t, 'a:', n]).
entry('Momentchen', [m, o, m, 'E', n, t, 'C', @, n]).
entry('Moment', [m, o, m, 'E', n, t]).
entry(mo, [m, o]).
entry('Monaco', [m, 'o:', n, a, k, o]).
entry('Monate', [m, 'o:', n, a, t, @]).
entry('Monaten', [m, 'o:', n, a, t, @, n]).
entry('Monat', [m, 'o:', n, a, t]).
entry('Monatsanfang', [m, 'o:', n, a, ts, a, n, f, a, 'N']).
entry('Monatsende', [m, 'o:', n, a, ts, 'E', n, d, @]).
entry('Monatskalender', [m, 'o:', n, a, ts, k, a, l, 'E', n, d, '6']).
entry('Monatskalenders', [m, 'o:', n, a, ts, k, a, l, 'E', n, d, '6', s]).
entry('Monatskalen', [m, 'o:', n, a, ts, k, a, l, 'E', n]).
entry('Monatsmitte', [m, 'o:', n, a, ts, m, 'I', t, @]).
entry('Monats', [m, 'o:', n, a, ts]).
entry('Monatsplan', [m, 'o:', n, a, ts, p, l, 'a:', n]).
entry('Monatswechsel', [m, 'o:', n, a, ts, v, 'E', k, s, @, l]).
entry('Mönchengladbach', [m, '9', n, 'C', @, n, g, l, a, t, b, a, x]).
entry('Mon', [m, o, n]).
entry(montag, [m, 'o:', n, t, 'a:', k]).
entry(montags, [m, 'o:', n, t, 'a:', k, s]).
entry('Montagtermin', [m, 'o:', n, t, 'a:', k, t, 'E', '6', m, 'i:', n]).
entry('Monta', [m, 'o:', n, t, 'a:']).
entry('Mont', [m, 'o:', n, t]).
entry('Mooshammer', [m, 'o:', s, h, a, m, '6']).
entry('Morgenmensch', [m, 'O', '6', g, @, n, m, 'E', n, 'S']).
entry(morgen, [m, 'O', '6', g, @, n]).
entry('Morgen-Produktivität', [m, 'O', '6', g, @, n, p, r, o, d, 'U', k, t, i, v, i, t, 'E:', t]).
entry(morgens, [m, 'O', '6', g, @, n, s]).
entry('Morgenzug', [m, 'O', '6', g, @, n, ts, 'u:', k]).
entry(morgige, [m, 'O', '6', g, 'I', g, @]).
entry(morgigen, [m, 'O', '6', g, 'I', g, @, n]).
entry(morgiger, [m, 'O', '6', g, 'I', g, '6']).
entry('Mor', [m, 'O', '6']).
entry(mösch, [m, '9', 'S']).
entry(mosert, [m, 'o:', z, '6', t]).
entry(motiviert, [m, o, t, i, v, 'i:', '6', t]).
entry('Motorsäge', [m, 'o:', t, 'o:', '6', z, 'E:', g, @]).
entry(motzen, [m, 'O', ts, @, n]).
entry(mou, [m, 'o:']).
entry('Mövenpick', [m, '2:', v, @, n, p, 'I', k]).
entry(m, ['E', m]).
entry('Mrs', [m, 'I', s, 'I', z]).
entry('Mt', [m, t]).
entry('Much', [m, 'U', x]).
entry(müde, [m, 'y:', d, @]).
entry('Mueller', [m, 'Y', l, '6']).
entry('Mühe', [m, 'y:', @]).
entry('Mühlbauer', [m, 'y:', l, b, aU, '6']).
entry('Mula', [m, 'u:', l, a]).
entry('Müller-Elektronik', [m, 'Y', l, '6', e, l, 'E', k, t, r, 'o:', n, 'I', k]).
entry('Müller-Lüdenscheidt', [m, 'Y', l, '6', l, 'y:', d, @, n, 'S', aI, t]).
entry('Müll', [m, 'Y', l]).
entry(mu, [m, 'U']).
entry(mü, [m, 'Y']).
entry('München-Hannover', [m, 'Y', n, 'C', @, n, h, a, n, 'o:', f, '6']).
entry('München', [m, 'Y', n, 'C', @, n]).
entry('Münchens', [m, 'y:', n, 'C', @, n, s]).
entry('Münch', [m, 'Y', n, 'C']).
entry('Münchner', [m, 'Y', n, 'C', @, n, '6']).
entry(munkelt, [m, 'U', 'N', k, @, l, t]).
entry('Münster', [m, 'Y', n, s, t, '6']).
entry(munter, [m, 'U', n, t, '6']).
entry('Münt', [m, 'Y', n, t]).
entry('Müria-Himmelfahrt', [m, y, r, 'i:', a, h, 'I', m, @, l, f, 'a:', r, t]).
entry('Muschweck', [m, 'U', 'S', v, 'E', k]).
entry('Museen', [m, u, z, 'e:', @, n]).
entry('Muße', [m, 'u:', s, @]).
entry('Museum', [m, u, z, 'e:', 'U', m]).
entry('Museumsbesuche', [m, u, z, 'e:', 'U', m, s, b, @, z, 'u:', x, @]).
entry('Museumsufer', [m, u, z, 'e:', 'U', m, s, 'u:', f, '6']).
entry('Museun', [m, u, z, 'e:', n]).
entry('Musicalbesuch', [m, j, 'u:', z, i, k, @, l, b, @, z, 'u:', x]).
entry('Musicalfan', [m, j, 'u:', z, i, k, @, l, f, 'E', n]).
entry('Musical', [m, j, 'u:', z, i, k, @, l]).
entry('Musicals', [m, j, 'u:', z, i, k, @, l, s]).
entry(musikbegeistert, [m, u, z, 'i:', k, b, @, g, aI, s, t, '6', t]).
entry('Musik', [m, u, z, 'i:', k]).
entry('Mußmann', [m, 'U', s, m, a, n]).
entry(muß, [m, 'U', s]).
entry(müß, [m, 'Y', s]).
entry(müssen, [m, 'Y', s, @, n]).
entry(mußte, [m, 'U', s, t, @]).
entry(müßte, [m, 'Y', s, t, @]).
entry(mußten, [m, 'U', s, t, @, n]).
entry(müßten, [m, 'Y', s, t, @, n]).
entry(müßtest, [m, 'Y', s, t, @, s, t]).
entry(mußt, [m, 'U', s, t]).
entry('Mutlu', [m, 'U', t, l, u]).
entry('Mutterhaus', [m, 'U', t, '6', h, aU, s]).
entry('Mutter', [m, 'U', t, '6']).
entry('Muttertag', [m, 'U', t, '6', t, 'a:', k]).
entry('Myrda', [m, 'Y', '6', d, a]).
entry(nacharbeiten, [n, 'a:', x, a, r, b, aI, t, @, n]).
entry(nachbereiten, [n, 'a:', x, b, @, r, aI, t, @, n]).
entry('Nachbereitung', [n, 'a:', x, b, @, r, aI, t, 'U', 'N']).
entry('Nachbereitungstreffen', [n, 'a:', x, b, @, r, aI, t, 'U', 'N', s, t, r, 'E', f, @, n]).
entry(nachbesprechen, [n, 'a:', x, b, @, 'S', p, r, 'E', 'C', @, n]).
entry('Nachbesprechungen', [n, 'a:', x, b, @, 'S', p, r, 'E', 'C', 'U', 'N', @, n]).
entry('Nachbesprechung', [n, 'a:', x, b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry(nachdem, [n, a, x, d, 'e:', m]).
entry(nachdenken, [n, 'a:', x, d, 'E', 'N', k, @, n]).
entry(nachdenkt, [n, 'a:', x, d, 'E', 'N', k, t]).
entry(nacheinander, [n, 'a:', x, aI, n, a, n, d, '6']).
entry('Nachfrage', [n, 'a:', x, f, r, 'a:', g, @]).
entry(nachfragen, [n, 'a:', x, f, r, 'a:', g, @, n]).
entry(nachfühlen, [n, 'a:', x, f, 'y:', l, @, n]).
entry(nachgedacht, [n, 'a:', x, g, @, d, a, x, t]).
entry(nachgehen, [n, 'a:', x, g, 'e:', @, n]).
entry(nachgekuckt, [n, 'a:', x, g, @, k, 'U', k, t]).
entry(nachgeschaut, [n, 'a:', x, g, @, 'S', aU, t]).
entry(nachgesehen, [n, 'a:', x, g, @, z, 'e:', @, n]).
entry(nachgesendet, [n, 'a:', x, g, @, z, 'E', n, d, @, t]).
entry(nachgezählt, [n, 'a:', x, g, @, ts, 'E:', l, t]).
entry(nachgiebig, [n, 'a:', x, g, 'i:', b, 'I', 'C']).
entry(nachgucken, [n, 'a:', x, g, 'U', k, @, n]).
entry(nachhängen, [n, 'a:', x, h, 'E', 'N', @, n]).
entry('Nachhauseweg', [n, 'a:', x, h, aU, z, @, v, 'e:', k]).
entry(nachher, [n, 'a:', x, h, 'e:', '6']).
entry(nachhinein, [n, 'a:', x, h, 'I', n, aI, n]).
entry(nachix, [n, 'a:', x, 'I', k, s]).
entry(nachkomme, [n, 'a:', x, k, 'O', m, @]).
entry(nachkommen, [n, 'a:', x, k, 'O', m, @, n]).
entry(nachkucken, [n, 'a:', x, k, 'U', k, @, n]).
entry(nachmi, [n, 'a:', x, m, 'I']).
entry(nachmittag, [n, 'a:', x, m, 'I', t, 'a:', k]).
entry('Nachmittagsbereich', [n, 'a:', x, m, 'I', t, 'a:', k, s, b, @, r, aI, 'C']).
entry('Nachmittags-Maschine', [n, 'a:', x, m, 'I', t, 'a:', k, s, m, a, 'S', 'i:', n, @]).
entry(nachmittags, [n, 'a:', x, m, 'I', t, 'a:', k, s]).
entry('Nachmittagsstunden', [n, 'a:', x, m, 'I', t, 'a:', k, s, 'S', t, 'U', n, d, @, n]).
entry('Nachmittagstermin', [n, 'a:', x, m, 'I', t, 'a:', k, s, t, 'E', '6', m, 'i:', n]).
entry('Nachmittagstreffen', [n, 'a:', x, m, 'I', t, 'a:', k, s, t, r, 'E', f, @, n]).
entry(nachmitt, [n, 'a:', x, m, 'I', t]).
entry('Nachnamen', [n, 'a:', x, n, 'a:', m, @, n]).
entry(nach, [n, 'a:', x]).
entry(näch, [n, 'E:', 'C']).
entry(nachprüfen, [n, 'a:', x, p, r, 'y:', f, @, n]).
entry(nachrechnen, [n, 'a:', x, r, 'E', 'C', n, @, n]).
entry('Nachricht', [n, 'a:', x, r, 'I', 'C', t]).
entry(nachschauen, [n, 'a:', x, 'S', aU, @, n]).
entry(nachschlagen, [n, 'a:', x, 'S', l, 'a:', g, @, n]).
entry(nachsch, [n, 'a:', x, 'S']).
entry(nachsehen, [n, 'a:', x, z, 'e:', @, n]).
entry(nächs, [n, 'E:', 'C', s]).
entry(nächstbilligere, [n, 'E:', 'C', s, t, b, 'I', l, 'I', g, @, r, @]).
entry(nächste, [n, 'E:', 'C', s, t, @]).
entry(nächsten, [n, 'E:', 'C', s, t, @, n]).
entry(nächster, [n, 'E:', 'C', s, t, '6']).
entry(nächstes, [n, 'E:', 'C', s, t, @, s]).
entry(nächstfreier, [n, 'E:', 'C', s, t, f, r, aI, '6']).
entry(nächstfrühere, [n, 'E:', 'C', s, t, f, r, 'y:', @, r, @]).
entry(nächstjährige, [n, 'E:', 'C', s, t, j, 'E:', r, 'I', g, @]).
entry(nächstkommenden, [n, 'E:', 'C', s, t, k, 'O', m, @, n, d, @, n]).
entry(nächstmögliche, [n, 'E:', 'C', s, t, m, '2:', k, l, 'I', 'C', @]).
entry(nächstmöglichen, [n, 'E:', 'C', s, t, m, '2:', k, l, 'I', 'C', @, n]).
entry('Nachst', [n, 'a:', x, s, t]).
entry(nächst, [n, 'E:', 'C', s, t]).
entry('Nachteil', [n, 'a:', x, t, aI, l]).
entry('Nächte', [n, 'E', 'C', t, @]).
entry('Nächten', [n, 'E', 'C', t, @, n]).
entry('Nachtexpreß', [n, a, x, t, 'E', k, s, p, r, 'E', s]).
entry(nächtigen, [n, 'E', 'C', t, 'I', g, @, n]).
entry('Nachtleben', [n, a, x, t, l, 'e:', b, @, n]).
entry(nacht, [n, a, x, t]).
entry('Nachtreffen', [n, 'a:', x, t, r, 'E', f, @, n]).
entry('Nachtricht', [n, 'a:', x, t, r, 'I', 'C', t]).
entry(nachts, [n, a, x, ts]).
entry('Nachtzüge', [n, a, x, t, ts, 'y:', g, @]).
entry('Nachtzug', [n, a, x, t, ts, 'u:', k]).
entry(nachzählen, [n, 'a:', x, ts, 'E:', l, @, n]).
entry(nachzuarbeiten, [n, 'a:', x, ts, u, a, r, b, aI, t, @, n]).
entry(nachzubesprechen, [n, 'a:', x, ts, u, b, @, 'S', p, r, 'E', 'C', @, n]).
entry('Nadolmy', [n, a, d, 'O', l, m, i]).
entry('Nadolny', [n, a, d, 'O', l, n, i]).
entry('Nagel', [n, 'a:', g, @, l]).
entry('Nägel', [n, 'E:', g, @, l]).
entry(nahegelegenen, [n, 'a:', @, g, @, l, 'e:', g, @, n, @, n]).
entry(nahe, [n, 'a:', @]).
entry('Nähe', [n, 'E:', @]).
entry(nähere, [n, 'E:', @, r, @]).
entry(näheren, [n, 'E:', @, r, @, n]).
entry(näherer, [n, 'E:', @, r, '6']).
entry('Näheres', [n, 'E:', @, r, @, s]).
entry(näher, [n, 'E:', '6']).
entry(nähern, [n, 'E:', '6', n]).
entry(nähesten, [n, 'E:', @, s, t, @, n]).
entry('Nähler', [n, 'E:', l, '6']).
entry(nähme, [n, 'E:', m, @]).
entry(nähmen, [n, 'E:', m, @, n]).
entry(nah, [n, 'a:']).
entry(nak, [n, a, k]).
entry('Name', [n, 'a:', m, @]).
entry(name, [n, e, 'I', m]).
entry('Namen', [n, 'a:', m, @, n]).
entry('Namensbestandteil', [n, 'a:', m, @, n, s, b, @, 'S', t, a, n, t, t, aI, l]).
entry(nämlich, [n, 'E:', m, l, 'I', 'C']).
entry(na, [n, a]).
entry(nä, [n, 'E:']).
entry(nat, [n, 'a:', t]).
entry(naturgemäß, [n, a, t, 'u:', '6', g, @, m, 'E:', s]).
entry(natürlich, [n, a, t, 'y:', '6', l, 'I', 'C']).
entry(nau, [n, aU]).
entry('Navigationssystemen', [n, a, v, i, g, a, ts, j, 'o:', n, s, z, 'Y', s, t, 'e:', m, @, n]).
entry(neas, [n, 'e:', a, s]).
entry('Nebel', [n, 'e:', b, @, l]).
entry('Nebels', [n, 'e:', b, @, l, s]).
entry(nebenan, [n, 'e:', b, @, n, a, n]).
entry(nebenbei, [n, 'e:', b, @, n, b, aI]).
entry('Nebeneffekt', [n, 'e:', b, @, n, 'E', f, 'E', k, t]).
entry(nebeneinander, [n, 'e:', b, @, n, aI, n, a, n, d, '6']).
entry(nebenher, [n, 'e:', b, @, n, h, 'e:', '6']).
entry(neben, [n, 'e:', b, @, n]).
entry('Neckermann', [n, 'E', k, '6', m, a, n]).
entry('Neck', [n, 'E', k]).
entry(nee, [n, 'e:']).
entry(negativen, [n, 'e:', g, a, t, 'i:', v, @, n]).
entry(negativ, [n, 'e:', g, a, t, 'i:', f]).
entry(nehme, [n, 'e:', m, @]).
entry(nehmen, [n, 'e:', m, @, n]).
entry(nehm, [n, 'e:', m]).
entry(nehn, [n, 'e:', n]).
entry(nehu, [n, 'e:', u]).
entry(neidisch, [n, aI, d, 'I', 'S']).
entry(nei, [n, aI]).
entry(nein, [n, aI, n]).
entry('Neldner', [n, 'E', l, d, n, '6']).
entry(nemm, [n, 'E', m]).
entry(ne, [n, 'E']).
entry(nen, [n, 'E', n]).
entry(nenne, [n, 'E', n, @]).
entry(nennen, [n, 'E', n, @, n]).
entry('Nenner', [n, 'E', n, '6']).
entry(nennt, [n, 'E', n, t]).
entry('Nermitz', [n, 'E', '6', m, 'I', ts]).
entry(nerven, [n, 'E', '6', f, @, n]).
entry(nervig, [n, 'E', '6', f, 'I', 'C']).
entry(nette, [n, 'E', t, @]).
entry(netten, [n, 'E', t, @, n]).
entry(netter, [n, 'E', t, '6']).
entry(nettes, [n, 'E', t, @, s]).
entry(nett, [n, 'E', t]).
entry('Netz', [n, 'E', ts]).
entry('Netzwerk', [n, 'E', ts, v, 'E', '6', k]).
entry('Neubeck', [n, 'OY', b, 'E', k]).
entry(neuem, [n, 'OY', @, m]).
entry(neuen, [n, 'OY', @, n]).
entry(neue, [n, 'OY', @]).
entry(neuerdings, [n, 'OY', '6', d, 'I', 'N', s]).
entry(neuer, [n, 'OY', '6']).
entry(neues, [n, 'OY', @, s]).
entry(neueste, [n, 'OY', @, s, t, @]).
entry(neugierig, [n, 'OY', g, 'i:', r, 'I', 'C']).
entry('Neuigkeiten', [n, 'OY', 'I', 'C', k, aI, t, @, n]).
entry('Neujahr', [n, 'OY', j, 'a:', r]).
entry(neulich, [n, 'OY', l, 'I', 'C']).
entry('Neumann-Häfelin', [n, 'OY', m, a, n, h, 'E', f, @, l, i, n]).
entry(neun, [n, 'OY', n]).
entry(neu, [n, 'OY']).
entry(neunten, [n, 'OY', n, t, @, n]).
entry(neunte, [n, 'OY', n, t, @]).
entry(neunter, [n, 'OY', n, t, '6']).
entry('Neun-Uhr-fünf-Maschine', [n, 'OY', n, 'u:', '6', f, 'Y', n, f, m, a, 'S', 'i:', n, @]).
entry(neununddreißig, [n, 'OY', n, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(neununddreißigsten, [n, 'OY', n, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(neunundfünfzig, [n, 'OY', n, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(neunundneunzig, [n, 'OY', n, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(neunund, [n, 'OY', n, 'U', n, t]).
entry(neunundsechzig, [n, 'OY', n, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(neunundvierzig, [n, 'OY', n, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(neunundz, [n, 'OY', n, 'U', n, t, ts]).
entry(neunundzwanzig, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(neunundzwanzigs, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s]).
entry(neunundzwanzigsten, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(neunundzwanzigste, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(neunundzwanzigster, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(neunundzwanzi, [n, 'OY', n, 'U', n, t, ts, v, a, n, ts, 'I']).
entry(neunun, [n, 'OY', n, 'U', n]).
entry(neunzehnhundert, [n, 'OY', n, ts, 'e:', n, h, 'U', n, d, '6', t]).
entry(neunzehn, [n, 'OY', n, ts, 'e:', n]).
entry(neunzehntem, [n, 'OY', n, ts, 'e:', n, t, @, m]).
entry(neunzehnten, [n, 'OY', n, ts, 'e:', n, t, @, n]).
entry(neunzehnte, [n, 'OY', n, ts, 'e:', n, t, @]).
entry(neunzehnter, [n, 'OY', n, ts, 'e:', n, t, '6']).
entry('Neunzehn-Uhr-Termin', [n, 'OY', n, ts, 'e:', n, 'u:', '6', t, 'E', '6', m, 'i:', n]).
entry(neunze, [n, 'OY', n, ts, e]).
entry(neunzig, [n, 'OY', n, ts, 'I', 'C']).
entry(neunzigsten, [n, 'OY', n, ts, 'I', 'C', s, t, @, n]).
entry(neunz, [n, 'OY', n, ts]).
entry('Neuperlach', [n, 'OY', p, 'E', '6', l, a, x]).
entry('Neureichen', [n, 'OY', r, aI, 'C', @, n]).
entry(neusten, [n, 'OY', s, t, @, n]).
entry(neutralem, [n, 'OY', t, r, 'a:', l, @, m]).
entry(neutralen, [n, 'OY', t, r, 'a:', l, @, n]).
entry('Nev', [n, 'E', f]).
entry('New-Yorker', [n, j, u, j, 'O', '6', k, '6']).
entry('New-York', [n, j, u, j, 'O', '6', k]).
entry('Next-Step', [n, 'E', k, s, ts, t, 'E', p]).
entry(nge, ['N', @]).
entry(ngste, ['N', s, t, @]).
entry(nich, [n, 'I', 'C']).
entry(nicht, [n, 'I', 'C', t]).
entry('Nichtraucher-Abteil', [n, 'I', 'C', t, r, aU, x, '6', a, p, t, aI, l]).
entry('Nichtraucher-Etage', [n, 'I', 'C', t, r, aU, x, '6', e, t, 'a:', 'Z', @]).
entry('Nichtraucher-Flüge', [n, 'I', 'C', t, r, aU, x, '6', f, l, 'y:', g, @]).
entry('Nichtraucherin', [n, 'I', 'C', t, r, aU, x, @, r, 'I', n]).
entry('Nichtraucher', [n, 'I', 'C', t, r, aU, x, '6']).
entry('nichtraucher-speziellem', [n, 'I', 'C', t, r, aU, x, '6', 'S', p, e, ts, j, 'E', l, @, m]).
entry('Nichtraucher-Trakt', [n, 'I', 'C', t, r, aU, x, '6', t, r, a, k, t]).
entry('Nichtraucher-Zimmer', [n, 'I', 'C', t, r, aU, x, '6', ts, 'I', m, '6']).
entry('Nichtrauerabt', [n, 'I', 'C', t, r, aU, @, r, 'a:', p, t]).
entry('Nichtrau', [n, 'I', 'C', t, r, aU]).
entry(nichts, [n, 'I', 'C', ts]).
entry(nichtssagende, [n, 'I', 'C', ts, z, 'a:', g, @, n, d, @]).
entry('Nicklas', [n, 'I', k, l, a, s]).
entry('Nicole', [n, i, k, 'O', l]).
entry('Niederlande', [n, 'i:', d, '6', l, a, n, d, @]).
entry('Niederlasslung', [n, 'i:', d, '6', l, a, s, l, 'U', 'N']).
entry('Niederlassung', [n, 'i:', d, '6', l, a, s, 'U', 'N']).
entry('Niedersachsen', [n, 'i:', d, '6', z, a, k, s, @, n]).
entry('Niederschrift', [n, 'i:', d, '6', 'S', r, 'I', f, t]).
entry(niedrigere, [n, 'i:', d, r, 'I', g, @, r, @]).
entry(niedrigeren, [n, 'i:', d, r, 'I', g, @, r, @, n]).
entry(niedriger, [n, 'i:', d, r, 'I', g, '6']).
entry(niedrig, [n, 'i:', d, r, 'I', 'C']).
entry('Niehmeyer', [n, 'i:', m, aI, '6']).
entry('Nieh', [n, 'i:']).
entry('Niela', [n, 'i:', l, a]).
entry(niemanden, [n, 'i:', m, a, n, d, @, n]).
entry(niemand, [n, 'i:', m, a, n, t]).
entry('Nikolaus', [n, 'I', k, o, l, aU, s]).
entry('Nikolaus-Treffen', [n, 'I', k, o, l, aU, s, t, r, 'E', f, @, n]).
entry(nimmt, [n, 'I', m, t]).
entry(ni, [n, 'I']).
entry(nirgendwo, [n, 'I', '6', g, @, n, t, v, 'o:']).
entry('Nishimoto', [n, 'I', 'S', i, m, 'o:', t, o]).
entry(nix, [n, 'I', k, s]).
entry(nja, [n, j, a]).
entry(n, [n]).
entry(nobel, [n, 'o:', b, @, l]).
entry(nobleren, [n, 'o:', b, l, @, r, @, n]).
entry(nobles, [n, 'o:', b, l, @, s]).
entry(nochmaligen, [n, 'O', x, m, 'a:', l, 'I', g, @, n]).
entry(nochmals, [n, 'O', x, m, 'a:', l, s]).
entry(noch, [n, 'O', x]).
entry('Nöhre', [n, '2:', r, @]).
entry(nö, [n, '2:']).
entry('Nonnenstraße', [n, 'O', n, @, n, 'S', t, r, 'a:', s, @]).
entry(no, [n, 'O']).
entry('No', [n, o]).
entry('Norbert', [n, 'O', '6', b, 'E', '6', t]).
entry('Norddeutschen', [n, 'O', '6', t, d, 'OY', tS, @, n]).
entry('Norden', [n, 'O', '6', d, @, n]).
entry(nördlicher, [n, '9', '6', t, l, 'I', 'C', '6']).
entry('Nordpol', [n, 'O', '6', t, p, 'o:', l]).
entry(normalen, [n, 'O', '6', m, 'a:', l, @, n]).
entry(normale, [n, 'O', '6', m, 'a:', l, @]).
entry(normalerweise, [n, 'O', '6', m, 'a:', l, '6', v, aI, z, @]).
entry(normales, [n, 'O', '6', m, 'a:', l, @, s]).
entry(normal, [n, 'O', '6', m, 'a:', l]).
entry('Norman', [n, 'O', '6', m, @, n]).
entry('Nöten', [n, '2:', t, @, n]).
entry('Notfall', [n, 'o:', t, f, a, l]).
entry(notfalls, [n, 'o:', t, f, a, l, s]).
entry(notieren, [n, o, t, 'i:', r, @, n]).
entry(notiere, [n, o, t, 'i:', r, @]).
entry(notiert, [n, o, t, 'i:', '6', t]).
entry(nötige, [n, '2:', t, 'I', g, @]).
entry(nötigen, [n, '2:', t, 'I', g, @, n]).
entry(nötig, [n, '2:', t, 'I', 'C']).
entry(nötigsten, [n, '2:', t, 'I', 'C', s, t, @, n]).
entry('Noti', [n, o, t, 'i:']).
entry('Notizbuch', [n, o, t, 'i:', ts, b, 'u:', x]).
entry('Notizen', [n, o, t, 'i:', ts, @, n]).
entry('Notiz', [n, o, t, 'i:', ts]).
entry('Notlösung', [n, 'o:', t, l, '2:', z, 'U', 'N']).
entry(not, [n, 'o:', t]).
entry(notorisch, [n, o, t, 'o:', r, 'I', 'S']).
entry('Notwendigkeit', [n, 'o:', t, v, 'E', n, d, 'I', 'C', k, aI, t]).
entry(notwendig, [n, 'o:', t, v, 'E', n, d, 'I', 'C']).
entry('Novemberhälfte', [n, 'o:', v, 'E', m, b, '6', h, 'E', l, f, t, @]).
entry('November', [n, 'o:', v, 'E', m, b, '6']).
entry('Novembers', [n, 'o:', v, 'E', m, b, '6', s]).
entry('Novemberwoche', [n, 'o:', v, 'E', m, b, '6', v, 'O', x, @]).
entry('Novemb', [n, 'o:', v, 'E', m, p]).
entry('Novem', [n, 'o:', v, 'E', m]).
entry(ns, [n, s]).
entry(nstag, [n, s, t, 'a:', k]).
entry(nten, [n, t, @, n]).
entry(nter, [n, t, '6']).
entry('Nudelgerichte', [n, 'u:', d, @, l, g, @, r, 'I', 'C', t, @]).
entry(null, [n, 'U', l]).
entry(numeriert, [n, 'U', m, @, r, 'i:', '6', t]).
entry('Nummern', [n, 'U', m, '6', n]).
entry('Nummer', [n, 'U', m, '6']).
entry(nung, [n, 'U', 'N']).
entry(nun, [n, 'u:', n]).
entry('Nürnberg', [n, 'Y', '6', n, b, 'E', '6', k]).
entry(nur, [n, 'u:', '6']).
entry(nutzen, [n, 'U', ts, @, n]).
entry(nützen, [n, 'Y', ts, @, n]).
entry('Nützlichen', [n, 'Y', ts, l, 'I', 'C', @, n]).
entry(nützlich, [n, 'Y', ts, l, 'I', 'C']).
entry(nützt, [n, 'Y', ts, t]).
entry(obenhin, ['o:', b, @, n, h, 'I', n]).
entry(oben, ['o:', b, @, n]).
entry(oberen, ['o:', b, @, r, @, n]).
entry(obere, ['o:', b, @, r, @]).
entry('Oberföhringer-Straße', ['o:', b, '6', f, '2:', r, 'I', 'N', '6', 'S', t, r, 'a:', s, @]).
entry('Oberressel', ['o:', b, '6', r, 'E', s, @, l]).
entry(oberster, ['o:', b, '6', 'S', t, '6']).
entry('Objekte', ['O', p, j, 'E', k, t, @]).
entry('Oborski', [o, b, 'O', '6', s, k, i]).
entry(ob, ['O', p]).
entry('Obrowski', [o, b, r, 'O', f, s, k, i]).
entry(observiert, ['O', p, z, 'E', '6', v, 'i:', '6', t]).
entry('Obst', ['o:', p, s, t]).
entry(obwohl, ['O', p, v, 'o:', l]).
entry(ochenende, ['O', x, @, n, 'E', n, d, @]).
entry(och, ['O', x]).
entry(ode, ['o:', d, @]).
entry(oder, ['o:', d, '6']).
entry('Ödipus', ['2:', d, i, p, 'U', s]).
entry(od, ['o:', t]).
entry('Oertel', ['9', '6', t, @, l]).
entry('Offenbach', ['O', f, @, n, b, a, x]).
entry(offenbar, ['O', f, @, n, b, 'a:', r]).
entry('Offenburg', ['O', f, @, n, b, 'U', '6', k]).
entry(offenen, ['O', f, @, n, @, n]).
entry(offenes, ['O', f, @, n, @, s]).
entry(offenhalten, ['O', f, @, n, h, a, l, t, @, n]).
entry(offenlassen, ['O', f, @, n, l, a, s, @, n]).
entry(offen, ['O', f, @, n]).
entry(offensichtlich, ['O', f, @, n, z, 'I', 'C', t, l, 'I', 'C']).
entry(öffentlichen, ['9', f, @, n, t, l, 'I', 'C', @, n]).
entry(öffentliche, ['9', f, @, n, t, l, 'I', 'C', @]).
entry('Offergeld', ['O', f, '6', g, 'E', l, t]).
entry(office, ['O', f, 'I', s]).
entry(öffnet, ['9', f, n, @, t]).
entry('Öffnungszeiten', ['9', f, n, 'U', 'N', s, ts, aI, t, @, n]).
entry('Öffnungszeit', ['9', f, n, 'U', 'N', s, ts, aI, t]).
entry('of-Phonetics', ['O', f, f, o, n, 'E', t, 'I', k, s]).
entry('Öf', ['9', f]).
entry(öfteren, ['9', f, t, @, r, @, n]).
entry(öfter, ['9', f, t, '6']).
entry(öfters, ['9', f, t, '6', s]).
entry(oftmals, ['O', f, t, m, 'a:', l, s]).
entry(oft, ['O', f, t]).
entry('Ögai', ['2:', g, aI]).
entry(ohnehin, ['o:', n, @, h, 'I', n]).
entry(ohne, ['o:', n, @]).
entry(ohohoho, [o, h, o, h, o, h, o]).
entry(öh, ['2:']).
entry(oh, ['o:']).
entry('Ohren', ['o:', r, @, n]).
entry('Ohres', ['o:', r, @, s]).
entry('Ohr', ['o:', '6']).
entry(oi, ['O', 'i:']).
entry(oje, [o, j, 'e:']).
entry(oka, [o, k, @]).
entry(okay, [o, k, 'e:']).
entry(ökologischem, ['2', k, o, l, 'o:', g, 'I', 'S', @, m]).
entry(ökologischer, ['2', k, o, l, 'o:', g, 'I', 'S', '6']).
entry(ökologisch, ['2', k, o, l, 'o:', g, 'I', 'S']).
entry(ökonomisch, ['2', k, o, n, 'o:', m, 'I', 'S']).
entry('Okpue', ['O', k, p, 'u:', @]).
entry(ok, [o, k]).
entry('Ok', ['O', k]).
entry('Oktobe', ['O', k, t, 'o:', b, @]).
entry('Oktoberhälfte', ['O', k, t, 'o:', b, '6', h, 'E', l, f, t, @]).
entry('Oktober', ['O', k, t, 'o:', b, '6']).
entry('Oktobers', ['O', k, t, 'o:', b, '6', s]).
entry('Oktobertermine', ['O', k, t, 'o:', b, '6', t, 'E', '6', m, 'i:', n, @]).
entry('Oktoberwochen', ['O', k, t, 'o:', b, '6', v, 'O', x, @, n]).
entry('Oktoberwoche', ['O', k, t, 'o:', b, '6', v, 'O', x, @]).
entry('Oldenburg', ['O', l, d, @, n, b, 'U', '6', k]).
entry(old, ['O', l, t]).
entry(ole, [o, l, 'e:']).
entry('Oli', ['O', l, i]).
entry('Oliver', ['O', l, i, v, '6']).
entry('Öl', ['2:', l]).
entry('Olympiadorf', [o, l, 'Y', m, p, j, a, d, 'O', '6', f]).
entry(om, ['O', m]).
entry(onegai, ['o:', n, 'e:', g, aI]).
entry(one, [v, 'O', n]).
entry('Ootawara', ['O', t, a, v, 'a:', r, a]).
entry(open, ['o:', p, @, n]).
entry(operativ, [o, p, @, r, a, t, 'i:', f]).
entry('Operette', [o, p, @, r, 'E', t, @]).
entry('Opernbesuche', ['o:', p, '6', n, b, @, z, u, x, @]).
entry('Opernbesuch', ['o:', p, '6', n, b, @, z, 'u:', x]).
entry('Opernkarten', ['o:', p, '6', n, k, a, r, t, @, n]).
entry('Oper', ['o:', p, '6']).
entry(opfern, ['O', pf, '6', n]).
entry(opfert, ['O', pf, '6', t]).
entry('Oppenrieder', ['O', p, @, n, r, 'i:', d, '6']).
entry('Oppermann', ['O', p, '6', m, a, n]).
entry('Op', ['o:', p]).
entry(optimale, ['O', p, t, i, m, 'a:', l, @]).
entry(optimal, ['O', p, t, i, m, 'a:', l]).
entry('Option', ['O', p, ts, j, 'o:', n]).
entry(o, [o]).
entry('Orchester', ['O', '6', k, 'E', s, t, '6']).
entry(ordentlich, ['O', '6', d, @, n, t, l, 'I', 'C']).
entry(ordern, ['O', '6', d, '6', n]).
entry('Ordnung', ['O', '6', d, n, 'U', 'N']).
entry('Ordnu', ['O', '6', d, n, 'U']).
entry(ören, ['2:', r, @, n]).
entry('Organi', ['O', '6', g, a, n, i]).
entry('Organisation', ['O', '6', g, a, n, i, z, a, ts, j, 'o:', n]).
entry('Organisationstalent', ['O', '6', g, a, n, i, z, a, ts, j, 'o:', n, s, t, a, l, 'E', n, t]).
entry(organisatorischen, ['O', '6', g, a, n, i, z, a, t, 'o:', r, 'I', 'S', @, n]).
entry(organisieren, ['O', '6', g, a, n, i, z, 'i:', r, @, n]).
entry(organisiere, ['O', '6', g, a, n, i, z, 'i:', r, @]).
entry(organisiert, ['O', '6', g, a, n, i, z, 'i:', '6', t]).
entry(orientieren, [o, '6', j, 'E', n, t, 'i:', r, @, n]).
entry(orientiere, [o, '6', j, 'E', n, t, 'i:', r, @]).
entry(original, [o, r, i, g, i, n, 'a:', l]).
entry(orlesung, ['o:', '6', l, 'e:', z, 'U', 'N']).
entry(orragend, ['o:', '6', r, 'a:', g, @, n, t]).
entry(orschlagen, ['o:', '6', 'S', l, 'a:', g, @, n]).
entry('Örtlichkeit', ['9', '6', t, l, 'I', 'C', k, aI, t]).
entry('Ortmeier', ['O', '6', t, m, aI, '6']).
entry(ort, ['O', '6', t]).
entry('Ortsbesichtigung', ['O', '6', ts, b, @, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry('Ortskern', ['O', '6', ts, k, 'E', '6', n]).
entry(ortskundig, ['O', '6', ts, k, 'U', n, d, 'I', 'C']).
entry('Osnabrück', ['O', s, n, a, b, r, 'Y', k]).
entry('Osna', ['O', s, n, a]).
entry('Osten', ['O', s, t, @, n]).
entry('Osterdienstag', ['o:', s, t, '6', d, 'i:', n, s, t, 'a:', k]).
entry('Osterfeiertagen', ['o:', s, t, '6', f, aI, '6', t, 'a:', g, @, n]).
entry('Osterfeiertage', ['o:', s, t, '6', f, aI, '6', t, 'a:', g, @]).
entry('Osterferien', ['o:', s, t, '6', f, 'e:', '6', j, @, n]).
entry('Ostermann', ['o:', s, t, '6', m, a, n]).
entry('Ostermontag', ['o:', s, t, '6', m, 'o:', n, t, 'a:', k]).
entry('Ostern', ['o:', s, t, '6', n]).
entry('Oster', ['o:', s, t, '6']).
entry('Ostersamstag', ['o:', s, t, '6', z, a, m, s, t, 'a:', k]).
entry('Ostersonntag', ['o:', s, t, '6', z, 'O', n, t, 'a:', k]).
entry('Ostertagen', ['o:', s, t, '6', t, 'a:', g, @, n]).
entry('Ostertage', ['o:', s, t, '6', t, 'a:', g, @]).
entry('Osterwochenende', ['o:', s, t, '6', v, 'O', x, @, n, 'E', n, d, @]).
entry('Osterwochen', ['o:', s, t, '6', v, 'O', x, @, n]).
entry('Osterwoche', ['o:', s, t, '6', v, 'O', x, @]).
entry('Ostseestraße', ['O', s, t, z, 'e:', 'S', t, r, 'a:', s, @]).
entry('Otto', ['O', t, o]).
entry(over, ['o:', v, '6']).
entry(paar, [p, a, r]).
entry('Päckchen', [p, 'E', k, 'C', @, n]).
entry(packen, [p, a, k, @, n]).
entry(packe, [p, a, k, @]).
entry(packt, [p, a, k, t]).
entry('Paderborn', [p, 'a:', d, '6', b, 'O', '6', n]).
entry('Pahl', [p, 'a:', l]).
entry('Paket', [p, a, k, 'e:', t]).
entry(paletti, [p, a, l, 'E', t, i]).
entry('Panasonic', [p, a, n, a, z, 'o:', n, 'I', k]).
entry('Panik', [p, 'a:', n, 'I', k]).
entry(pa, [p, a]).
entry('Pa', [p, 'a:']).
entry(pä, [p, 'E:']).
entry('Papier', [p, a, p, 'i:', '6']).
entry(parat, [p, a, r, 'a:', t]).
entry('Pardon', [p, a, r, d, 'O', 'N']).
entry('Paris', [p, a, r, 'i:', s]).
entry('Parkanlage', [p, a, r, k, a, n, l, 'a:', g, @]).
entry(parken, [p, a, r, k, @]).
entry('Parkgebühren', [p, a, r, k, g, @, b, 'y:', r, @, n]).
entry('Parkhotel', [p, a, r, k, h, o, t, 'E', l]).
entry('Parkmöglichkeiten', [p, a, r, k, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Parkmöglichkeit', [p, a, r, k, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Park', [p, a, r, k]).
entry('Parkplatz', [p, a, r, k, p, l, a, ts]).
entry('Parkplatzsuche', [p, a, r, k, p, l, a, ts, s, 'U', x, @]).
entry('Parsch', [p, a, r, 'S']).
entry('Parteien', [p, a, r, t, aI, @, n]).
entry('Parterre', [p, a, r, t, 'E', r, @]).
entry('Partner', [p, a, r, t, n, '6']).
entry('Partu', [p, a, r, t, u]).
entry(paß, [p, a, s]).
entry(passabel, [p, a, s, 'a:', b, @, l]).
entry(passables, [p, a, s, 'a:', b, l, @, s]).
entry('Passau', [p, a, s, aU]).
entry(passenden, [p, a, s, @, n, d, @, n]).
entry(passende, [p, a, s, @, n, d, @]).
entry('Passenderes', [p, a, s, @, n, d, @, r, @, s]).
entry(passender, [p, a, s, @, n, d, '6']).
entry(passendes, [p, a, s, @, n, d, @, s]).
entry(passend, [p, a, s, @, n, t]).
entry(passen, [p, a, s, @, n]).
entry(passe, [p, a, s, @]).
entry(passieren, [p, a, s, 'i:', r, @, n]).
entry(passiert, [p, a, s, 'i:', '6', t]).
entry(paßten, [p, a, s, t, @, n]).
entry(paßte, [p, a, s, t, @]).
entry(paßt, [p, a, s, t]).
entry('Pätsch', [p, 'E', tS]).
entry('Pätzold', [p, 'E', ts, 'O', l, t]).
entry('Paula', [p, aU, l, a]).
entry('Paul-Klee', [p, aU, l, k, l, 'e:']).
entry('Paul', [p, aU, l]).
entry('Paulsen', [p, aU, l, z, @, n]).
entry('Paulson', [p, aU, l, s, 'O', n]).
entry(pause, [p, aU, z, @]).
entry('Payome', [p, a, j, 'o:', m, @]).
entry('Pech', [p, 'E', 'C']).
entry('Peikert', [p, aI, k, '6', t]).
entry(peilen, [p, aI, l, @, n]).
entry('Peimann', [p, aI, m, a, n]).
entry('Pei', [p, aI]).
entry('Pelikan', [p, 'e:', l, 'I', k, 'a:', n]).
entry(pendeln, [p, 'E', n, d, @, l, n]).
entry('Pendlern', [p, 'E', n, d, l, '6', n]).
entry('Pension', [p, 'E', n, z, j, 'o:', n]).
entry(pe, [p, e]).
entry('Perfekte', [p, 'E', '6', f, 'E', k, t, @]).
entry(perfekt, [p, 'E', '6', f, 'E', k, t]).
entry('Periode', [p, e, '6', j, 'o:', d, @]).
entry(permanent, [p, 'E', '6', m, a, n, 'E', n, t]).
entry(per, [p, 'E', '6']).
entry('Personalien', [p, 'E', '6', z, o, n, 'a:', l, j, @, n]).
entry('Personal', [p, 'E', '6', z, o, n, 'a:', l]).
entry('Personalplanung', [p, 'E', '6', z, o, n, 'a:', l, p, l, 'a:', n, 'U', 'N']).
entry('Personalratssitzung', [p, 'E', '6', z, o, n, 'a:', l, r, 'a:', ts, z, 'I', ts, 'U', 'N']).
entry('Personen', [p, 'E', '6', z, 'o:', n, @, n]).
entry(persönlichen, [p, 'E', '6', z, '2:', n, l, 'I', 'C', @, n]).
entry(persönliche, [p, 'E', '6', z, '2:', n, l, 'I', 'C', @]).
entry(persönlich, [p, 'E', '6', z, '2:', n, l, 'I', 'C']).
entry('Person', [p, 'E', '6', z, 'o:', n]).
entry(pessimistisch, [p, 'E', s, i, m, 'I', s, t, 'I', 'S']).
entry('Petermann', [p, 'e:', t, '6', m, a, n]).
entry('Peter', [p, 'e:', t, '6']).
entry('Petersberg', [p, 'e:', t, '6', s, b, 'E', '6', k]).
entry('Peters', [p, 'i:', t, @, r, z]).
entry('Petz', [p, 'E', ts]).
entry('Pfaff', [pf, a, f]).
entry('Pfa', [pf, a]).
entry('Pferd', [pf, 'e:', '6', t]).
entry('Pfienstag', [pf, 'i:', n, s, t, 'a:', k]).
entry(pfierten, [pf, 'i:', '6', t, @, n]).
entry('Pfingstdienstag', [pf, 'I', 'N', s, t, d, 'i:', n, s, t, 'a:', k]).
entry('Pfingsten', [pf, 'I', 'N', s, t, @, n]).
entry('Pfingstfeiertagen', [pf, 'I', 'N', s, t, f, aI, '6', t, 'a:', g, @, n]).
entry('Pfingstfeiertage', [pf, 'I', 'N', s, t, f, aI, '6', t, 'a:', g, @]).
entry('Pfingstferien', [pf, 'I', 'N', s, t, f, 'e:', '6', j, @, n]).
entry('Pfingstmontag', [pf, 'I', 'N', s, t, m, 'o:', n, t, 'a:', k]).
entry('Pfingstsonntag', [pf, 'I', 'N', s, t, z, 'O', n, t, 'a:', k]).
entry('Pfingsttage', [pf, 'I', 'N', s, t, t, 'a:', g, @]).
entry('Pfingstwochenende', [pf, 'I', 'N', s, t, v, 'O', x, @, n, 'E', n, d, @]).
entry('Pfingstwoche', [pf, 'I', 'N', s, t, v, 'O', x, @]).
entry('Pfingstzeit', [pf, 'I', 'N', s, t, ts, aI, t]).
entry('Pfin', [pf, 'I', n]).
entry('Pfitzinger', [pf, 'I', ts, 'I', 'N', '6']).
entry(pflichtungen, [pf, l, 'I', 'C', t, 'U', 'N', @, n]).
entry('Pforte', [pf, 'O', '6', t, @]).
entry('Pforzheim', [pf, 'O', '6', ts, h, aI, m]).
entry(pf, [pf]).
entry(pfüa, [pf, 'Y', a]).
entry('Pfunde', [pf, 'U', n, d, @]).
entry(pfü, [pf, y]).
entry(phänomenal, [f, 'E', n, o, m, e, n, 'a:', l]).
entry(phantastische, [f, a, n, t, a, s, t, 'I', 'S', @]).
entry(phantastisch, [f, a, n, t, a, s, t, 'I', 'S']).
entry('Phase', [f, 'a:', z, @]).
entry('Philharmonie', [f, 'I', l, h, a, r, m, o, n, 'i:']).
entry(phony, [f, 'o:', n, i]).
entry('Photographie', [f, o, t, o, g, r, a, f, 'i:']).
entry('Pianobar', [p, i, 'a:', n, o, b, 'a:', r]).
entry('Piano-Spieler', [p, i, 'a:', n, o, 'S', p, 'i:', l, '6']).
entry('Picasso-Ausstellung', [p, i, k, a, s, o, aU, s, 'S', t, 'E', l, 'U', 'N']).
entry(piell, [p, j, 'E', l]).
entry(pikiert, [p, i, k, 'i:', '6', t]).
entry('Pilgersheimer-Straße', [p, 'I', l, g, '6', s, h, aI, m, '6', 'S', t, r, 'a:', s, @]).
entry('Pillau', [p, 'I', l, aU]).
entry('Pillmann', [p, 'I', l, m, a, n]).
entry('Pilot', [p, i, l, 'o:', t]).
entry('Piske', [p, 'I', s, k, @]).
entry('Piste', [p, 'I', s, t, @]).
entry(plädieren, [p, l, 'E', d, 'i:', r, @, n]).
entry(plädiere, [p, l, 'E', d, 'i:', r, @]).
entry(plagt, [p, l, 'a:', k, t]).
entry(planen, [p, l, 'a:', n, @, n]).
entry('Plänen', [p, l, 'E:', n, @, n]).
entry(plane, [p, l, 'a:', n, @]).
entry('Pläne', [p, l, 'E:', n, @]).
entry('Planer', [p, l, 'a:', n, '6']).
entry(plan, [p, l, 'a:', n]).
entry(plant, [p, l, 'a:', n, t]).
entry('Planungen', [p, l, 'a:', n, 'U', 'N', @, n]).
entry('Planung', [p, l, 'a:', n, 'U', 'N']).
entry('Planungsterminen', [p, l, 'a:', n, 'U', 'N', s, t, 'E', '6', m, 'i:', n, @, n]).
entry('Planu', [p, l, 'a:', n, 'U']).
entry(pla, [p, l, a]).
entry('Plath', [p, l, 'a:', t]).
entry('Plattform', [p, l, a, t, f, 'O', '6', m]).
entry('Plätzchen', [p, l, 'E', ts, 'C', @, n]).
entry('Plätze', [p, l, 'E', ts, @]).
entry('Platzkarten', [p, l, a, ts, k, a, r, t, @, n]).
entry('Platz', [p, l, a, ts]).
entry('Platzreservierungen', [p, l, a, ts, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', @, n]).
entry('Platzreservierung', [p, l, a, ts, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N']).
entry('Plecas', [p, l, 'E', k, 'a:', s]).
entry('Plex', [p, l, 'E', k, s]).
entry('Ploss', [p, l, 'O', s]).
entry(plötzlich, [p, l, '9', ts, l, 'I', 'C']).
entry(pl, [p, l]).
entry(plus, [p, l, 'U', s]).
entry('Pohl', [p, 'o:', l]).
entry(point, [p, 'OY', n, t]).
entry('Polenz', [p, 'o:', l, 'E', n, ts]).
entry('Poloklub', [p, 'o:', l, o, k, l, 'U', p]).
entry('Pommer', [p, 'O', m, '6']).
entry('Pool', [p, 'u:', l]).
entry('Porsche', [p, 'O', '6', 'S', @]).
entry(portable, [p, 'O', '6', t, 'a:', b, l, @]).
entry('Portemonnaie', [p, o, '6', t, m, 'O', n, 'e:']).
entry('Portier', [p, 'O', '6', t, j, 'e:']).
entry('Position', [p, o, z, i, ts, j, 'o:', n]).
entry('Positive', [p, 'o:', z, i, t, 'i:', v, @]).
entry(positiv, [p, 'o:', z, i, t, 'i:', f]).
entry('Poster', [p, 'o:', s, t, '6']).
entry(pößt, [p, '9', s, t]).
entry('Post', [p, 'O', s, t]).
entry(potenziert, [p, o, t, 'E', n, ts, 'i:', '6', t]).
entry('Potsdam', [p, 'O', ts, d, a, m]).
entry('Potte', [p, 'O', t, @]).
entry(p, [p]).
entry('P', [p, 'e:']).
entry(prächtig, [p, r, 'E', 'C', t, 'I', 'C']).
entry('Präferenzen', [p, r, 'E', f, e, r, 'E', n, ts, @, n]).
entry('Präferenz', [p, r, 'E', f, e, r, 'E', n, ts]).
entry(präferieren, [p, r, 'E', f, e, r, 'i:', r, @, n]).
entry(präferierten, [p, r, 'E', f, e, r, 'i:', '6', t, @, n]).
entry(praktikable, [p, r, a, k, t, i, k, 'a:', b, l, @]).
entry('Praktikantin', [p, r, a, k, t, i, k, a, n, t, 'I', n]).
entry('Praktikum', [p, r, a, k, t, i, k, 'U', m]).
entry(prakti, [p, r, a, k, t, 'I']).
entry(praktischer, [p, r, a, k, t, 'I', 'S', '6']).
entry(praktisch, [p, r, a, k, t, 'I', 'S']).
entry(prakt, [p, r, a, k, t]).
entry('Präsentation', [p, r, 'E', z, 'E', n, t, a, ts, j, 'o:', n]).
entry(präsentieren, [p, r, 'E', z, 'E', n, t, 'i:', r, @, n]).
entry(präsent, [p, r, 'E', z, 'E', n, t]).
entry(predigen, [p, r, 'e:', d, 'I', g, @, n]).
entry('Preisbereich', [p, r, aI, s, b, @, r, aI, 'C']).
entry('Preisdifferenz', [p, r, aI, s, d, 'I', f, @, r, 'E', n, ts]).
entry('Preisen', [p, r, aI, z, @, n]).
entry('Preise', [p, r, aI, z, @]).
entry('Preisfrage', [p, r, aI, s, f, r, 'a:', g, @]).
entry(preisgünstigeres, [p, r, aI, s, g, 'Y', n, s, t, 'I', g, @, r, @, s]).
entry(preisgünstiger, [p, r, aI, s, g, 'Y', n, s, t, 'I', g, '6']).
entry(preisgünstiges, [p, r, aI, s, g, 'Y', n, s, t, 'I', g, @, s]).
entry(preisgünstig, [p, r, aI, s, g, 'Y', n, s, t, 'I', 'C']).
entry('Preiskategorien', [p, r, aI, s, k, a, t, e, g, o, r, 'i:', @, n]).
entry('Preiskategorie', [p, r, aI, s, k, a, t, e, g, o, r, 'i:']).
entry('Preisklassen', [p, r, aI, s, k, l, a, s, @, n]).
entry('Preisklasse', [p, r, aI, s, k, l, a, s, @]).
entry('Preiskra', [p, r, aI, s, k, r, a]).
entry('Preislagen', [p, r, aI, s, l, 'a:', g, @, n]).
entry('Preislage', [p, r, aI, s, l, 'a:', g, @]).
entry('Preislichen', [p, r, aI, s, l, 'I', 'C', @, n]).
entry(preislich, [p, r, aI, s, l, 'I', 'C']).
entry(preismäßig, [p, r, aI, s, m, 'E:', s, 'I', 'C']).
entry('Preis', [p, r, aI, s]).
entry('Preisunterschiede', [p, r, aI, s, 'U', n, t, '6', 'S', 'i:', d, @]).
entry('Preisunterschied', [p, r, aI, s, 'U', n, t, '6', 'S', 'i:', t]).
entry('Preisverhältnisse', [p, r, aI, s, f, 'E', '6', h, 'E', l, t, n, 'I', s, @]).
entry('Preisvorstellungen', [p, r, aI, s, f, 'o:', '6', 'S', t, 'E', l, 'U', 'N', @, n]).
entry('Preisvorstellung', [p, r, aI, s, f, 'o:', '6', 'S', t, 'E', l, 'U', 'N']).
entry(preiswerte, [p, r, aI, s, v, 'e:', '6', t, @]).
entry(preiswerteren, [p, r, aI, s, v, 'e:', '6', t, @, r, @, n]).
entry(preiswerter, [p, r, aI, s, v, 'e:', '6', t, '6']).
entry(preiswertes, [p, r, aI, s, v, 'e:', '6', t, @, s]).
entry(preiswertesten, [p, r, aI, s, v, 'e:', '6', t, @, s, t, @, n]).
entry(preiswerteste, [p, r, aI, s, v, 'e:', '6', t, @, s, t, @]).
entry(preiswert, [p, r, aI, s, v, 'e:', '6', t]).
entry('Premiere', [p, r, @, m, j, 'e:', r, @]).
entry(pre, [p, r, e]).
entry('Pressekonferenz', [p, r, 'E', s, @, k, 'O', n, f, e, r, 'E', n, ts]).
entry('Presse', [p, r, 'E', s, @]).
entry('Pressetermin', [p, r, 'E', s, @, t, 'E', '6', m, 'i:', n]).
entry(pressiert, [p, r, 'E', s, 'i:', '6', t]).
entry('Preußen', [p, r, 'OY', s, @, n]).
entry('Priewe', [p, r, 'i:', v, @]).
entry(pril, [p, r, 'I', l]).
entry(prima, [p, r, 'i:', m, a]).
entry(prim, [p, r, 'I', m]).
entry('Prin', [p, r, 'I', n]).
entry('Prinzenhof-Hotel', [p, r, 'I', n, ts, @, n, h, 'o:', f, h, o, t, 'E', l]).
entry('Prinzenhof', [p, r, 'I', n, ts, @, n, h, 'o:', f]).
entry('Prinzen-Hotel', [p, r, 'I', n, ts, @, n, h, o, t, 'E', l]).
entry('Prinzen', [p, r, 'I', n, ts, @, n]).
entry('Prinzhotel', [p, r, 'I', n, ts, h, o, t, 'E', l]).
entry(prinzipielle, [p, r, 'I', n, ts, i, p, j, 'E', l, @]).
entry(prinzipiell, [p, r, 'I', n, ts, i, p, j, 'E', l]).
entry(prinzipie, [p, r, 'I', n, ts, i, p, j, 'E']).
entry('Prinzip', [p, r, 'I', n, ts, 'i:', p]).
entry(prinzi, [p, r, 'I', n, ts, i]).
entry('Prinz', [p, r, 'I', n, ts]).
entry('Priorität', [p, r, i, o, r, i, t, 'E:', t]).
entry(pri, [p, r, i]).
entry(priva, [p, r, i, v, a]).
entry('Privatauto', [p, r, i, v, 'a:', t, aU, t, o]).
entry('Privatem', [p, r, i, v, 'a:', t, @, m]).
entry(privaten, [p, r, i, v, 'a:', t, @, n]).
entry(privater, [p, r, i, v, 'a:', t, '6']).
entry('Privatnummer', [p, r, i, v, 'a:', t, n, 'U', m, '6']).
entry(privat, [p, r, i, v, 'a:', t]).
entry('Privatsache', [p, r, i, v, 'a:', t, z, a, x, @]).
entry('Privatsphäre', [p, r, i, v, 'a:', ts, f, 'E:', r, @]).
entry('Privattutorium', [p, r, i, v, 'a:', t, t, u, t, 'o:', '6', j, 'U', m]).
entry(probieren, [p, r, o, b, 'i:', r, @, n]).
entry(probiere, [p, r, o, b, 'i:', r, @]).
entry(probier, [p, r, o, b, 'i:', '6']).
entry(probiert, [p, r, o, b, 'i:', '6', t]).
entry(problematischer, [p, r, o, b, l, e, m, 'a:', t, 'I', 'S', '6']).
entry(problematisch, [p, r, o, b, l, e, m, 'a:', t, 'I', 'S']).
entry('Problemchen', [p, r, o, b, l, 'e:', m, 'C', @, n]).
entry('Probleme', [p, r, o, b, l, 'e:', m, @]).
entry(problemlos, [p, r, o, b, l, 'e:', m, l, 'o:', s]).
entry('Problem', [p, r, o, b, l, 'e:', m]).
entry('Prodöhl', [p, r, o, d, '2:', l]).
entry('Produktes', [p, r, o, d, 'U', k, t, @, s]).
entry('Produktion', [p, r, o, d, 'U', k, ts, j, 'o:', n]).
entry(produktiv, [p, r, o, d, 'U', k, t, 'i:', f]).
entry('Produktpräsentation', [p, r, o, d, 'U', k, t, p, r, 'E', z, 'E', n, t, a, ts, j, 'o:', n]).
entry('Produkt', [p, r, o, d, 'U', k, t]).
entry('Professor', [p, r, o, f, 'E', s, 'o:', '6']).
entry(prof, [p, r, 'O', f]).
entry('Programme', [p, r, o, g, r, a, m, @]).
entry(programmieren, [p, r, o, g, r, a, m, 'i:', r, @, n]).
entry(programm, [p, r, o, g, r, a, m]).
entry('Programmpunkt', [p, r, o, g, r, a, m, p, 'U', 'N', k, t]).
entry('Progra', [p, r, o, g, r, a]).
entry('Projektabschluß', [p, r, o, j, 'E', k, t, a, p, 'S', l, 'U', s]).
entry('Projektbesprechung', [p, r, o, j, 'E', k, t, b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Projekten', [p, r, o, j, 'E', k, t, @, n]).
entry('Projekte', [p, r, o, j, 'E', k, t, @]).
entry('Projektleiter', [p, r, o, j, 'E', k, t, l, aI, t, '6']).
entry('Projekt', [p, r, o, j, 'E', k, t]).
entry('Projekts', [p, r, o, j, 'E', k, ts]).
entry(prophylaktisch, [p, r, o, f, y, l, a, k, t, 'I', 'S']).
entry(proppenvoll, [p, r, 'O', p, @, n, f, 'O', l]).
entry(pro, [p, r, 'o:']).
entry('Prosi', [p, r, 'o:', z, i]).
entry(prospekte, [p, r, o, s, p, 'E', k, t, @]).
entry('Prospektmaterial', [p, r, o, s, p, 'E', k, t, m, a, t, e, '6', j, 'a:', l]).
entry('Prospekt', [p, r, o, s, p, 'E', k, t]).
entry(protestantischer, [p, r, o, t, 'E', s, t, a, n, t, 'I', 'S', '6']).
entry('Prozedur', [p, r, o, ts, e, d, 'u:', '6']).
entry('Prozent', [p, r, o, ts, 'E', n, t]).
entry(pr, [p, r]).
entry(prüfen, [p, r, 'y:', f, @, n]).
entry('Prüfung', [p, r, 'y:', f, 'U', 'N']).
entry(prügeln, [p, r, 'y:', g, @, l, n]).
entry('Puchta', [p, 'U', x, t, a]).
entry('Puffer', [p, 'U', f, '6']).
entry('Pufferzeit', [p, 'U', f, '6', ts, aI, t]).
entry(puh, [p, 'u:']).
entry('Pulsnitz', [p, 'U', l, s, n, 'I', ts]).
entry('Punkte', [p, 'U', 'N', k, t, @]).
entry(pünktlich, [p, 'Y', 'N', k, t, l, 'I', 'C']).
entry('Punkt', [p, 'U', 'N', k, t]).
entry('Pusch', [p, 'U', 'S']).
entry('Putis', [p, u, t, 'I', s]).
entry('Putz', [p, 'U', ts]).
entry('Q', [k, 'u:']).
entry('Qualität', [k, v, a, l, i, t, 'E:', t]).
entry('Quartalen', [k, v, a, r, t, 'a:', l, @, n]).
entry('Quartales', [k, v, a, r, t, 'a:', l, @, s]).
entry('Quartal', [k, v, a, r, t, 'a:', l]).
entry('Quartals', [k, v, a, r, t, 'a:', l, s]).
entry('Quartaltreffen', [k, v, a, r, t, 'a:', l, t, r, 'E', f, @, n]).
entry('Quarti', [k, v, a, r, t, 'i:']).
entry('Quart', [k, v, a, r, t]).
entry(quasi, [k, v, 'a:', z, i]).
entry(quatschen, [k, v, 'a:', tS, @, n]).
entry('Quatsch', [k, v, a, tS]).
entry('Quelle', [k, v, 'E', l, @]).
entry('Quell', [k, v, 'E', l]).
entry('Quere', [k, v, 'e:', r, @]).
entry(quer, [k, v, 'e:', '6']).
entry('Querstrich', [k, v, 'e:', '6', 'S', t, r, 'I', 'C']).
entry(quetschen, [k, v, 'E', tS, @, n]).
entry('Quittung', [k, v, 'I', t, 'U', 'N']).
entry(qu, [k, v]).
entry('Raab', [r, 'a:', p]).
entry('Rabatt', [r, a, b, a, t]).
entry(rabe, [r, 'a:', b, @]).
entry('Radeberger', [r, 'a:', d, @, b, 'E', '6', g, '6']).
entry('Rademacher', [r, 'a:', d, @, m, a, x, '6']).
entry(radikal, [r, a, d, i, k, 'a:', l]).
entry('Radl', [r, 'a:', d, l]).
entry('Radtour', [r, 'a:', t, t, 'u:', '6']).
entry(ragend, [r, 'a:', g, @, n, t]).
entry(ragen, [r, 'a:', g, @, n]).
entry('Rahmenprogramm', [r, 'a:', m, @, n, p, r, o, g, r, a, m]).
entry(rahmen, [r, 'a:', m, @, n]).
entry('Rahn', [r, 'a:', n]).
entry('Raiffeisen-Bank', [r, aI, f, aI, z, @, n, b, a, 'N', k]).
entry('Rande', [r, a, n, d, @]).
entry(randvoll, [r, a, n, t, f, 'O', l]).
entry('Rang', [r, a, 'N']).
entry(ranhalten, [r, a, n, h, a, l, t, @, n]).
entry(ranhängen, [r, a, n, h, 'E', 'N', @, n]).
entry(ranmachen, [r, a, n, m, a, x, @, n]).
entry(ran, [r, a, n]).
entry(rappeldickevoll, [r, a, p, @, l, d, 'I', k, @, f, 'O', l]).
entry(rappel, [r, a, p, @, l]).
entry('Ra', [r, a]).
entry(rasch, [r, a, 'S']).
entry(rasen, [r, 'a:', z, @, n]).
entry('Rastatt', [r, a, 'S', t, a, t]).
entry(raten, [r, 'a:', t, @, n]).
entry(rate, [r, 'a:', t, @]).
entry('Rathausmarkt', [r, 'a:', t, h, aU, s, m, a, r, k, t]).
entry('Rathaus', [r, 'a:', t, h, aU, s]).
entry(ratlos, [r, 'a:', t, l, 'o:', s]).
entry('Rat', [r, 'a:', t]).
entry('Ratzenkammerl', [r, a, ts, @, n, k, a, m, '6', l]).
entry(raubt, [r, aU, p, t]).
entry(rauchen, [r, aU, x, @, n]).
entry('Raucherabteil', [r, aU, x, '6', a, p, t, aI, l]).
entry(rauche, [r, aU, x, @]).
entry('Raucher-Plätze', [r, aU, x, '6', p, l, 'E', ts, @]).
entry('Raucher', [r, aU, x, '6']).
entry('Raucher-Zimmer', [r, aU, x, '6', ts, 'I', m, '6']).
entry(rauffahren, [r, aU, f, f, 'a:', r, @, n]).
entry(raufkommen, [r, aU, f, k, 'O', m, @, n]).
entry(rauf, [r, aU, f]).
entry('Räume', [r, 'OY', m, @]).
entry('Räumliche', [r, 'OY', m, l, 'I', 'C', @]).
entry('Räumlichkeiten', [r, 'OY', m, l, 'I', 'C', k, aI, t, @, n]).
entry(raum, [r, aU, m]).
entry(rau, [r, aU]).
entry(rausbekommen, [r, aU, s, b, @, k, 'O', m, @, n]).
entry('Rausch', [r, aU, 'S']).
entry(raußen, [r, aU, s, @, n]).
entry('Rause', [r, aU, z, @]).
entry(rausfahren, [r, aU, s, f, 'a:', r, @, n]).
entry(rausfallen, [r, aU, s, f, a, l, @, n]).
entry(rausfällt, [r, aU, s, f, 'E', l, t]).
entry(rausgefunden, [r, aU, s, g, @, f, 'U', n, d, @, n]).
entry(rausgehen, [r, aU, s, g, 'e:', @, n]).
entry(rausgekommen, [r, aU, s, g, @, k, 'O', m, @, n]).
entry(rausgeschaut, [r, aU, s, g, @, 'S', aU, t]).
entry(rausgeschrieben, [r, aU, s, g, @, 'S', r, 'i:', b, @, n]).
entry(rausgesucht, [r, aU, s, g, @, z, 'u:', x, t]).
entry(rausgezögert, [r, aU, s, g, @, ts, '2:', g, '6', t]).
entry(rausholen, [r, aU, s, h, 'o:', l, @, n]).
entry(rauskommen, [r, aU, s, k, 'O', m, @, n]).
entry(rauskommt, [r, aU, s, k, 'O', m, t]).
entry(rauskramen, [r, aU, s, k, r, 'a:', m, @, n]).
entry(rauskriegen, [r, aU, s, k, r, 'i:', g, @, n]).
entry(rauslassen, [r, aU, s, l, a, s, @, n]).
entry(rausnehmen, [r, aU, s, n, 'e:', m, @, n]).
entry(raus, [r, aU, s]).
entry(rausschauen, [r, aU, s, 'S', aU, @, n]).
entry(rausschicken, [r, aU, s, 'S', 'I', k, @, n]).
entry(rausschmeißen, [r, aU, s, 'S', m, aI, s, @, n]).
entry(rausschreiben, [r, aU, s, 'S', r, aI, b, @, n]).
entry(raussuchen, [r, aU, s, z, 'u:', x, @, n]).
entry(rauszögern, [r, aU, s, ts, '2:', g, '6', n]).
entry(rauszufinden, [r, aU, s, ts, u, f, 'I', n, d, @, n]).
entry(rauszukommen, [r, aU, s, ts, u, k, 'O', m, @, n]).
entry(rauszusuchen, [r, aU, s, ts, u, z, 'u:', x, @, n]).
entry(realisieren, [r, e, a, l, i, z, 'i:', r, @, n]).
entry(realistisch, [r, e, a, l, 'I', s, t, 'I', 'S']).
entry('Rebe', [r, 'e:', b, @]).
entry('Rechenzentrum', [r, 'E', 'C', @, n, ts, 'E', n, t, r, 'U', m]).
entry(rechnen, [r, 'E', 'C', n, @, n]).
entry(rechne, [r, 'E', 'C', n, @]).
entry('Rechnervernetzung', [r, 'E', 'C', n, '6', f, 'E', '6', n, 'E', ts, 'U', 'N']).
entry('Rechnung', [r, 'E', 'C', n, 'U', 'N']).
entry('Rechnungsabteilung', [r, 'E', 'C', n, 'U', 'N', s, a, p, t, aI, l, 'U', 'N']).
entry('Rechnungsprüfer', [r, 'E', 'C', n, 'U', 'N', s, p, r, 'y:', f, '6']).
entry(rech, [r, 'E', 'C']).
entry(rechtesten, [r, 'E', 'C', t, @, s, t, @, n]).
entry(rechtfertigt, [r, 'E', 'C', t, f, 'E', '6', t, 'I', 'C', t]).
entry(recht, [r, 'E', 'C', t]).
entry(rechts, [r, 'E', 'C', ts]).
entry(rechtzeitig, [r, 'E', 'C', t, ts, aI, t, 'I', 'C']).
entry('Recklinghausen', [r, 'E', k, l, 'I', 'N', h, aU, z, @, n]).
entry(reden, [r, 'e:', d, @, n]).
entry(rede, [r, 'e:', d, @]).
entry(redi, [r, 'e:', d, i]).
entry('Redoute', [r, e, d, 'u:', t, @]).
entry(reduziert, [r, e, d, u, ts, 'i:', '6', t]).
entry('Referate', [r, e, f, e, r, 'a:', t, @]).
entry('Referat', [r, e, f, e, r, 'a:', t]).
entry('Referenten', [r, e, f, e, r, 'E', n, t, @, n]).
entry(reflektieren, [r, e, f, l, 'E', k, t, 'i:', r, @, n]).
entry('Reformationstag', [r, e, f, 'O', '6', m, a, ts, j, 'o:', n, s, t, 'a:', k]).
entry(regele, [r, 'e:', g, @, l, @]).
entry(regelmäßige, [r, 'e:', g, @, l, m, 'E:', s, 'I', g, @]).
entry('Regelmäßigkeit', [r, 'e:', g, @, l, m, 'E:', s, 'I', 'C', k, aI, t]).
entry(regelmäßig, [r, 'e:', g, @, l, m, 'E:', s, 'I', 'C']).
entry(regeln, [r, 'e:', g, @, l, n]).
entry('Regel', [r, 'e:', g, @, l]).
entry('Regelung', [r, 'e:', g, @, l, 'U', 'N']).
entry(regenerieren, [r, e, g, e, n, e, r, 'i:', r, @, n]).
entry('Regensburg', [r, 'e:', g, @, n, s, b, 'U', '6', k]).
entry('Regie', [r, e, 'Z', 'i:']).
entry('Regionalbahn', [r, e, g, j, o, n, 'a:', l, b, 'a:', n]).
entry(registrieren, [r, e, g, 'I', s, t, r, 'i:', r, @, n]).
entry(registriert, [r, e, g, 'I', s, t, r, 'i:', '6', t]).
entry(regle, [r, 'e:', g, l, @]).
entry('Regularien', [r, e, g, u, l, 'a:', r, j, @, n]).
entry('Regul', [r, e, g, u, l]).
entry('Rehbein', [r, 'i:', b, aI, n]).
entry('Rehwein', [r, 'i:', w, aI, n]).
entry('Reichel', [r, aI, 'C', @, l]).
entry(reichen, [r, aI, 'C', @, n]).
entry(reiche, [r, aI, 'C', @]).
entry('Reichert', [r, aI, 'C', '6', t]).
entry(reichhaltiges, [r, aI, 'C', h, a, l, t, 'I', g, @, s]).
entry(reichlich, [r, aI, 'C', l, 'I', 'C']).
entry(reicht, [r, aI, 'C', t]).
entry('Reihenfolge', [r, aI, @, n, f, 'O', l, g, @]).
entry('Reihe', [r, aI, @]).
entry(reinarbeiten, [r, aI, n, a, r, b, aI, t, @, n]).
entry(reinblättern, [r, aI, n, b, l, 'E', t, '6', n]).
entry(reinbringen, [r, aI, n, b, r, 'I', 'N', @, n]).
entry(reinen, [r, aI, n, @, n]).
entry(reine, [r, aI, n, @]).
entry(reinfahren, [r, aI, n, f, 'a:', r, @, n]).
entry(reinfliegen, [r, aI, n, f, l, 'i:', g, @, n]).
entry(reingefahren, [r, aI, n, g, @, f, 'a:', r, @, n]).
entry(reingefallen, [r, aI, n, g, @, f, a, l, @, n]).
entry(reingehen, [r, aI, n, g, 'e:', @, n]).
entry(reingeht, [r, aI, n, g, 'e:', t]).
entry(reingelegt, [r, aI, n, g, @, l, 'e:', k, t]).
entry(reinge, [r, aI, n, g, @]).
entry(reingespart, [r, aI, n, g, @, 'S', p, 'a:', r, t]).
entry(reinhängen, [r, aI, n, h, 'E', 'N', @, n]).
entry('Reinhard', [r, aI, n, h, a, r, t]).
entry(reinkommen, [r, aI, n, k, 'O', m, @, n]).
entry(reinkommt, [r, aI, n, k, 'O', m, t]).
entry(reinkucken, [r, aI, n, k, 'U', k, @, n]).
entry(reinlegen, [r, aI, n, l, 'e:', g, @, n]).
entry(reinmüssen, [r, aI, n, m, 'Y', s, @, n]).
entry(reinnehmen, [r, aI, n, n, 'e:', m, @, n]).
entry('Reinöhl', [r, aI, n, '2:', l]).
entry(reinpacken, [r, aI, n, p, a, k, @, n]).
entry(reinpassen, [r, aI, n, p, a, s, @, n]).
entry(reinpl, [r, aI, n, p, l]).
entry(reinquetschen, [r, aI, n, k, v, 'E', tS, @, n]).
entry(rein, [r, aI, n]).
entry(reinschauen, [r, aI, n, 'S', aU, @, n]).
entry(reinschieben, [r, aI, n, 'S', 'i:', b, @, n]).
entry(reinsetzen, [r, aI, n, z, 'E', ts, @, n]).
entry(reinspielt, [r, aI, n, 'S', p, 'i:', l, t]).
entry(reinübernachten, [r, aI, n, 'y:', b, '6', n, a, x, t, @, n]).
entry(reinzupacken, [r, aI, n, ts, u, p, a, k, @, n]).
entry(reinzuquetschen, [r, aI, n, ts, u, k, v, 'E', tS, @, n]).
entry(rei, [r, aI]).
entry('Reiseablauf', [r, aI, z, @, a, p, l, aU, f]).
entry('Reiseabteilung', [r, aI, z, @, a, p, t, aI, l, 'U', 'N']).
entry('Reiseagentur', [r, aI, z, @, a, g, 'E', n, t, 'u:', '6']).
entry('Reiseantritt', [r, aI, z, @, a, n, t, r, 'I', t]).
entry('Reisebeginn', [r, aI, z, @, b, @, g, 'I', n]).
entry('Reisebe', [r, aI, z, @, b, @]).
entry('Reisebericht', [r, aI, z, @, b, @, r, 'I', 'C', t]).
entry('Reisebuchung', [r, aI, z, @, b, 'u:', x, 'U', 'N']).
entry('Reisebudget', [r, aI, z, @, b, 'Y', d, 'Z', 'e:']).
entry('Reisebüro', [r, aI, z, @, b, 'Y', r, 'o:']).
entry('Reisedauer', [r, aI, z, @, d, aU, '6']).
entry('Reisegelegenheiten', [r, aI, z, @, g, @, l, 'e:', g, @, n, h, aI, t, @, n]).
entry('Reiseinformationen', [r, aI, z, @, 'I', n, f, 'O', '6', m, a, ts, j, 'o:', n, @, n]).
entry('Reisekosten-Abrechnung', [r, aI, z, @, k, 'O', s, t, @, n, a, p, r, 'E', 'C', n, 'U', 'N']).
entry('Reisekosten', [r, aI, z, @, k, 'O', s, t, @, n]).
entry('Reisele', [r, aI, z, @, l, @]).
entry('Reisemöglichkeiten', [r, aI, z, @, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry(reisende, [r, aI, z, @, n, d, @]).
entry(reisen, [r, aI, z, @, n]).
entry('Reiseplänen', [r, aI, z, @, p, l, 'E:', n, @, n]).
entry('Reisepläne', [r, aI, z, @, p, l, 'E:', n, @]).
entry('Reisepreis', [r, aI, z, @, p, r, aI, s]).
entry(reise, [r, aI, z, @]).
entry('Reiser', [r, aI, z, '6']).
entry('Reisestelle', [r, aI, z, @, 'S', t, 'E', l, @]).
entry('Reisetage', [r, aI, z, @, t, 'a:', g, @]).
entry('Reisetag', [r, aI, z, @, t, 'a:', k]).
entry('Reiseterminen', [r, aI, z, @, t, 'E', '6', m, 'i:', n, @, n]).
entry('Reisetermin', [r, aI, z, @, t, 'E', '6', m, 'i:', n]).
entry('Reiseunterlagen', [r, aI, z, @, 'U', n, t, '6', l, 'a:', g, @, n]).
entry('Reiseverbindungen', [r, aI, z, @, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry('Reiseverbindung', [r, aI, z, @, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('Reiseverkehrs-Kaufmann', [r, aI, z, @, f, 'E', '6', k, 'e:', '6', s, k, aU, f, m, a, n]).
entry('Reisevorbereitungen', [r, aI, z, @, f, 'o:', '6', b, @, r, aI, t, 'U', 'N', @, n]).
entry('Reisevorbereitung', [r, aI, z, @, f, 'o:', '6', b, @, r, aI, t, 'U', 'N']).
entry('Reisevorbesprechung', [r, aI, z, @, f, 'o:', '6', b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Reisezeiten', [r, aI, z, @, ts, aI, t, @, n]).
entry('Reisezeit', [r, aI, z, @, ts, aI, t]).
entry('Reisezentrum', [r, aI, z, @, ts, 'E', n, t, r, 'U', m]).
entry(reizend, [r, aI, ts, @, n, t]).
entry(reizen, [r, aI, ts, @, n]).
entry(reizvoll, [r, aI, ts, f, 'O', l]).
entry(rekapituliere, [r, e, k, a, p, i, t, u, l, 'i:', r, @]).
entry(rela, [r, e, l, a]).
entry(relativ, [r, e, l, a, t, 'i:', f]).
entry(relaviel, [r, e, l, a, f, 'i:', l]).
entry(relaxed, [r, 'I', l, 'E', k, s, t]).
entry(relevanten, [r, e, l, e, v, a, n, t, @, n]).
entry(relevant, [r, e, l, e, v, a, n, t]).
entry(religiösem, [r, e, l, i, g, j, '2:', z, @, m]).
entry(religiösen, [r, e, l, i, g, j, '2:', z, @, n]).
entry(rel, [r, e, l]).
entry('Rendsburg', [r, 'E', n, ts, b, 'U', '6', k]).
entry(rennen, [r, 'E', n, @, n]).
entry('Renner', [r, 'E', n, '6']).
entry(renommiertes, [r, e, n, 'O', m, 'i:', '6', t, @, s]).
entry('Renovierung', [r, e, n, o, v, 'i:', r, 'U', 'N']).
entry(renov, [r, e, n, o, f]).
entry(ren, [r, 'E', n]).
entry('Reparatur', [r, e, p, a, r, a, t, 'u:', '6']).
entry('Republik', [r, e, p, u, b, l, 'i:', k]).
entry(re, [r, e]).
entry(reschang, [r, 'E', 'S', a, 'N']).
entry(reservation, [r, 'I', z, @, r, v, e, 'I', 'S', @, n]).
entry(reservieren, [r, e, z, 'E', '6', v, 'i:', r, @, n]).
entry(reserviere, [r, e, z, 'E', '6', v, 'i:', r, @]).
entry('Reserviernun', [r, 'e:', z, 'E', '6', f, 'i:', '6', n, 'u:', n]).
entry(reserviert, [r, e, z, 'E', '6', v, 'i:', '6', t]).
entry('Reservierungen', [r, e, z, 'E', '6', v, 'i:', r, 'U', 'N']).
entry(reservierungen, [r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', @, n]).
entry('Reservierungsgutschein', [r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', s, g, 'u:', tS, aI, n]).
entry('Reservierungsnummer', [r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', s, n, 'U', m, '6']).
entry('Reserv', [r, e, z, 'E', '6', f]).
entry(residiere, [r, e, z, i, d, 'i:', r, @]).
entry('Restaurantbesuche', [r, 'E', s, t, o, r, 'a~:', b, @, z, 'u:', x, @]).
entry('Restaurant', [r, 'E', s, t, o, r, 'a~:']).
entry('Restaurants', [r, 'E', s, t, o, r, 'a~:', s]).
entry(restlichen, [r, 'E', s, t, l, 'I', 'C', @, n]).
entry(restliche, [r, 'E', s, t, l, 'I', 'C', @]).
entry('Rest', [r, 'E', s, t]).
entry('Reutlingen', [r, 'OY', t, l, 'I', 'N', @, n]).
entry(revanchieren, [r, e, v, 'a~', 'S', 'i:', r, @, n]).
entry('Revisionstreffen', [r, e, v, i, z, j, 'o:', n, s, t, r, 'E', f, @, n]).
entry('Revue', [r, e, v, 'y:']).
entry('Rezepte', [r, e, ts, 'E', p, t, @]).
entry('Rheinländer', [r, aI, n, l, 'E', n, d, '6']).
entry('Rheinland', [r, aI, n, l, a, n, t]).
entry('Rhetorikkurse', [r, e, t, 'o:', r, 'I', k, k, 'U', '6', z, @]).
entry('Rhetorikkurs', [r, e, t, 'o:', r, 'I', k, k, 'U', '6', s]).
entry(rhythmus, [r, 'Y', t, m, 'U', s]).
entry('Richard', [r, 'I', 'C', a, r, t]).
entry(richten, [r, 'I', 'C', t, @, n]).
entry(richte, [r, 'I', 'C', t, @]).
entry(richtet, [r, 'I', 'C', t, @, t]).
entry(richtigen, [r, 'I', 'C', t, 'I', g, @, n]).
entry(richtige, [r, 'I', 'C', t, 'I', g, @]).
entry(richtiger, [r, 'I', 'C', t, 'I', g, '6']).
entry(richtiges, [r, 'I', 'C', t, 'I', g, @, s]).
entry(richtig, [r, 'I', 'C', t, 'I', 'C']).
entry(richti, [r, 'I', 'C', t, 'I']).
entry('Richtungen', [r, 'I', 'C', t, 'U', 'N', @, n]).
entry('Richtung', [r, 'I', 'C', t, 'U', 'N']).
entry('Rickhoff', [r, 'I', k, h, 'O', f]).
entry('Riem', [r, 'i:', m]).
entry(riesen, [r, 'i:', z, @, n]).
entry('Riese', [r, 'i:', z, @]).
entry(rima, [r, 'i:', m, a]).
entry('Ripfel', [r, 'I', pf, @, l]).
entry(ri, [r, 'I']).
entry('Ri', [r, i]).
entry('Risiko', [r, 'i:', z, i, k, o]).
entry(riskant, [r, 'I', s, k, a, n, t]).
entry('Ritter', [r, 'I', t, '6']).
entry('Rix', [r, 'I', k, s]).
entry('Roberta', [r, @, b, 'E:', r, t, '6']).
entry('Robert', [r, 'o:', b, '6', t]).
entry('Rochusstraße', [r, 'O', x, 'U', s, 'S', t, r, 'a:', s, @]).
entry(roger, [r, 'O', d, 'Z', '6']).
entry('Rohde', [r, 'o:', d, @]).
entry('Rohlfling', [r, 'o:', l, f, l, 'I', 'N']).
entry('Röhre', [r, '2:', r, @]).
entry('Rollen', [r, 'O', l, @, n]).
entry('Rolle', [r, 'O', l, @]).
entry('Rollschuhen', [r, 'O', l, 'S', 'u:', @, n]).
entry(romantischen, [r, o, m, a, n, t, 'I', 'S', @, n]).
entry(romantischer, [r, o, m, a, n, t, 'I', 'S', '6']).
entry(romantisch, [r, o, m, a, n, t, 'I', 'S']).
entry('Römer', [r, '2:', m, '6']).
entry('Rooster-Group', [r, 'u:', s, t, '6', g, r, 'u:', p]).
entry(ro, [r, o]).
entry('Rosenmontag', [r, 'o:', z, @, n, m, 'o:', n, t, 'a:', k]).
entry('Rosenmontags', [r, 'o:', z, @, n, m, 'o:', n, t, 'a:', k, s]).
entry('Rosenmontags-Sendung', [r, 'o:', z, @, n, m, 'o:', n, t, 'a:', k, s, z, 'E', n, d, 'U', 'N']).
entry('Rosenstock', [r, 'o:', z, @, n, 'S', t, 'O', k]).
entry(rosig, [r, 'o:', z, 'I', 'C']).
entry('Rössner', [r, '9', s, n, '6']).
entry('Rostock', [r, 'O', s, t, 'O', k]).
entry(roten, [r, 'o:', t, @, n]).
entry('Rothenbaumchaussee', [r, @, 'U', f, @, n, b, aU, m, 'S', @, 'U', s, 'i:']).
entry('Rothenburg', [r, 'o:', t, @, n, b, 'U', '6', k]).
entry('Rot', [r, 'o:', t]).
entry('Rottweil', [r, 'O', t, v, aI, l]).
entry('Rotwein', [r, 'o:', t, v, aI, n]).
entry('Rouette', [r, 'u:', 'E', t]).
entry('Routinen', [r, u, t, 'i:', n, @, n]).
entry('Routine', [r, u, t, 'i:', n, @]).
entry('Routineuntersuchung', [r, u, t, 'i:', n, @, 'U', n, t, '6', z, 'u:', x, 'U', 'N']).
entry('Royal', [r, 'O', a, j, 'a:', l]).
entry(r, ['E', '6']).
entry('Rübenschmidt', [r, 'y:', b, @, n, 'S', m, 'I', t]).
entry(rüberfahren, [r, 'y:', b, '6', f, 'a:', r, @, n]).
entry(rüberfaxen, [r, 'y:', b, '6', f, a, k, s, @, n]).
entry(rübergehend, [r, 'y:', b, '6', g, 'e:', @, n, t]).
entry(rüberjetten, [r, 'y:', b, '6', d, 'Z', 'E', t, @, n]).
entry(rüberkommen, [r, 'y:', b, '6', k, 'O', m, @, n]).
entry(rüberreichen, [r, 'y:', b, '6', r, aI, 'C', @, n]).
entry(rüber, [r, 'y:', b, '6']).
entry(ruckelt, [r, 'U', k, @, l, t]).
entry('Rückenleiden', [r, 'Y', k, @, n, l, aI, d, @, n]).
entry('Rückfahren', [r, 'Y', k, f, 'a:', r, @, n]).
entry('Rückfahrkarte', [r, 'Y', k, f, 'a:', r, k, a, r, t, @]).
entry('Rückfahrmöglichkeit', [r, 'Y', k, f, 'a:', r, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Rückfahr', [r, 'Y', k, f, 'a:', r]).
entry('Rückfahrticket', [r, 'Y', k, f, 'a:', r, t, 'I', k, @, t]).
entry('Rückfahrtickets', [r, 'Y', k, f, 'a:', r, t, 'I', k, @, ts]).
entry('Rückfahrt', [r, 'Y', k, f, 'a:', r, t]).
entry('Rückfliegen', [r, 'Y', k, f, l, 'i:', g, @, n]).
entry('Rückflüge', [r, 'Y', k, f, l, 'y:', g, @]).
entry('Rückflug', [r, 'Y', k, f, l, 'u:', k]).
entry('Rückflugzeit', [r, 'Y', k, f, l, 'u:', k, ts, aI, t]).
entry(rückfragen, [r, 'Y', k, f, r, 'a:', g, @, n]).
entry('Rückf', [r, 'Y', k, f]).
entry(rückgängig, [r, 'Y', k, g, 'E', 'N', 'I', 'C']).
entry('Rückkehr', [r, 'Y', k, k, 'e:', '6']).
entry('Rückmeldung', [r, 'Y', k, m, 'E', l, d, 'U', 'N']).
entry('Rückreise', [r, 'Y', k, r, aI, z, @]).
entry('Rückreisetermin', [r, 'Y', k, r, aI, z, @, t, 'E', '6', m, 'i:', n]).
entry('Rückreisewelle', [r, 'Y', k, r, aI, z, @, v, 'E', l, @]).
entry('Rück', [r, 'Y', k]).
entry('Rückscha', [r, 'Y', k, 'S', a]).
entry('Rücksprache', [r, 'Y', k, 'S', p, r, 'a:', x, @]).
entry('Rücktermin', [r, 'Y', k, t, 'E', '6', m, 'i:', n]).
entry('Rückverbindungen', [r, 'Y', k, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry(rückwärts, [r, 'Y', k, v, 'E', '6', ts]).
entry('Rückweg', [r, 'Y', k, v, 'e:', k]).
entry(rufen, [r, 'u:', f, @, n]).
entry(rufe, [r, 'u:', f, @]).
entry('Ruf', [r, 'u:', f]).
entry(rufst, [r, 'u:', f, s, t]).
entry(ruft, [r, 'u:', f, t]).
entry('Ruhe', [r, 'u:', @]).
entry(ruhigeres, [r, 'u:', 'I', g, @, r, @, s]).
entry(ruhiger, [r, 'u:', 'I', g, '6']).
entry(ruhige, [r, 'u:', 'I', g, @]).
entry(ruhiges, [r, 'u:', 'I', g, @, s]).
entry(ruhig, [r, 'u:', 'I', 'C']).
entry(ruhigste, [r, 'u:', 'I', 'C', s, t, @]).
entry(rühre, [r, 'y:', r, @]).
entry(ruh, [r, u]).
entry(rülpsen, [r, 'Y', l, p, s, @, n]).
entry(rumdrücken, [r, 'U', m, d, r, 'Y', k, @, n]).
entry(rumgehen, [r, 'U', m, g, 'e:', @, n]).
entry(rumkriegen, [r, 'U', m, k, r, 'i:', g, @, n]).
entry(rumreisen, [r, 'U', m, r, aI, z, @, n]).
entry(rum, [r, 'U', m]).
entry(rumschauen, [r, 'U', m, 'S', aU, @, n]).
entry(rumsehen, [r, 'U', m, z, 'e:', @, n]).
entry(rumstehen, [r, 'U', m, 'S', t, 'e:', @, n]).
entry(rumtelefonieren, [r, 'U', m, t, e, l, e, f, o, n, 'i:', r, @, n]).
entry(rumzubringen, [r, 'U', m, ts, u, b, r, 'I', 'N', @, n]).
entry(rumzuhüpfen, [r, 'U', m, ts, u, h, 'Y', pf, @, n]).
entry(rumzusitzen, [r, 'U', m, ts, u, z, 'I', ts, @, n]).
entry(rumzustehen, [r, 'U', m, ts, u, 'S', t, 'e:', @, n]).
entry(runden, [r, 'U', n, d, @, n]).
entry(runder, [r, 'U', n, d, '6']).
entry(runde, [r, 'U', n, d, @]).
entry('Rundreise', [r, 'U', n, t, r, aI, z, @]).
entry(rund, [r, 'U', n, t]).
entry('Rundschreiben', [r, 'U', n, tS, r, aI, b, @, n]).
entry(runterfahren, [r, 'U', n, t, '6', f, 'a:', r, @, n]).
entry(runtergekommen, [r, 'U', n, t, '6', g, @, k, 'O', m, @, n]).
entry(runtergeschaut, [r, 'U', n, t, '6', g, @, 'S', aU, t]).
entry(runter, [r, 'U', n, t, '6']).
entry(runterzufahren, [r, 'U', n, t, '6', ts, u, f, 'a:', r, @, n]).
entry('Ruppert', [r, 'U', p, '6', t]).
entry(rü, [r, 'y:']).
entry('Rüt', [r, 'y:', t]).
entry('Rutsch', [r, 'U', tS]).
entry(rütteln, [r, 'Y', t, @, l, n]).
entry('Rutzinger', [r, 'U', ts, 'I', 'N', '6']).
entry(rz, ['6', ts]).
entry('Saarbrücken', [z, a, r, b, r, 'Y', k, @, n]).
entry('Sabena', [z, a, b, 'e:', n, a]).
entry('Sabine', [z, a, b, 'i:', n, @]).
entry('Sachbearbeiterin', [z, a, x, b, @, a, r, b, aI, t, @, r, 'I', n]).
entry('Sachbear', [z, a, x, b, @, a, r]).
entry(sachen, [z, a, x, @, n]).
entry('Sache', [z, a, x, @]).
entry('Sachverhalt', [z, a, x, f, 'E', '6', h, a, l, t]).
entry(sacken, [z, a, k, @, n]).
entry('Safaribüchse', [z, a, f, 'a:', r, i, b, 'Y', k, s, @]).
entry(sagen, [z, 'a:', g, @, n]).
entry('Sagerer', [z, 'a:', g, @, r, '6']).
entry(sage, [z, 'a:', g, @]).
entry(sagst, [z, 'a:', k, s, t]).
entry(sagten, [z, 'a:', k, t, @, n]).
entry(sagte, [z, 'a:', k, t, @]).
entry(sagt, [z, 'a:', k, t]).
entry(sähe, [z, 'E:', @]).
entry(säh, [z, 'E:']).
entry('Salatbuffet', [z, a, l, 'a:', t, b, 'Y', f, 'e:']).
entry('Salatteller', [z, a, l, 'a:', t, t, 'E', l, '6']).
entry(sammeln, [z, a, m, @, l, n]).
entry('Samstagen', [z, a, m, s, t, 'a:', g, @, n]).
entry('Samstage', [z, a, m, s, t, 'a:', g, @]).
entry(samstags, [z, a, m, s, t, 'a:', k, s]).
entry('Samstag', [z, a, m, s, t, 'a:', k]).
entry('Sams', [z, a, m, s]).
entry(sämtlichen, [z, 'E', m, t, l, 'I', 'C', @, n]).
entry(sämtliche, [z, 'E', m, t, l, 'I', 'C', @]).
entry(sam, [z, 'a:', m]).
entry('Sam', [z, a, m]).
entry('Sanders', [z, a, n, d, '6', s]).
entry('Sander', [z, a, n, d, '6']).
entry(sann, [s, a, n]).
entry('Sara', [z, 'a:', r, a]).
entry('Saskia', [s, 'E', s, k, i, @]).
entry(sas, [z, a, s]).
entry('Satellit', [z, a, t, 'E', l, 'i:', t]).
entry(satte, [z, a, t, @]).
entry(sauber, [z, aU, b, '6']).
entry('Sauer', [z, aU, '6']).
entry('Saunaangebot', [z, aU, n, 'a:', a, n, g, @, b, 'o:', t]).
entry('Saunafan', [z, aU, n, a, f, 'E:', n]).
entry('Saunafreak', [z, aU, n, a, f, r, 'i:', k]).
entry('Saunagänger', [z, aU, n, a, g, 'E', 'N', '6']).
entry('Sauna', [z, aU, n, a]).
entry('Saun', [z, aU, n]).
entry(sauren, [z, aU, r, @, n]).
entry(sausen, [z, aU, z, @, n]).
entry('Sau', [z, aU]).
entry('Savoy', [z, a, v, 'OY']).
entry(sa, [z, a]).
entry('Scapoff', [s, k, 'E', p, 'O', f]).
entry('Schaaf', ['S', 'a:', f]).
entry(schaden, ['S', 'a:', d, @, n]).
entry('Schäden', ['S', 'E:', d, @, n]).
entry(schade, ['S', 'a:', d, @]).
entry(schadet, ['S', 'a:', d, @, t]).
entry('Schaeffler', ['S', 'E', f, l, '6']).
entry('Schäfer', ['S', 'E:', f, '6']).
entry(schaffbar, ['S', a, f, b, 'a:', r]).
entry(schaffen, ['S', a, f, @, n]).
entry(schaffe, ['S', a, f, @]).
entry(schag, ['S', 'a:', k]).
entry(schahrscheinlich, ['S', 'a:', r, 'S', aI, n, l, 'I', 'C']).
entry('Schalter', ['S', a, l, t, '6']).
entry(schäme, ['S', 'E:', m, @]).
entry('Schampus', ['S', a, m, p, 'U', s]).
entry('Schande', ['S', a, n, d, @]).
entry(scharfes, ['S', a, r, f, @, s]).
entry(scharf, ['S', a, r, f]).
entry(schäts, ['S', 'E:', ts]).
entry('Schatten', ['S', a, t, @, n]).
entry(schätzen, ['S', 'E', ts, @, n]).
entry(schätze, ['S', 'E', ts, @]).
entry('Schätzle', ['S', 'E', ts, l, @]).
entry('Schätz', ['S', 'E', ts]).
entry(schätzungsweise, ['S', 'E', ts, u, 'N', s, v, aI, z, @]).
entry(schauen, ['S', aU, @, n]).
entry(schaue, ['S', aU, @]).
entry(schaukeln, ['S', aU, k, @, l, n]).
entry('Schauspielhaus', ['S', aU, 'S', p, 'i:', l, h, aU, s]).
entry('Schauspiel', ['S', aU, 'S', p, 'i:', l]).
entry(schaut, ['S', aU, t]).
entry('Scheck', ['S', 'E', k]).
entry('Scheer', ['S', 'e:', '6']).
entry(scheiden, ['S', aI, d, @, n]).
entry(scheidet, ['S', aI, d, @, t]).
entry(scheinbar, ['S', aI, n, b, 'a:', r]).
entry(scheinen, ['S', aI, n, @, n]).
entry(scheine, ['S', aI, n, @]).
entry(scheinst, ['S', aI, n, s, t]).
entry(scheint, ['S', aI, n, t]).
entry(schei, ['S', aI]).
entry('Scheiße', ['S', aI, s, @]).
entry('Schellingsalon', ['S', 'E', l, 'I', 'N', z, a, l, 'o~:']).
entry('Schellingstraße', ['S', 'E', l, 'I', 'N', 'S', t, r, 'a:', s, @]).
entry('Schendel', ['S', 'E', n, d, @, l]).
entry('Schenk', ['S', 'E', 'N', k]).
entry(schenkt, ['S', 'E', 'N', k, t]).
entry('Scherz', ['S', 'E', '6', ts]).
entry(sche, ['S', e]).
entry('Scheytt', ['S', aI, t]).
entry(schicken, ['S', 'I', k, @, n]).
entry(schicke, ['S', 'I', k, @]).
entry(schick, ['S', 'I', k]).
entry(schickt, ['S', 'I', k, t]).
entry(schieben, ['S', 'i:', b, @, n]).
entry(schiedene, ['S', 'i:', d, @, n, @]).
entry(schiefgehen, ['S', 'i:', f, g, 'e:', @, n]).
entry('Schiel', ['S', 'i:', l]).
entry(schienen, ['S', 'i:', n, @, n]).
entry(schiene, ['S', 'i:', n, @]).
entry(schien, ['S', 'i:', n]).
entry(schießt, ['S', 'i:', s, t]).
entry('Schiete', ['S', 'i:', t, @]).
entry('Schiller', ['S', 'I', l, '6']).
entry('Schindelbeck', ['S', 'I', n, d, @, l, b, 'E', k]).
entry('Schindel', ['S', 'I', n, d, @, l]).
entry(schinden, ['S', 'I', n, d, @, n]).
entry('Schirmer', ['S', 'I', '6', m, '6']).
entry(schi, ['S', 'I']).
entry('Schislowsky', ['S', 'I', s, l, 'O', f, s, k, i]).
entry(schlafen, ['S', l, 'a:', f, @, n]).
entry(schlafe, ['S', l, 'a:', f, @]).
entry('Schlaf', ['S', l, 'a:', f]).
entry(schläft, ['S', l, 'E:', f, t]).
entry('Schlafwagen', ['S', l, 'a:', f, v, 'a:', g, @, n]).
entry('Schlafwaggon', ['S', l, 'a:', f, v, a, g, 'O', 'N']).
entry(schlagen, ['S', l, 'a:', g, @, n]).
entry(schlage, ['S', l, 'a:', g, @]).
entry(schlag, ['S', l, 'a:', k]).
entry(schlägt, ['S', l, 'E:', k, t]).
entry('Schlange', ['S', l, a, 'N', @]).
entry(schla, ['S', l, a]).
entry(schlauer, ['S', l, aU, '6']).
entry(schlau, ['S', l, aU]).
entry(schlech, ['S', l, 'E', 'C']).
entry(schlechten, ['S', l, 'E', 'C', t, @, n]).
entry(schlechteren, ['S', l, 'E', 'C', t, @, r, @, n]).
entry(schlechtere, ['S', l, 'E', 'C', t, @, r, @]).
entry(schlechter, ['S', l, 'E', 'C', t, '6']).
entry(schlechte, ['S', l, 'E', 'C', t, @]).
entry(schlechtes, ['S', l, 'E', 'C', t, @, s]).
entry(schlecht, ['S', l, 'E', 'C', t]).
entry('Schleicher', ['S', l, aI, 'C', '6']).
entry(schleppen, ['S', l, 'E', p, @, n]).
entry(schle, ['S', l, 'E']).
entry('Schleswig-Holstein', ['S', l, 'e:', s, v, 'I', 'C', h, 'O', l, 'S', t, aI, n]).
entry('Schleswig', ['S', l, 'e:', s, v, 'I', 'C']).
entry(schlicht, ['S', l, 'I', 'C', t]).
entry(schlichtweg, ['S', l, 'I', 'C', t, v, 'E', k]).
entry(schließen, ['S', l, 'i:', s, @, n]).
entry(schließe, ['S', l, 'i:', s, @]).
entry(schließlich, ['S', l, 'i:', s, l, 'I', 'C']).
entry(schließt, ['S', l, 'i:', s, t]).
entry('Schlimmes', ['S', l, 'I', m, @, s]).
entry(schlimm, ['S', l, 'I', m]).
entry(schlimmste, ['S', l, 'I', m, s, t, @]).
entry('Schlitz', ['S', l, 'I', ts]).
entry('Schlizio', ['S', l, 'I', ts, j, o]).
entry('Schlör-Quell', ['S', l, '2:', '6', k, v, 'E', l]).
entry('Schloßgaststätte', ['S', l, 'O', s, g, a, s, tS, t, 'E', t, @]).
entry('Schloßhotel', ['S', l, 'O', s, h, o, t, 'E', l]).
entry('Schloß', ['S', l, 'O', s]).
entry('Schlott', ['S', l, 'O', t]).
entry(schl, ['S', l]).
entry(schlüge, ['S', l, 'y:', g, @]).
entry(schlug, ['S', l, 'u:', k]).
entry('Schlummertrunk', ['S', l, 'U', m, '6', t, r, u, 'N', k]).
entry('Schlu', ['S', l, 'U']).
entry('Schlüssel', ['S', l, 'Y', s, @, l]).
entry(schlüssig, ['S', l, 'Y', s, 'I', 'C']).
entry('Schluß', ['S', l, 'U', s]).
entry('Schmarren', ['S', m, a, r, @, n]).
entry(schmeckt, ['S', m, 'E', k, t]).
entry(schmeiße, ['S', m, aI, s, @]).
entry('Schmid-Müller-Bardorf', ['S', m, 'I', t, m, 'Y', l, '6', b, 'a:', r, d, 'O', '6', f]).
entry('Schmid', ['S', m, 'I', t]).
entry('Schmied', ['S', m, 'i:', t]).
entry('Schmitz', ['S', m, 'I', ts]).
entry('Schmuck', ['S', m, 'U', k]).
entry(schnappen, ['S', n, a, p, @, n]).
entry(schnarchen, ['S', n, a, r, 'C', @, n]).
entry('Schnee', ['S', n, 'e:']).
entry('Schneidereit', ['S', n, aI, d, @, r, aI, t]).
entry('Schneider', ['S', n, aI, d, '6']).
entry(schneit, ['S', n, aI, t]).
entry(schnellen, ['S', n, 'E', l, @, n]).
entry(schneller, ['S', n, 'E', l, '6']).
entry(schnelle, ['S', n, 'E', l, @]).
entry(schnelles, ['S', n, 'E', l, @, s]).
entry(schnell, ['S', n, 'E', l]).
entry(schnellsten, ['S', n, 'E', l, s, t, @, n]).
entry(schnellstens, ['S', n, 'E', l, s, t, @, n, s]).
entry(schnellste, ['S', n, 'E', l, s, t, @]).
entry('Schnerring', ['S', n, 'E', r, 'I', 'N']).
entry('Schniepenpiepel', ['S', n, 'i:', p, @, n, p, 'i:', p, @, l]).
entry('Schnittmenge', ['S', n, 'I', t, m, 'E', 'N', @]).
entry(schnuckelige, ['S', n, 'U', k, @, l, 'I', g, @]).
entry('Schnulze', ['S', n, 'U', l, ts, @]).
entry(schnurzegal, ['S', n, 'U', '6', ts, e, g, 'a:', l]).
entry('Schoen', ['S', 'O', 'I', n]).
entry(schönen, ['S', '2:', n, @, n]).
entry(schonen, ['S', 'o:', n, @, n]).
entry(schöneres, ['S', '2:', n, @, r, @, s]).
entry(schöner, ['S', '2:', n, '6']).
entry(schöne, ['S', '2:', n, @]).
entry(schönes, ['S', '2:', n, @, s]).
entry('Schönkirchen', ['S', '2:', n, k, 'I', '6', 'C', @, n]).
entry(schön, ['S', '2:', n]).
entry(schon, ['S', 'o:', n]).
entry(schönsten, ['S', '2:', n, s, t, @, n]).
entry(schönste, ['S', '2:', n, s, t, @]).
entry('Schoppen', ['S', 'O', p, @, n]).
entry(schö, ['S', '2:']).
entry(scho, ['S', 'o:']).
entry('Schotten', ['S', 'O', t, @, n]).
entry('Schrade', ['S', r, 'a:', d, @]).
entry(schrat, ['S', r, a, t]).
entry('Schrecken', ['S', r, 'E', k, @, n]).
entry(schrecklichen, ['S', r, 'E', k, l, 'I', 'C', @, n]).
entry(schreckliches, ['S', r, 'E', k, l, 'I', 'C', @, s]).
entry(schrecklich, ['S', r, 'E', k, l, 'I', 'C']).
entry(schreiben, ['S', r, aI, b, @, n]).
entry('Schreiber', ['S', r, aI, b, '6']).
entry(schreibe, ['S', r, aI, b, @]).
entry('Schreibtisch', ['S', r, aI, p, t, 'I', 'S']).
entry(schreibt, ['S', r, aI, p, t]).
entry(schrei, ['S', r, aI]).
entry(schreiten, ['S', r, aI, t, @, n]).
entry(schrie, ['S', r, 'i:']).
entry(schriftlich, ['S', r, 'I', f, t, l, 'I', 'C']).
entry('Schritten', ['S', r, 'I', t, @, n]).
entry('Schröder', ['S', r, '2:', d, '6']).
entry('Schrot', ['S', r, 'o:', t]).
entry(schr, ['S', r]).
entry(sch, ['S']).
entry(scht, ['S', t]).
entry('Schuler', ['S', 'u:', l, '6']).
entry('Schule', ['S', 'u:', l, @]).
entry('Schulferien', ['S', 'u:', l, f, 'e:', '6', j, @, n]).
entry('Schulkinder', ['S', 'u:', l, k, 'I', n, d, '6']).
entry('Schüller', ['S', 'Y', l, '6']).
entry('Schulte', ['S', 'U', l, t, @]).
entry('Schulungstreffen', ['S', 'u:', l, 'U', 'N', s, t, r, 'E', f, @, n]).
entry('Schulung', ['S', 'u:', l, 'U', 'N']).
entry('Schulzeit', ['S', 'u:', l, ts, aI, t]).
entry('Schulze', ['S', 'U', l, ts, @]).
entry('Schulz', ['S', 'U', l, ts]).
entry('Schumacher', ['S', 'u:', m, a, x, '6']).
entry(schun, ['S', 'U', n]).
entry('Schür', ['S', 'y:', '6']).
entry('Schuß', ['S', 'U', s]).
entry('Schuster', ['S', 'u:', s, t, '6']).
entry('Schuth', ['S', 'u:', t]).
entry('Schüttauf', ['S', 'Y', t, aU, f]).
entry('Schüttemeyer', ['S', 'Y', t, @, m, aI, '6']).
entry('Schützenfes', ['S', 'Y', ts, @, n, f, 'E', s]).
entry('Schützenfest', ['S', 'Y', ts, @, n, f, 'E', s, t]).
entry('Schützenverein', ['S', 'Y', ts, @, n, f, 'E', '6', aI, n]).
entry('Schwaben', ['S', v, 'a:', b, @, n]).
entry('Schwäbisch-Gmünd', ['S', v, 'E:', b, 'I', 'S', g, m, 'Y', n, t]).
entry(schwache, ['S', v, a, x, @]).
entry(schwach, ['S', v, a, x]).
entry('Schwägerin', ['S', v, 'E:', g, @, r, 'I', n]).
entry('Schwaiger', ['S', v, aI, g, '6']).
entry('Schwarze', ['S', v, a, r, ts, @]).
entry(schwarz, ['S', v, a, r, ts]).
entry('Schwarzwälder', ['S', v, a, r, ts, v, 'E', l, d, '6']).
entry(schwebt, ['S', v, 'e:', p, t]).
entry(schweifen, ['S', v, aI, f, @, n]).
entry('Schweikl', ['S', v, aI, k, l]).
entry('Schweine-Zuchtanstalt', ['S', v, aI, n, @, ts, 'U', x, t, a, n, 'S', t, a, l, t]).
entry('Schwein', ['S', v, aI, n]).
entry('Schweisthal', ['S', v, aI, s, t, 'a:', l]).
entry('Schweizerhof', ['S', v, aI, ts, '6', h, 'o:', f]).
entry(schweren, ['S', v, 'e:', r, @, n]).
entry(schweres, ['S', v, 'e:', r, @, s]).
entry('Schwerin', ['S', v, e, r, 'i:', n]).
entry('Schwerpunkte', ['S', v, 'e:', '6', p, 'U', 'N', k, t, @]).
entry(schwerpunktmäßig, ['S', v, 'e:', '6', p, 'U', 'N', k, t, m, 'E:', s, 'I', 'C']).
entry(schwer, ['S', v, 'e:', '6']).
entry('Schwester', ['S', v, 'E', s, t, '6']).
entry('Schwidewski', ['S', v, i, d, 'E', f, s, k, i]).
entry(schwierigen, ['S', v, 'i:', r, 'I', g, @, n]).
entry(schwierigeres, ['S', v, 'i:', r, 'I', g, @, r, @, s]).
entry(schwieriger, ['S', v, 'i:', r, 'I', g, '6']).
entry(schwierige, ['S', v, 'i:', r, 'I', g, @]).
entry('Schwierigkeiten', ['S', v, 'i:', r, 'I', 'C', k, aI, t, @, n]).
entry('Schwierigste', ['S', v, 'i:', r, 'I', 'C', s, t, @]).
entry(schwierig, ['S', v, 'i:', r, 'I', 'C']).
entry('Schwill', ['S', v, 'I', l]).
entry('Schwimmbads', ['S', v, 'I', m, b, 'a:', ts]).
entry('Schwimmbad', ['S', v, 'I', m, b, 'a:', t]).
entry(schwimmen, ['S', v, 'I', m, @, n]).
entry(schwimme, ['S', v, 'I', m, @]).
entry('Schwimm', ['S', v, 'I', m]).
entry(schwindlig, ['S', v, 'I', n, d, l, 'I', 'C']).
entry(schwitzen, ['S', v, 'I', ts, @, n]).
entry(schw, ['S', v]).
entry('Schwung', ['S', v, 'U', 'N']).
entry('Sebastian', [z, e, b, a, s, t, j, a, n]).
entry(sechshundert, [z, 'E', k, s, h, 'U', n, d, '6', t]).
entry(sechstägigen, [z, 'E', k, s, t, 'E:', g, 'I', g, @, n]).
entry(sechsten, [z, 'E', k, s, t, @, n]).
entry(sechster, [z, 'E', k, s, t, '6']).
entry(sechste, [z, 'E', k, s, t, @]).
entry('Sechs-Uhr-Zug', [z, 'E', k, s, 'u:', '6', ts, 'u:', k]).
entry(sechsundachtzig, [z, 'E', k, s, 'U', n, t, a, x, ts, 'I', 'C']).
entry(sechsunddreißigsten, [z, 'E', k, s, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(sechsunddreißigste, [z, 'E', k, s, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @]).
entry(sechsunddreißig, [z, 'E', k, s, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(sechsundfünf, [z, 'E', k, s, 'U', n, t, f, 'Y', n, f]).
entry(sechsundfünfzig, [z, 'E', k, s, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(sechsundneunzig, [z, 'E', k, s, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(sechsundvierzigsten, [z, 'E', k, s, 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(sechsundvierzig, [z, 'E', k, s, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(sechsund, [z, 'E', k, s, 'U', n, t]).
entry(sechsundzwanzigsten, [z, 'E', k, s, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(sechsundzwanzigster, [z, 'E', k, s, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(sechsundzwanzigste, [z, 'E', k, s, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(sechsundzwanzig, [z, 'E', k, s, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(sechs, [z, 'E', k, s]).
entry(sech, [z, 'E', 'C']).
entry(sechzehntem, [z, 'E', 'C', ts, 'e:', n, t, @, m]).
entry(sechzehnten, [z, 'E', 'C', ts, 'e:', n, t, @, n]).
entry(sechzehnter, [z, 'E', 'C', ts, 'e:', n, t, '6']).
entry(sechzehnte, [z, 'E', 'C', ts, 'e:', n, t, @]).
entry(sechzehn, [z, 'E', 'C', ts, 'e:', n]).
entry(sechzig, [z, 'E', 'C', ts, 'I', 'C']).
entry(sechz, [z, 'E', 'C', ts]).
entry('Sedlmayer', [z, 'e:', d, l, m, aI, '6']).
entry('See', [z, 'e:']).
entry(segeln, [z, 'e:', g, @, l, n]).
entry(seg, [z, 'e:', k]).
entry('Sehenswürdigkeiten', [z, 'e:', @, n, s, v, 'Y', '6', d, 'I', 'C', k, aI, t, @, n]).
entry('Sehenswürdigkeit', [z, 'e:', @, n, s, v, 'Y', '6', d, 'I', 'C', k, aI, t]).
entry(sehen, [z, 'e:', @, n]).
entry(sehe, [z, 'e:', @]).
entry(sehr, [z, 'e:', '6']).
entry(seien, [z, aI, @, n]).
entry(seinem, [z, aI, n, @, m]).
entry(seinen, [z, aI, n, @, n]).
entry(seiner, [z, aI, n, '6']).
entry(seine, [z, aI, n, @]).
entry(sein, [z, aI, n]).
entry(seitdem, [z, aI, t, d, 'e:', m]).
entry('Seite', [z, aI, t, @]).
entry(seit, [z, aI, t]).
entry(sei, [z, aI]).
entry('Sekretärinnen', [z, e, k, r, e, t, 'E:', r, 'I', n, @, n]).
entry('Sekretärin', [z, e, k, r, e, t, 'E:', r, 'I', n]).
entry('Sekretär', [z, e, k, r, e, t, 'E:', '6']).
entry('Sekre', [z, e, k, r, e]).
entry('Sektion', [z, 'E', k, ts, j, 'o:', n]).
entry('Sekt', [z, 'E', k, t]).
entry('Sekunde', [z, e, k, 'U', n, d, @]).
entry(selben, [z, 'E', l, b, @, n]).
entry(selber, [z, 'E', l, b, '6']).
entry(selbe, [z, 'E', l, b, @]).
entry(selbstverständlich, [z, 'E', l, p, s, t, f, 'E', '6', 'S', t, 'E', n, t, l, 'I', 'C']).
entry(selbst, [z, 'E', l, p, s, t]).
entry(seltenes, [z, 'E', l, t, @, n, @, s]).
entry(selten, [z, 'E', l, t, @, n]).
entry(sel, [z, 'E', l]).
entry('Semester', [z, e, m, 'E', s, t, '6']).
entry('Seminaren', [z, e, m, i, n, 'a:', r, @, n]).
entry('Seminare', [z, e, m, i, n, 'a:', r, @]).
entry('Seminarhaus', [z, e, m, i, n, 'a:', r, h, aU, s]).
entry(seminarmäßig, [z, e, m, i, n, 'a:', r, m, 'E:', s, 'I', 'C']).
entry('Seminarräume', [z, e, m, i, n, 'a:', r, r, 'OY', m, @]).
entry('Seminarraum', [z, e, m, i, n, 'a:', r, r, aU, m]).
entry('Seminars', [z, e, m, i, n, 'a:', r, s]).
entry('Seminarvortrag', [z, e, m, i, n, 'a:', r, f, 'o:', '6', t, r, 'a:', k]).
entry(seminar, [z, e, m, i, n, 'a:', r]).
entry('Semmeln', [z, 'E', m, @, l, n]).
entry('Semptember', [z, 'E', m, p, t, 'E', m, b, '6']).
entry(senden, [z, 'E', n, d, @, n]).
entry('Sendung', [z, 'E', n, d, 'U', 'N']).
entry(separat, [z, e, p, a, r, 'a:', t]).
entry('Septembers', [z, 'E', p, t, 'E', m, b, '6', s]).
entry('Septembertage', [z, 'E', p, t, 'E', m, b, '6', t, 'a:', g, @]).
entry('Septemberwoche', [z, 'E', p, t, 'E', m, b, '6', v, 'O', x, @]).
entry('September', [z, 'E', p, t, 'E', m, b, '6']).
entry('Septem', [z, 'E', p, t, 'E', m]).
entry('Sept', [z, 'E', p, t]).
entry('Sep', [z, 'E', p]).
entry('Serviceleistung', [z, '2:', '6', v, 'I', s, l, aI, s, t, 'U', 'N']).
entry('Service', [s, '9', '6', v, 'I', s]).
entry(serviert, [z, 'E', '6', v, 'i:', '6', t]).
entry(servus, [s, 'E', '6', v, 'U', s]).
entry(ser, [z, 'E', '6']).
entry(setzen, [z, 'E', ts, @, n]).
entry(setze, [z, 'E', ts, @]).
entry(setzten, [z, 'E', ts, t, @, n]).
entry(setz, [z, 'E', ts]).
entry(se, [z, @]).
entry('Shakespeare-in-Love', ['S', 'e:', k, s, p, 'I', '6', 'I', n, l, a, f]).
entry('Shakespeare-Zyklus', ['S', 'e:', k, s, p, 'I', '6', ts, 'y:', k, l, 'U', s]).
entry('Shay', ['S', e, 'I']).
entry('Sheraton', ['S', 'E', r, @, t, @, n]).
entry('Sherry', ['S', 'E', r, i]).
entry('Shetland', ['S', 'E', t, l, @, n, t]).
entry(shimasu, ['S', i, m, a, z, u]).
entry(shit, ['S', 'I', t]).
entry(shuttle, ['S', a, t, l]).
entry(sicherer, [z, 'I', 'C', @, r, '6']).
entry(sichergestellt, [z, 'I', 'C', '6', g, @, 'S', t, 'E', l, t]).
entry(sicherheitshalber, [z, 'I', 'C', '6', h, aI, ts, h, a, l, b, '6']).
entry('Sicherheit', [z, 'I', 'C', '6', h, aI, t]).
entry(sicherlich, [z, 'I', 'C', '6', l, 'I', 'C']).
entry(sicher, [z, 'I', 'C', '6']).
entry(sicherzugehen, [z, 'I', 'C', '6', ts, u, g, 'e:', @, n]).
entry(sicherzustellen, [z, 'I', 'C', '6', ts, u, 'S', t, 'E', l, @, n]).
entry(sicht, [z, 'I', 'C', t]).
entry(sich, [z, 'I', 'C']).
entry('Sidnuz', [z, 'i:', t, n, 'U', ts]).
entry(siebenhundert, [z, 'i:', b, @, n, h, 'U', n, d, '6', t]).
entry(siebenten, [z, 'i:', b, @, n, t, @, n]).
entry(siebenter, [z, 'i:', b, @, n, t, '6']).
entry(siebente, [z, 'i:', b, @, n, t, @]).
entry('Sieben-Uhr-fünfzehn-Flieger', [z, 'i:', b, @, n, 'u:', '6', f, 'Y', n, f, z, 'e:', n, f, l, 'i:', g, '6']).
entry(siebenunddreißigsten, [z, 'i:', b, @, n, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(siebenundfünfzig, [z, 'i:', b, @, n, 'U', n, t, f, 'Y', n, f, ts, 'I', 'C']).
entry(siebenundneunzig, [z, 'i:', b, @, n, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(siebenundsiebzig, [z, 'i:', b, @, n, 'U', n, t, z, 'i:', p, ts, 'I', 'C']).
entry(siebenundvierzigsten, [z, 'i:', b, @, n, 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(siebenundvierzigste, [z, 'i:', b, @, n, 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @]).
entry(siebenundvierzig, [z, 'i:', b, @, n, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(siebenundzwan, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n]).
entry(siebenundzwanzigstem, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, m]).
entry(siebenundzwanzigsten, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(siebenundzwanzigster, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(siebenundzwanzigste, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(siebenundzwanzig, [z, 'i:', b, @, n, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(siebenundzwa, [z, 'i:', b, @, n, 'U', n, t, ts, v, a]).
entry(siebenundz, [z, 'i:', b, @, n, 'U', n, t, ts]).
entry(sieben, [z, 'i:', b, @, n]).
entry(siebten, [z, 'i:', p, t, @, n]).
entry(siebter, [z, 'i:', p, t, '6']).
entry(siebte, [z, 'i:', p, t, @]).
entry(siebzehnten, [z, 'i:', p, ts, 'e:', n, t, @, n]).
entry(siebzehnter, [z, 'i:', p, ts, 'e:', n, t, '6']).
entry(siebzehnte, [z, 'i:', p, ts, 'e:', n, t, @]).
entry(siebzehnt, [z, 'i:', p, ts, 'e:', n, t]).
entry(siebzehn, [z, 'i:', p, ts, 'e:', n]).
entry(siebze, [z, 'i:', p, ts, e]).
entry(siebzig, [z, 'i:', p, ts, 'I', 'C']).
entry(sieb, [z, 'i:', p]).
entry(siebz, [z, 'i:', p, ts]).
entry(siedendheiß, [z, 'i:', d, @, n, t, h, aI, s]).
entry('Siegen', [z, 'i:', g, @, n]).
entry('Siegfried', [z, 'i:', k, f, r, 'i:', t]).
entry(siehst, [z, 'i:', s, t]).
entry(sieht, [z, 'i:', t]).
entry('Siemens', [z, 'i:', m, @, n, s]).
entry('Siepmann', [z, 'i:', p, m, a, n]).
entry('Sievert', [z, 'i:', v, '6', t]).
entry(sie, [z, 'i:']).
entry(sightseeing, [s, aI, ts, 'i:', 'I', 'N']).
entry('Sightseeing', [s, aI, ts, 'i:', 'I', n, g]).
entry('Silberne-Habicht', [z, 'I', l, b, '6', n, @, h, 'a:', b, 'I', 'C', t]).
entry('Silbernen-Habicht', [z, 'I', l, b, '6', n, @, n, h, 'a:', b, 'I', 'C', t]).
entry('Silberwald', [z, 'I', l, b, '6', v, a, l, t]).
entry('Silke', [z, 'I', l, k, @]).
entry('Silvester', [z, 'I', l, v, 'E', s, t, '6']).
entry('Simons', [z, 'i:', m, 'O', n, s]).
entry(sind, [z, 'I', n, t]).
entry('Singen', [z, 'I', 'N', @, n]).
entry('Singer', [z, 'I', 'N', '6']).
entry(sinke, [z, 'I', 'N', k, @]).
entry('Sinne', [z, 'I', n, @]).
entry(sinnig, [z, 'I', n, 'I', 'C']).
entry(sinnlos, [z, 'I', n, l, 'o:', s]).
entry(sinnvollerweise, [z, 'I', n, f, 'O', l, '6', v, aI, z, @]).
entry(sinnvoller, [z, 'I', n, f, 'O', l, '6']).
entry(sinnvollsten, [z, 'I', n, f, 'O', l, s, t, @, n]).
entry(sinnvoll, [z, 'I', n, f, 'O', l]).
entry('Sinn', [z, 'I', n]).
entry('Situation', [z, i, t, u, a, ts, j, 'o:', n]).
entry(situell, [z, i, t, u, 'E', l]).
entry(sitzen, [z, 'I', ts, @, n]).
entry(sitze, [z, 'I', ts, @]).
entry('Sitzplätze', [z, 'I', ts, p, l, 'E', ts, @]).
entry('Sitzplatzreservierung', [z, 'I', ts, p, l, a, ts, r, 'e:', z, @, r, v, 'i:', r, 'U', 'N']).
entry('Sitzplatz', [z, 'I', ts, p, l, a, ts]).
entry(sitzt, [z, 'I', ts, t]).
entry('Sitzungen', [z, 'I', ts, 'U', 'N', @, n]).
entry('Sitzung', [z, 'I', ts, 'U', 'N']).
entry(si, [z, 'I']).
entry('Sizilien', [z, i, ts, 'i:', l, j, @, n]).
entry(skeptisch, [s, k, 'E', p, t, 'I', 'S']).
entry('Skifahren', ['S', 'i:', f, 'a:', r, @, n]).
entry('Skiurlaub', ['S', 'i:', 'u:', '6', l, aU, p]).
entry('Sloboda', [s, l, o, b, o, d, a]).
entry(slot, [s, l, 'O', t]).
entry('Smith', [s, m, 'I', f]).
entry('Snack', [s, n, 'E', k]).
entry('Snobs', [s, n, 'O', p, s]).
entry(sn, [s, n]).
entry(sobald, [z, o, b, a, l, t]).
entry(soeben, [z, o, 'e:', b, @, n]).
entry(sofern, [z, o, f, 'E', '6', n]).
entry(sofort, [z, o, f, 'O', '6', t]).
entry('Softwaretechnik', [s, 'O', f, t, v, 'e:', '6', t, 'E', 'C', n, 'I', k]).
entry(sogar, [z, o, g, 'a:', r]).
entry(sogleich, [z, o, g, l, aI, 'C']).
entry(solange, [z, o, l, a, 'N', @]).
entry('Solarium', [z, o, l, 'a:', r, j, 'U', m]).
entry(solchen, [z, 'O', l, 'C', @, n]).
entry(solches, [z, 'O', l, 'C', @, s]).
entry(solche, [z, 'O', l, 'C', @]).
entry(sollen, [z, 'O', l, @, n]).
entry(sollten, [z, 'O', l, t, @, n]).
entry(solltest, [z, 'O', l, t, @, s, t]).
entry(sollte, [z, 'O', l, t, @]).
entry(sollt, [z, 'O', l, t]).
entry(soll, [z, 'O', l]).
entry('Sölter', [z, '9', l, t, '6']).
entry(somit, [z, o, m, 'I', t]).
entry('Somman', [z, 'O', m, a, n]).
entry('Sommerabend', [z, 'O', m, '6', 'a:', b, @, n, t]).
entry('Sommerferien', [z, 'O', m, '6', f, 'e:', '6', j, @, n]).
entry('Sommermonaten', [z, 'O', m, '6', m, 'o:', n, a, t, @, n]).
entry('Sommerschule', [z, 'O', m, '6', 'S', 'u:', l, @]).
entry('Sommerschulung', [z, 'O', m, '6', 'S', 'u:', l, 'U', 'N']).
entry('Sommers', [z, 'O', m, '6', s]).
entry('Sommertagen', [z, 'O', m, '6', t, 'a:', g, @, n]).
entry('Sommerurlaub', [z, 'O', m, '6', 'u:', '6', l, aU, p]).
entry('Sommerzeit', [z, 'O', m, '6', ts, aI, t]).
entry('Sommer', [z, 'O', m, '6']).
entry('Sonderausstellung', [z, 'O', n, d, '6', aU, s, 'S', t, 'E', l, 'U', 'N']).
entry(sonderlich, [z, 'O', n, d, '6', l, 'I', 'C']).
entry(sondern, [z, 'O', n, d, '6', n]).
entry('Sonderpreis', [z, 'O', n, d, '6', p, r, aI, s]).
entry('Sonderreisen', [z, 'O', n, d, '6', r, aI, z, @, n]).
entry('Sonderspartarif', [z, 'O', n, d, '6', 'S', p, 'a:', r, t, a, r, 'i:', f]).
entry('Sönksen', [s, '9', 'N', k, s, @, n]).
entry('Sonnabend', [z, 'O', n, 'a:', b, @, n, t]).
entry('Sonnenschein', [z, 'O', n, @, n, 'S', aI, n]).
entry('Sonnenstrahlen', [z, 'O', n, @, n, 'S', t, r, 'a:', l, @, n]).
entry(sonne, [z, 'O', n, @]).
entry('Sonntagen', [z, 'O', n, t, 'a:', g, @, n]).
entry('Sonntage', [z, 'O', n, t, 'a:', g, @]).
entry(sonntags, [z, 'O', n, t, 'a:', k, s]).
entry(sonntag, [z, 'O', n, t, 'a:', k]).
entry('Sonnta', [z, 'O', n, t, 'a:']).
entry('Sonnt', [z, 'O', n, t]).
entry(sonoren, [z, o, n, 'o:', r, @, n]).
entry(sonstigen, [z, 'O', n, s, t, 'I', g, @, n]).
entry(sonstiges, [z, 'O', n, s, t, 'I', g, @, s]).
entry(sonstige, [z, 'O', n, s, t, 'I', g, @]).
entry(sonst, [z, 'O', n, s, t]).
entry('Sonts', [z, 'O', n, ts]).
entry(son, [z, 'O', n]).
entry('Sorgenkind', [z, 'O', '6', g, @, n, k, 'I', n, t]).
entry(sorgen, [z, 'O', '6', g, @, n]).
entry(sorge, [z, 'O', '6', g, @]).
entry(sorry, [s, 'O', r, i]).
entry('Sorte', [z, 'O', '6', t, @]).
entry(soso, [z, o, z, 'o:']).
entry(soundso, [z, 'o:', 'U', n, t, z, 'o:']).
entry(soviel, [z, o, f, 'i:', l]).
entry(sowas, [z, o, v, a, s]).
entry(soweit, [z, o, v, aI, t]).
entry(sowieso, [z, 'o:', v, 'i:', z, 'o:']).
entry(sowie, [z, o, v, 'i:']).
entry(sowohl, [z, 'o:', v, 'o:', l]).
entry(so, [z, 'o:']).
entry('So', [z, 'O']).
entry(sozusagen, [z, 'o:', ts, u, z, 'a:', g, @, n]).
entry(spannend, ['S', p, a, n, @, n, t]).
entry('Spanne', ['S', p, a, n, @]).
entry(sparen, ['S', p, 'a:', r, @, n]).
entry(sparsamer, ['S', p, 'a:', r, z, 'a:', m, '6']).
entry(spart, ['S', p, 'a:', r, t]).
entry(spaßiger, ['S', p, 'a:', s, 'I', g, '6']).
entry('Spaß', ['S', p, 'a:', s]).
entry(spätabends, ['S', p, 'E:', t, 'a:', b, @, n, ts]).
entry(späten, ['S', p, 'E:', t, @, n]).
entry(späteren, ['S', p, 'E:', t, @, r, @, n]).
entry(spätere, ['S', p, 'E:', t, @, r, @]).
entry(späteres, ['S', p, 'E:', t, @, r, @, s]).
entry(später, ['S', p, 'E:', t, '6']).
entry(spätes, ['S', p, 'E:', t, @, s]).
entry(spätesten, ['S', p, 'E:', t, @, s, t, @, n]).
entry(spätestens, ['S', p, 'E:', t, @, s, t, @, n, s]).
entry(spätester, ['S', p, 'E:', t, @, s, t, '6']).
entry(späteste, ['S', p, 'E:', t, @, s, t, @]).
entry('Spätnachmittag', ['S', p, 'E:', t, n, 'a:', x, m, 'I', t, 'a:', k]).
entry('Spätnachmittags', ['S', p, 'E:', t, n, 'a:', x, m, 'I', t, 'a:', k, s]).
entry(spät, ['S', p, 'E:', t]).
entry(spazierengehen, ['S', p, a, ts, 'i:', r, @, n, g, 'e:', @, n]).
entry('Spaziergang', ['S', p, a, ts, 'i:', '6', g, a, 'N']).
entry('Specht', ['S', p, 'E', 'C', t]).
entry(special, [s, p, e, 'S', @, l]).
entry('Speisen', ['S', p, aI, z, @, n]).
entry('Speisewagen', ['S', p, aI, z, @, v, 'a:', g, @, n]).
entry(spekuliere, ['S', p, e, k, u, l, 'i:', r, @]).
entry('Spesenkosten-Abrechnung', ['S', p, 'e:', z, @, n, k, 'O', s, t, @, n, a, p, r, 'E', 'C', n, 'U', 'N']).
entry(spesenmäßig, ['S', p, 'e:', z, @, n, m, 'E:', s, 'I', 'C']).
entry('Spesenrechnung', ['S', p, 'e:', z, @, n, r, 'E', 'C', n, 'U', 'N']).
entry('Spesen', ['S', p, 'e:', z, @, n]).
entry('Speyer', ['S', p, aI, '6']).
entry('Spezialtip', ['S', p, e, ts, j, 'a:', l, t, 'I', p]).
entry(speziellen, ['S', p, e, ts, j, 'E', l, @, n]).
entry(spezielle, ['S', p, e, ts, j, 'E', l, @]).
entry('Spezielles', ['S', p, e, ts, j, 'E', l, @, s]).
entry(speziell, ['S', p, e, ts, j, 'E', l]).
entry(spezifischen, ['S', p, e, ts, 'i:', f, 'I', 'S', @, n]).
entry(spezifizieren, ['S', p, e, ts, i, f, i, ts, 'i:', r, @, n]).
entry(spielen, ['S', p, 'i:', l, @, n]).
entry(spiele, ['S', p, 'i:', l, @]).
entry('Spielhoven', ['S', p, 'i:', l, h, 'o:', v, @, n]).
entry('Spielplan', ['S', p, 'i:', l, p, l, 'a:', n]).
entry('Spielraum', ['S', p, 'i:', l, r, aU, m]).
entry(spiel, ['S', p, 'i:', l]).
entry(spielst, ['S', p, 'i:', l, s, t]).
entry(spielt, ['S', p, 'i:', l, t]).
entry('Spitze', ['S', p, 'I', ts, @]).
entry(spontan, ['S', p, 'O', n, t, 'a:', n]).
entry(sportbegeistert, ['S', p, 'O', '6', t, b, @, g, aI, s, t, '6', t]).
entry(sportliche, ['S', p, 'O', '6', t, l, 'I', 'C', @]).
entry(sportlich, ['S', p, 'O', '6', t, l, 'I', 'C']).
entry('Sportmöglichkeit', ['S', p, 'O', '6', t, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Sportplatz', ['S', p, 'O', '6', t, p, l, a, ts]).
entry('Sportschau', ['S', p, 'O', '6', tS, aU]).
entry('Sport', ['S', p, 'O', '6', t]).
entry('Sportveranstaltungen', ['S', p, 'O', '6', t, f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', @, n]).
entry(sprachen, ['S', p, r, 'a:', x, @, n]).
entry('Spracherkennung', ['S', p, r, 'a:', x, 'E', '6', k, 'E', n, 'U', 'N']).
entry(sprache, ['S', p, r, 'a:', x, @]).
entry('Sprachspende', ['S', p, r, 'a:', x, 'S', p, 'E', n, d, @]).
entry(sprach, ['S', p, r, 'a:', x]).
entry(sprechen, ['S', p, r, 'E', 'C', @, n]).
entry(spreche, ['S', p, r, 'E', 'C', @]).
entry(sprech, ['S', p, r, 'E', 'C']).
entry('Sprengel-Museum', ['S', p, r, 'E', 'N', @, l, m, u, z, 'e:', 'U', m]).
entry(sprengt, ['S', p, r, 'E', 'N', t]).
entry(spre, ['S', p, r, 'E']).
entry(sprich, ['S', p, r, 'I', 'C']).
entry(spricht, ['S', p, r, 'I', 'C', t]).
entry(springen, ['S', p, r, 'I', 'N', @, n]).
entry(springt, ['S', p, r, 'I', 'N', t]).
entry('Sprünge', ['S', p, r, 'Y', 'N', @]).
entry(sp, ['S', p]).
entry('Spürsinn', ['S', p, 'y:', '6', z, 'I', n]).
entry(sputen, ['S', p, 'u:', t, @, n]).
entry(s, [s]).
entry('Staaten', ['S', t, 'a:', t, @, n]).
entry('Staatsoper', ['S', t, 'a:', ts, 'o:', p, '6']).
entry('Stab', ['S', t, 'a:', p]).
entry('Stadtbesichtigung', ['S', t, a, t, b, @, z, 'I', 'C', t, 'I', g, 'U', 'N']).
entry('Stadtbroschüre', ['S', t, a, t, b, r, 'O', 'S', 'y:', r, @]).
entry('Städte', ['S', t, 'E:', t, @]).
entry('Stadtfest', ['S', t, a, t, f, 'E', s, t]).
entry('Stadtführer', ['S', t, a, t, f, 'y:', r, '6']).
entry('Stadthotel', ['S', t, a, t, h, o, t, 'E', l]).
entry(städtische, ['S', t, 'E', t, 'I', 'S', @]).
entry('Stadtmitte', ['S', t, a, t, m, 'I', t, @]).
entry('Stadtnähe', ['S', t, a, t, n, 'E:', @]).
entry(stadtnah, ['S', t, a, t, n, 'a:']).
entry('Stadtpark', ['S', t, a, t, p, a, r, k]).
entry('Stadtplanes', ['S', t, a, t, p, l, 'a:', n, @, s]).
entry('Stadtplan', ['S', t, a, t, p, l, 'a:', n]).
entry('Stadtprogramm', ['S', t, a, t, p, r, o, g, r, a, m]).
entry('Stadtrundfahrten', ['S', t, a, t, r, 'U', n, t, f, 'a:', r, t, @, n]).
entry('Stadtrundfahrt', ['S', t, a, t, r, 'U', n, t, f, 'a:', r, t]).
entry('Stadtrundgang', ['S', t, a, t, r, 'U', n, t, g, a, 'N']).
entry('Stadt', ['S', t, a, t]).
entry('Stadttheater', ['S', t, a, t, t, e, 'a:', t, '6']).
entry('Stadtzentrum', ['S', t, a, t, ts, 'E', n, t, r, 'U', m]).
entry(stag, [s, t, 'a:', k]).
entry('Stairway-Queen', [s, t, 'E', r, w, e, 'I', k, w, 'i:', n]).
entry('Stammstrecke', ['S', t, a, m, 'S', t, r, 'E', k, @]).
entry('Standardpreis', ['S', t, a, n, d, a, r, t, p, r, aI, s]).
entry('Standard', ['S', t, a, n, d, a, r, t]).
entry(standen, ['S', t, a, n, d, @, n]).
entry(ständen, ['S', t, 'E', n, d, @, n]).
entry(standesgemäß, ['S', t, a, n, d, @, s, g, @, m, 'E:', s]).
entry(stände, ['S', t, 'E', n, d, @]).
entry(ständig, ['S', t, 'E', n, d, 'I', 'C']).
entry(stand, ['S', t, a, n, t]).
entry('Standvorbereiten', ['S', t, a, n, t, f, 'o:', '6', b, @, r, aI, t, @, n]).
entry(starke, ['S', t, a, r, k, @]).
entry(stark, ['S', t, a, r, k]).
entry('Starlight-Express', [s, t, 'a:', r, l, aI, t, 'E', k, s, p, r, 'E', s]).
entry('Starnberg', ['S', t, a, r, n, b, 'E', '6', k]).
entry('Starr', ['S', t, a, r]).
entry('Star', ['S', t, 'a:', r]).
entry(starten, ['S', t, a, r, t, @, n]).
entry(startet, ['S', t, a, r, t, @, t]).
entry('Startpunkt', ['S', t, a, r, t, p, 'U', 'N', k, t]).
entry('Statement', [s, t, 'e:', t, m, @, n, t]).
entry('Stationen', ['S', t, a, ts, j, 'o:', n, @, n]).
entry(stattfindenden, ['S', t, a, t, f, 'I', n, d, @, n, d, @, n]).
entry(stattfinden, ['S', t, a, t, f, 'I', n, d, @, n]).
entry(stattfindet, ['S', t, a, t, f, 'I', n, d, @, t]).
entry(stattgefunden, ['S', t, a, t, g, @, f, 'U', n, d, @, n]).
entry('Staus', ['S', t, aU, s]).
entry('Stau', ['S', t, aU]).
entry(stecken, ['S', t, 'E', k, @, n]).
entry(steckt, ['S', t, 'E', k, t]).
entry('Stefanie', ['S', t, 'E', f, a, n, i]).
entry('Steffes', ['S', t, 'E', f, @, s]).
entry(stehenden, ['S', t, 'e:', @, n, d, @, n]).
entry(stehen, ['S', t, 'e:', @, n]).
entry(stehe, ['S', t, 'e:', @]).
entry(steht, ['S', t, 'e:', t]).
entry(steigen, ['S', t, aI, g, @, n]).
entry(steige, ['S', t, aI, g, @]).
entry(steigt, ['S', t, aI, k, t]).
entry('Steinhuder-Meer', ['S', t, aI, n, h, 'u:', d, '6', m, 'e:', '6']).
entry('Steinlen', ['S', t, aI, n, l, @, n]).
entry('Steinmetz', ['S', t, aI, n, m, 'E', ts]).
entry(stellen, ['S', t, 'E', l, @, n]).
entry(stelle, ['S', t, 'E', l, @]).
entry(stellt, ['S', t, 'E', l, t]).
entry(sten, [s, t, @, n]).
entry('Sterne', ['S', t, 'E', '6', n, @]).
entry('Stern', ['S', t, 'E', '6', n]).
entry(ste, ['S', t, e]).
entry('Steuerberater', ['S', t, 'OY', '6', b, @, r, 'a:', t, '6']).
entry(stich, ['S', t, 'I', 'C']).
entry('Stichwort', ['S', t, 'I', 'C', v, 'O', '6', t]).
entry(stickigen, ['S', t, 'I', k, 'I', g, @, n]).
entry('Stiefelhagen', ['S', t, 'i:', f, @, l, h, 'a:', g, @, n]).
entry(stiegen, ['S', t, 'i:', g, @, n]).
entry('Stift', ['S', t, 'I', f, t]).
entry(stimmen, ['S', t, 'I', m, @, n]).
entry(stimme, ['S', t, 'I', m, @]).
entry(stimmt, ['S', t, 'I', m, t]).
entry('Stimmung', ['S', t, 'I', m, 'U', 'N']).
entry(stinklangweilig, ['S', t, 'I', 'N', k, l, a, 'N', v, aI, l, 'I', 'C']).
entry('Stober', ['S', t, 'o:', b, '6']).
entry('Stockholm', ['S', t, 'O', k, h, 'O', l, m]).
entry('Stock', ['S', t, 'O', k]).
entry('Stockwerken', ['S', t, 'O', k, v, 'E', '6', k, @, n]).
entry('Stockwerke', ['S', t, 'O', k, v, 'E', '6', k, @]).
entry('Stoffauswahl', ['S', t, 'O', f, aU, s, v, 'a:', l]).
entry('Stoff', ['S', t, 'O', f]).
entry('Stolle', ['S', t, 'O', l, @]).
entry(stop, ['S', t, 'O', p]).
entry(störend, ['S', t, '2:', r, @, n, t]).
entry(stören, ['S', t, '2:', r, @, n]).
entry(stornieren, ['S', t, 'O', '6', n, 'i:', r, @, n]).
entry(stör, ['S', t, '2:', '6']).
entry(stört, ['S', t, '2:', '6', t]).
entry('Störung', ['S', t, '2:', r, 'U', 'N']).
entry('Stoßzeiten', ['S', t, 'o:', s, ts, aI, t, @, n]).
entry('Strack', ['S', t, r, a, k]).
entry(straffer, ['S', t, r, a, f, '6']).
entry(straff, ['S', t, r, a, f]).
entry('Stralsund', ['S', t, r, 'a:', l, z, 'U', n, t]).
entry('Strandvilla', ['S', t, r, a, n, t, v, 'I', l, a]).
entry('Stränge', ['S', t, r, 'E', 'N', @]).
entry(strapazieren, ['S', t, r, a, p, a, ts, 'i:', r, @, n]).
entry(strapaziös, ['S', t, r, a, p, a, ts, j, '2:', s]).
entry('Straßenbahn', ['S', t, r, 'a:', s, @, n, b, 'a:', n]).
entry('Straße', ['S', t, r, 'a:', s, @]).
entry('Sträßner', ['S', t, r, 'E', s, n, '6']).
entry('Strassburg', ['S', t, r, 'a:', s, b, 'U', '6', k]).
entry('Strategie', ['S', t, r, a, t, e, g, 'i:']).
entry('Strätz', ['S', t, r, 'E', ts]).
entry(sträuben, ['S', t, r, 'OY', b, @, n]).
entry(strecken, ['S', t, r, 'E', k, @, n]).
entry('Strecke', ['S', t, r, 'E', k, @]).
entry(streichen, ['S', t, r, aI, 'C', @, n]).
entry(streng, ['S', t, r, 'E', 'N']).
entry(streßfreier, ['S', t, r, 'E', s, f, r, aI, '6']).
entry(streßfrei, ['S', t, r, 'E', s, f, r, aI]).
entry(stressigen, ['S', t, r, 'E', s, 'I', g, @, n]).
entry(stressige, ['S', t, r, 'E', s, 'I', g, @]).
entry(stressig, ['S', t, r, 'E', s, 'I', 'C']).
entry('Streß', ['S', t, r, 'E', s]).
entry('Streßtag', ['S', t, r, 'E', s, t, 'a:', k]).
entry('Strich', ['S', t, r, 'I', 'C']).
entry('Strippe', ['S', t, r, 'I', p, @]).
entry('Strohbusch', ['S', t, r, 'o:', b, 'U', 'S']).
entry('Strohschnieder', ['S', t, r, 'o:', 'S', n, 'i:', d, '6']).
entry('Strom', ['S', t, r, 'o:', m]).
entry(strukturiert, ['S', t, r, 'U', k, t, u, r, 'i:', '6', t]).
entry('Stückeleien', ['S', t, 'Y', k, @, l, aI, @, n]).
entry('Stücke', ['S', t, 'Y', k, @]).
entry('Stück', ['S', t, 'Y', k]).
entry('Studenten', ['S', t, u, d, 'E', n, t, @, n]).
entry('Studienfach', ['S', t, 'u:', d, j, @, n, f, a, x]).
entry('Studienfreund', ['S', t, 'u:', d, j, @, n, f, r, 'OY', n, t]).
entry('Studienkollegen', ['S', t, 'u:', d, j, @, n, k, 'O', l, 'e:', g, @, n]).
entry('Studienplan', ['S', t, 'u:', d, j, @, n, p, l, 'a:', n]).
entry(studiere, ['S', t, u, d, 'i:', r, @]).
entry('Studio', ['S', t, 'u:', d, j, o]).
entry('Stuhl', ['S', t, 'u:', l]).
entry('Stumpfecker', ['S', t, 'U', m, pf, 'E', k, '6']).
entry('Stumpfegger', ['S', t, 'U', m, pf, 'E', g, '6']).
entry('Stündchen', ['S', t, 'Y', n, t, 'C', @, n]).
entry(stundenlang, ['S', t, 'U', n, d, @, n, l, a, 'N']).
entry(stundenplan, ['S', t, 'U', n, d, @, n, p, l, 'a:', n]).
entry('Stunden', ['S', t, 'U', n, d, @, n]).
entry(stünden, ['S', t, 'Y', n, d, @, n]).
entry('Stundentakt', ['S', t, 'U', n, d, @, n, t, a, k, t]).
entry(stundenweise, ['S', t, 'U', n, d, @, n, v, aI, z, @]).
entry('Stunde', ['S', t, 'U', n, d, @]).
entry(stünde, ['S', t, 'Y', n, d, @]).
entry(stündliche, ['S', t, 'Y', n, t, l, 'I', 'C', @]).
entry(stündlich, ['S', t, 'Y', n, t, l, 'I', 'C']).
entry(stündli, ['S', t, 'Y', n, t, l, 'I']).
entry(stündl, ['S', t, 'Y', n, t, l]).
entry('Stünd', ['S', t, 'Y', n, t]).
entry('Stürme', ['S', t, 'Y', '6', m, @]).
entry(stur, ['S', t, 'u:', '6']).
entry(stürzen, ['S', t, 'Y', '6', ts, @, n]).
entry(stu, ['S', t, u]).
entry('Stu', ['S', t, 'U']).
entry(stüt, ['S', t, 'Y', t]).
entry('Stuttgarter', ['S', t, 'U', t, g, a, r, t, '6']).
entry('Stuttgart', ['S', t, 'U', t, g, a, r, t]).
entry(suchen, [z, 'u:', x, @, n]).
entry(suche, [z, 'u:', x, @]).
entry('Süchtigen', [z, 'Y', 'C', t, 'I', g, @, n]).
entry(sucht, [z, 'u:', x, t]).
entry(such, [z, 'u:', x]).
entry(südamerikanisch, [z, 'y:', t, a, m, e, r, i, k, 'a:', n, 'I', 'S']).
entry('Süden', [z, 'y:', d, @, n]).
entry('Südfrankreich', [z, 'y:', t, f, r, a, 'N', k, r, aI, 'C']).
entry(südlicher, [z, 'y:', t, l, 'I', 'C', '6']).
entry('Sudnig', [z, 'u:', d, n, 'I', 'C']).
entry('Sudniz', [z, 'u:', t, n, 'I', ts]).
entry('Suite', [s, v, 'i:', t, @]).
entry(sumimaseN, [z, 'u:', m, i, m, 'a:', z, @, n]).
entry('Summers', [s, a, m, '6', s]).
entry(supergute, [z, 'u:', p, '6', g, 'u:', t, @]).
entry('Super-Kino', [z, 'u:', p, '6', k, 'i:', n, o]).
entry(super, [z, 'u:', p, '6']).
entry('Surfen', [s, '9', '6', f, @, n]).
entry('Susanne', [z, u, z, a, n, @]).
entry('Susen', [z, 'u:', z, @, n]).
entry('Suska', [z, 'U', s, k, a]).
entry('Sus', [z, 'u:', s]).
entry('Süß', [z, 'y:', s]).
entry('Suzuki', [z, u, z, 'u:', k, i]).
entry('Swimmingpool', [s, v, 'I', m, 'I', 'N', p, 'u:', l]).
entry('Sylvesterraketen', [s, 'I', l, v, 'E', s, t, '6', r, a, k, 'e:', t, @, n]).
entry(sympathischer, [z, 'Y', m, p, 'a:', t, 'I', 'S', '6']).
entry(sympathische, [z, 'Y', m, p, 'a:', t, 'I', 'S', @]).
entry(sympathisch, [z, 'Y', m, p, 'a:', t, 'I', 'S']).
entry(systematisch, [z, 'Y', s, t, e, m, 'a:', t, 'I', 'S']).
entry('Systemen', [z, 'Y', s, t, 'e:', m, @, n]).
entry('Systeme', [z, 'Y', s, t, 'e:', m, @]).
entry('S', [z]).
entry('Tabelle', [t, a, b, 'E', l, @]).
entry('Tablee', [t, a, b, l, 'e:']).
entry(table, [t, 'e:', b, @, l]).
entry('Tabu', [t, a, b, 'u:']).
entry(tädiges, [t, 'E:', d, 'I', g, @, s]).
entry('Tagebuch', [t, 'a:', g, @, b, 'u:', x]).
entry(tagen, [t, 'a:', g, @, n]).
entry('Tagesbesuch', [t, 'a:', g, @, s, b, @, z, 'u:', x]).
entry('Tagesgeschehen', [t, 'a:', g, @, s, g, @, 'S', 'e:', @, n]).
entry('Tagesordnung', [t, 'a:', g, @, s, 'O', '6', d, n, 'U', 'N']).
entry('Tages', [t, 'a:', g, @, s]).
entry('Tagestermin', [t, 'a:', g, @, s, t, 'E', '6', m, 'i:', n]).
entry('Tagestreffen', [t, 'a:', g, @, s, t, r, 'E', f, @, n]).
entry('Tagesverfassung', [t, 'a:', g, @, s, f, 'E', '6', f, a, s, 'U', 'N']).
entry('Tage', [t, 'a:', g, @]).
entry(tägigen, [t, 'E:', g, 'I', g, @, n]).
entry(tägiges, [t, 'E:', g, 'I', g, @, s]).
entry(tägige, [t, 'E:', g, 'I', g, @]).
entry(tägig, [t, 'E:', g, 'I', 'C']).
entry(tägin, [t, 'E:', g, 'I', n]).
entry(tägi, [t, 'E:', g, 'I']).
entry(täglich, [t, 'E:', k, l, 'I', 'C']).
entry(tagsüber, [t, 'a:', k, s, 'y:', b, '6']).
entry(tag, [t, 'a:', k]).
entry(tagt, [t, 'a:', k, t]).
entry('Tagungen', [t, 'a:', g, 'U', 'N', @, n]).
entry('Tagungsordnungspunkte', [t, 'a:', g, 'U', 'N', s, 'O', '6', d, n, 'U', 'N', s, p, 'U', 'N', k, t, @]).
entry('Tagungsort', [t, 'a:', g, 'U', 'N', s, 'O', '6', t]).
entry('Tagungsraum', [t, 'a:', g, 'U', 'N', s, r, aU, m]).
entry('Tagung', [t, 'a:', g, 'U', 'N']).
entry(tähiges, [t, 'E:', 'I', g, @, s]).
entry('Tai-Pen', [t, aI, p, 'E', n]).
entry('Taki', [t, 'a:', k, i]).
entry('Talinkalender', [t, 'a:', l, 'I', n, k, a, l, 'E', n, d, '6']).
entry('Tanaka', [t, a, n, a, k, a]).
entry(tangieren, [t, a, 'N', g, 'i:', r, @, n]).
entry('Tanja', [t, a, n, j, a]).
entry(tanke, [t, a, 'N', k, @]).
entry(tank, [t, a, 'N', k]).
entry(tanzen, [t, a, n, ts, @, n]).
entry(tanze, [t, a, n, ts, @]).
entry('Tarife', [t, a, r, 'i:', f, @]).
entry('Tarif', [t, a, r, 'i:', f]).
entry('Täßchen', [t, 'E', s, 'C', @, n]).
entry('Tasche', [t, a, 'S', @]).
entry('Tasse', [t, a, s, @]).
entry(ta, [t, a]).
entry(tä, [t, 'E']).
entry(täten, [t, 'E:', t, @, n]).
entry(täte, [t, 'E:', t, @]).
entry(tätigen, [t, 'E:', t, 'I', g, @, n]).
entry('Tätigkeiten', [t, 'E:', t, 'I', 'C', k, aI, t, @, n]).
entry('Tätigkeit', [t, 'E:', t, 'I', 'C', k, aI, t]).
entry(tätig, [t, 'E:', t, 'I', 'C']).
entry('Tatsache', [t, 'a:', t, z, a, x, @]).
entry(tatsächlich, [t, 'a:', t, z, 'E', 'C', l, 'I', 'C']).
entry(tätsstädte, [t, 'E:', ts, 'S', t, 'E', t, @]).
entry('Tat', [t, 'a:', t]).
entry(tät, [t, 'E:', t]).
entry(tauchen, [t, aU, x, @, n]).
entry(taufrisch, [t, aU, f, r, 'I', 'S']).
entry('Taxe', [t, a, k, s, @]).
entry('Taxifahrer', [t, a, k, s, i, f, 'a:', r, '6']).
entry('Taxis', [t, a, k, s, 'I', s]).
entry('Taxistand', [t, a, k, s, i, 'S', t, a, n, t]).
entry('Taxi', [t, a, k, s, i]).
entry('Teambesprechung', [t, 'i:', m, b, @, s, p, r, 'E', 'C', 'U', 'N']).
entry('Team', [t, 'i:', m]).
entry(technischen, [t, 'E', 'C', n, 'I', 'S', @, n]).
entry(technische, [t, 'E', 'C', n, 'I', 'S', @]).
entry('Tee', [t, 'e:']).
entry(teff, [t, 'E', f]).
entry('Tegernsee', [t, 'e:', g, '6', n, z, 'e:']).
entry(teibl, [t, aI, b, l]).
entry(teigige, [t, aI, g, 'I', g, @]).
entry('Teilbereich', [t, aI, l, b, @, r, aI, 'C']).
entry(teilen, [t, aI, l, @, n]).
entry(teilgenommen, [t, aI, l, g, @, n, 'O', m, @, n]).
entry('Teilnahme', [t, aI, l, n, 'a:', m, @]).
entry(teilnehmen, [t, aI, l, n, 'e:', m, @, n]).
entry('Teilnehmern', [t, aI, l, n, 'e:', m, '6', n]).
entry('Teilnehmer', [t, aI, l, n, 'e:', m, '6']).
entry(teilnehme, [t, aI, l, n, 'e:', m, @]).
entry('Teil', [t, aI, l]).
entry(teilweise, [t, aI, l, v, aI, z, @]).
entry(teilzunehmen, [t, aI, l, ts, u, n, 'e:', m, @, n]).
entry(tei, [t, aI]).
entry('Telefonaktion', [t, e, l, e, f, 'o:', n, a, k, ts, j, 'o:', n]).
entry('Telefonats', [t, e, l, e, f, o, n, 'a:', ts]).
entry('Telefonat', [t, e, l, e, f, o, n, 'a:', t]).
entry(telefonieren, [t, e, l, e, f, o, n, 'i:', r, @, n]).
entry(telefoniert, [t, e, l, e, f, o, n, 'i:', '6', t]).
entry(telefonischer, [t, e, l, e, f, 'o:', n, 'I', 'S', '6']).
entry(telefonisch, [t, e, l, e, f, 'o:', n, 'I', 'S']).
entry('Telefonnummern', [t, e, l, e, f, 'o:', n, n, 'U', m, '6', n]).
entry('Telefonnummer', [t, e, l, e, f, 'o:', n, n, 'U', m, '6']).
entry('Telefon', [t, e, l, e, f, 'o:', n]).
entry('Telefunken', [t, e, l, e, f, 'U', 'N', k, @, n]).
entry('Tele', [t, 'e:', l, @]).
entry(tember, [t, 'E', m, b, '6']).
entry('Tembinen', [t, 'E', m, b, 'i:', n, @, n]).
entry('Tem', [t, 'E', m]).
entry(tendenziell, [t, 'E', n, d, 'E', n, ts, j, 'E', l]).
entry(tendieren, [t, 'E', n, d, 'i:', r, @, n]).
entry(tendiere, [t, 'E', n, d, 'i:', r, @]).
entry('Tenius', [t, 'e:', n, j, 'U', s]).
entry('Tennismatch', [t, 'E', n, 'I', s, m, 'E', tS]).
entry('Tennisplatz', [t, 'E', n, 'I', s, p, l, a, ts]).
entry('Tennis', [t, 'E', n, 'I', s]).
entry('Tennisturnier', [t, 'E', n, 'I', s, t, 'U', '6', n, 'i:', '6']).
entry(ten, [t, 'E', n]).
entry('Tenzer', [t, 'E', n, ts, '6']).
entry('Terfiminvorschlag', [t, 'E', '6', f, 'I', m, 'i:', n, f, 'o:', '6', 'S', l, 'a:', k]).
entry('Terminabsprachen', [t, 'E', '6', m, 'i:', n, a, p, 'S', p, r, 'a:', x, @, n]).
entry('Terminabsprache', [t, 'E', '6', m, 'i:', n, a, p, 'S', p, r, 'a:', x, @]).
entry('Terminabsprechung', [t, 'E', '6', m, 'i:', n, a, p, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Terminal', [t, '9', '6', m, 'I', n, @, l]).
entry('Terminaufstellung', [t, 'E', '6', m, 'i:', n, aU, f, 'S', t, 'E', l, 'U', 'N']).
entry(terminen, [t, 'E', '6', m, 'i:', n, @, n]).
entry(termines, [t, 'E', '6', m, 'i:', n, @, s]).
entry('Termine', [t, 'E', '6', m, 'i:', n, @]).
entry('Terminfestlegung', [t, 'E', '6', m, 'i:', n, f, 'E', s, t, l, 'e:', g, 'U', 'N']).
entry('Terminfindung', [t, 'E', '6', m, 'i:', n, f, 'I', n, d, 'U', 'N']).
entry(terminfreie, [t, 'E', '6', m, 'i:', n, f, r, aI, @]).
entry('Termingestaltung', [t, 'E', '6', m, 'i:', n, g, @, s, t, a, l, t, 'U', 'N']).
entry('Terminidee', [t, 'E', '6', m, 'i:', n, i, d, 'e:']).
entry(terminieren, [t, 'E', '6', m, i, n, 'i:', r, @, n]).
entry('Terminkalendern', [t, 'E', '6', m, 'i:', n, k, a, l, 'E', n, d, '6', n]).
entry('Terminkalenders', [t, 'E', '6', m, 'i:', n, k, a, l, 'E', n, d, '6', s]).
entry('Terminkalender', [t, 'E', '6', m, 'i:', n, k, a, l, 'E', n, d, '6']).
entry('Terminknappheit', [t, 'E', '6', m, 'i:', n, k, n, a, p, h, aI, t]).
entry(terminlichen, [t, 'E', '6', m, 'i:', n, l, 'I', 'C', @, n]).
entry(terminliche, [t, 'E', '6', m, 'i:', n, l, 'I', 'C', @]).
entry(terminlich, [t, 'E', '6', m, 'i:', n, l, 'I', 'C']).
entry(terminmäßig, [t, 'E', '6', m, 'i:', n, m, 'E:', s, 'I', 'C']).
entry('Terminplaner', [t, 'E', '6', m, 'i:', n, p, l, 'a:', n, '6']).
entry('Terminpläne', [t, 'E', '6', m, 'i:', n, p, l, 'E:', n, @]).
entry('Terminplan', [t, 'E', '6', m, 'i:', n, p, l, 'a:', n]).
entry('Terminplanung', [t, 'E', '6', m, 'i:', n, p, l, 'a:', n, 'U', 'N']).
entry('Terminschwierigkeiten', [t, 'E', '6', m, 'i:', n, 'S', v, 'i:', r, 'I', 'C', k, aI, t, @, n]).
entry('Termins', [t, 'E', '6', m, 'i:', n, s]).
entry('Terminstreß', [t, 'E', '6', m, 'i:', n, 'S', t, r, 'E', s]).
entry(termin, [t, 'E', '6', m, 'i:', n]).
entry('Terminvereinbarungen', [t, 'E', '6', m, 'i:', n, f, 'E', '6', aI, n, b, 'a:', r, 'U', 'N', @, n]).
entry('Terminvereinbarung', [t, 'E', '6', m, 'i:', n, f, 'E', '6', aI, n, b, 'a:', r, 'U', 'N']).
entry('Terminverschiebung', [t, 'E', '6', m, 'i:', n, f, 'E', '6', 'S', 'i:', b, 'U', 'N']).
entry('Terminvorschläge', [t, 'E', '6', m, 'i:', n, f, 'o:', '6', 'S', l, 'E:', g, @]).
entry('Terminvorschlag', [t, 'E', '6', m, 'i:', n, f, 'o:', '6', 'S', l, 'a:', k]).
entry('Terminvorstellung', [t, 'E', '6', m, 'i:', n, f, 'o:', '6', 'S', t, 'E', l, 'U', 'N']).
entry('Terminwünsche', [t, 'E', '6', m, 'i:', n, v, 'Y', n, 'S', @]).
entry('Terminzusage', [t, 'E', '6', m, 'i:', n, ts, 'u:', z, 'a:', g, @]).
entry('Termi', [t, 'E', '6', m, 'i:']).
entry('Termonierung', [t, 'E', '6', m, o, n, 'i:', r, 'U', 'N']).
entry('Term', [t, 'E', '6', m]).
entry('Ternim', [t, 'E', '6', n, 'i:', m]).
entry('Terrasse', [t, 'E', r, a, s, @]).
entry('Ter', [t, 'E', '6']).
entry('Tessmann', [t, 'E', s, m, a, n]).
entry(testen, [t, 'E', s, t, @, n]).
entry(test, [t, 'E', s, t]).
entry('Te', [t, e]).
entry('Teuchert', [t, 'OY', 'C', '6', t]).
entry(teuere, [t, 'OY', @, r, @]).
entry(teuersten, [t, 'OY', '6', s, t, @, n]).
entry(teuerste, [t, 'OY', '6', s, t, @]).
entry(teuer, [t, 'OY', '6']).
entry('Teufel', [t, 'OY', f, @, l]).
entry(teureren, [t, 'OY', r, @, r, @, n]).
entry(teureres, [t, 'OY', r, @, r, @, s]).
entry(teurere, [t, 'OY', r, @, r, @]).
entry(teurer, [t, 'OY', r, '6']).
entry(teures, [t, 'OY', r, @, s]).
entry(teure, [t, 'OY', r, @]).
entry('Thailänder', [t, aI, l, 'E', n, d, '6']).
entry(thailändisch, [t, aI, l, 'E', n, d, 'I', 'S']).
entry('Thailand', [t, aI, l, @, n, d]).
entry('Thalia-Theater', [t, a, l, 'i:', a, t, e, 'a:', t, '6']).
entry(that, [z, 'E', t]).
entry('Thea', [t, 'e:', a]).
entry('Theater-am-Aegi', [t, e, 'a:', t, '6', a, m, 'E:', g, i]).
entry('Theaterangebote', [t, e, 'a:', t, '6', a, n, g, @, b, 'o:', t, @]).
entry('Theateraufführung', [t, e, 'a:', t, '6', aU, f, f, 'y:', r, 'U', 'N']).
entry('Theaterbesuch', [t, e, 'a:', t, '6', b, @, z, 'u:', x]).
entry('Theaterfan', [t, e, 'a:', t, '6', f, 'E:', n]).
entry('Theaterinformationen', [t, e, 'a:', t, '6', 'I', n, f, 'O', '6', m, a, ts, j, 'o:', n, @, n]).
entry('Theaterkarten', [t, e, 'a:', t, '6', k, a, r, t, @, n]).
entry('Theatern', [t, e, 'a:', t, '6', n]).
entry('Theaterprogramme', [t, e, 'a:', t, '6', p, r, o, g, r, a, m, @]).
entry('Theaterprogramm', [t, e, 'a:', t, '6', p, r, o, g, r, a, m]).
entry('Theater-schauen', [t, e, 'a:', t, '6', 'S', aU, @, n]).
entry('Theaterstadt', [t, e, 'a:', t, '6', 'S', t, a, t]).
entry('Theaters', [t, e, 'a:', t, '6', s]).
entry('Theaterstücke', [t, e, 'a:', t, '6', 'S', t, 'Y', k, @]).
entry('Theaterstück', [t, e, 'a:', t, '6', 'S', t, 'Y', k]).
entry('Theaterszene', [t, e, 'a:', t, '6', s, ts, 'e:', n, @]).
entry('Theater', [t, e, 'a:', t, '6']).
entry('Theatertermine', [t, e, 'a:', t, '6', t, 'E', '6', m, 'i:', n, @]).
entry('Theatertermin', [t, e, 'a:', t, '6', t, 'E', '6', m, 'i:', n]).
entry('Theaterveranstaltungen', [t, e, 'a:', t, '6', f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', @, n]).
entry('Theatervorstellungen', [t, e, 'a:', t, '6', f, 'o:', '6', 'S', t, 'E', l, 'U', 'N', @, n]).
entry('Theatervorstellung', [t, e, 'a:', t, '6', f, 'o:', '6', 'S', t, 'E', l, 'U', 'N']).
entry('Theatralisches', [t, e, a, t, r, 'a:', l, 'I', 'S', @, s]).
entry('Thema', [t, 'e:', m, a]).
entry('Thematik', [t, e, m, 'a:', t, 'I', k]).
entry('Themen', [t, 'e:', m, @, n]).
entry('Them', [t, 'e:', m]).
entry(then, ['T', 'E', n]).
entry('Theodor', [t, 'e:', o, d, 'o:', '6']).
entry(theoretische, [t, e, o, r, 'e:', t, 'I', 'S', @]).
entry(theoretisch, [t, e, o, r, 'e:', t, 'I', 'S']).
entry('Thielenplatz', [t, 'i:', l, @, n, p, l, a, ts]).
entry('Thiele', [t, 'i:', l, @]).
entry('Thomas', [t, 'o:', m, a, s]).
entry('Thompson', [t, 'O', m, p, s, @, n]).
entry('Thüringer-Hof', [t, 'y:', r, 'I', 'N', '6', h, 'o:', f]).
entry('Thyssen-Schulte', [t, 'Y', s, @, n, 'S', 'U', l, t, @]).
entry(tickets, [t, 'I', k, @, ts]).
entry('Ticket', [t, 'I', k, @, t]).
entry('Tiergarten', [t, 'i:', '6', g, a, r, t, @, n]).
entry('Tierpark', [t, 'i:', '6', p, a, r, k]).
entry('Tier', [t, 'i:', '6']).
entry('Tillmann', [t, 'I', l, m, a, n]).
entry(time, [t, aI, m]).
entry('Tina', [t, 'i:', n, a]).
entry('Tips', [t, 'I', p, s]).
entry('Tischer', [t, 'I', 'S', '6']).
entry('Tisch', [t, 'I', 'S']).
entry('Titanic', [t, i, t, 'a:', n, 'I', k]).
entry(tja, [t, j, a]).
entry('Tobias', [t, o, b, 'i:', a, s]).
entry('Tobi', [t, 'o:', b, i]).
entry('Tochter', [t, 'O', x, t, '6']).
entry(tollen, [t, 'O', l, @, n]).
entry(tolles, [t, 'O', l, @, s]).
entry(tolle, [t, 'O', l, @]).
entry(toll, [t, 'O', l]).
entry('Toni', [t, 'o:', n, i]).
entry('Toskana', [t, 'O', s, k, 'a:', n, a]).
entry(total, [t, o, t, 'a:', l]).
entry('Totensonntags', [t, 'o:', t, @, n, z, 'O', n, t, 'a:', k, s]).
entry('Totensonntag', [t, 'o:', t, @, n, z, 'O', n, t, 'a:', k]).
entry(to, [t, 'O']).
entry(touch, [t, a, tS]).
entry('Tourismus-Führer', [t, u, r, 'I', s, m, 'U', s, f, 'y:', r, '6']).
entry('Tourismus', [t, u, r, 'I', s, m, 'U', s]).
entry('Touristen', [t, u, r, 'I', s, t, @, n]).
entry('Tourist-Information', [t, 'u:', r, 'I', s, t, 'I', n, f, 'O', '6', m, 'e:', 'S', n]).
entry('Tour', [t, 'u:', '6']).
entry(tour, [t, 'U', @, r]).
entry(traditionell, [t, r, a, d, i, ts, j, o, n, 'E', l]).
entry(tragbar, [t, r, 'a:', k, b, 'a:', r]).
entry(tragen, [t, r, 'a:', g, @, n]).
entry(trage, [t, r, 'a:', g, @]).
entry(tragisch, [t, r, 'a:', g, 'I', 'S']).
entry(trägt, [t, r, 'E:', k, t]).
entry(trainieren, [t, r, 'e:', n, 'i:', r, @, n]).
entry('Transferbusse', [t, r, a, n, s, f, 'e:', '6', b, 'U', s, @]).
entry('Transfer', [t, r, a, n, s, f, 'e:', '6']).
entry('Transferzeit', [t, r, a, n, s, f, 'e:', '6', ts, aI, t]).
entry('Transportation', [t, r, a, n, s, p, 'O', '6', t, a, ts, j, 'o:', n]).
entry(transportieren, [t, r, a, n, s, p, 'O', '6', t, 'i:', r, @, n]).
entry('Transportmittel', [t, r, a, n, s, p, 'O', '6', t, m, 'I', t, @, l]).
entry('Transportmöglichkeiten', [t, r, a, n, s, p, 'O', '6', t, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Transport', [t, r, a, n, s, p, 'O', '6', t]).
entry('Transportverbindungen', [t, r, a, n, s, p, 'O', '6', t, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry(tra, [t, r, 'a:']).
entry('Traub', [t, r, aU, p]).
entry('Traumdatum', [t, r, aU, m, d, 'a:', t, 'U', m]).
entry('Traunstein', [t, r, aU, n, 'S', t, aI, n]).
entry(traurig, [t, r, aU, r, 'I', 'C']).
entry('Treffens', [t, r, 'E', f, @, n, s]).
entry(treffen, [t, r, 'E', f, @, n]).
entry('Treffer', [t, r, 'E', f, '6']).
entry(treffe, [t, r, 'E', f, @]).
entry('Treff-Europa', [t, r, 'E', f, 'OY', r, 'o:', p, a]).
entry('Treff-Hotel-Europa', [t, r, 'E', f, h, o, t, 'E', l, 'OY', r, 'o:', p, a]).
entry('Treffi', [t, r, 'E', f, 'I']).
entry('Treffpunkte', [t, r, 'E', f, p, 'U', 'N', k, t, @]).
entry('Treffpunkt', [t, r, 'E', f, p, 'U', 'N', k, t]).
entry(treff, [t, r, 'E', f]).
entry(treiben, [t, r, aI, b, @, n]).
entry(treibt, [t, r, aI, p, t]).
entry(trennen, [t, r, 'E', n, @, n]).
entry(trennt, [t, r, 'E', n, t]).
entry('Trenntür', [t, r, 'E', n, t, 'y:', '6']).
entry(treten, [t, r, 'e:', t, @, n]).
entry(tre, [t, r, 'E']).
entry('Trier', [t, r, 'i:', '6']).
entry(trifft, [t, r, 'I', f, t]).
entry(trinken, [t, r, 'I', 'N', k, @, n]).
entry(trinke, [t, r, 'I', 'N', k, @]).
entry('Trin', [t, r, 'I', n]).
entry('Trip', [t, r, 'I', p]).
entry(trockenen, [t, r, 'O', k, @, n, @, n]).
entry(trocken, [t, r, 'O', k, @, n]).
entry('Trost', [t, r, 'o:', s, t]).
entry(trotzdem, [t, r, 'O', ts, d, 'e:', m]).
entry(trotz, [t, r, 'O', ts]).
entry('Trouble', [t, r, a, b, l]).
entry('Tr', [t, r]).
entry('Trubel', [t, r, 'u:', b, @, l]).
entry('Truman-Show', [t, r, 'u:', m, @, n, 'S', 'o:']).
entry(tschamtatam, [tS, a, m, t, a, t, a, m]).
entry(tschau, [tS, aU]).
entry('Tscheschlok', [tS, 'E', 'S', l, 'O', k]).
entry(tsch, [tS]).
entry(tschüßgen, [tS, 'Y', s, g, @, n]).
entry(tschüß, [tS, 'Y', s]).
entry('Tübingen', [t, 'y:', b, 'I', 'N', @, n]).
entry(tue, [t, 'u:', @]).
entry(tummeln, [t, 'U', m, @, l, n]).
entry('Tum', [t, 'u:', m]).
entry(tun, [t, 'u:', n]).
entry('Turbulenzen', [t, 'U', '6', b, u, l, 'E', n, ts, @, n]).
entry('Türen', [t, 'y:', r, @, n]).
entry('Türe', [t, 'y:', r, @]).
entry(türlich, [t, 'y:', '6', l, 'I', 'C']).
entry('Tür', [t, 'y:', '6']).
entry('Tutoren', [t, u, t, 'o:', r, @, n]).
entry('Tutorium', [t, u, t, 'o:', '6', j, 'U', m]).
entry('Tutor', [t, 'u:', t, 'o:', '6']).
entry(tut, [t, 'u:', t]).
entry('TÜV-Termin', [t, 'Y', f, t, 'E', '6', m, 'i:', n]).
entry('TÜV', [t, 'Y', f]).
entry(typisch, [t, 'y:', p, 'I', 'S']).
entry('Typ', [t, 'y:', p]).
entry('Tzislinski', [ts, 'I', s, l, 'I', n, s, k, i]).
entry(uah, [u, 'a:']).
entry(übel, ['y:', b, @, l]).
entry(überall, ['y:', b, '6', a, l]).
entry(überarbeiten, ['y:', b, '6', a, r, b, aI, t, @, n]).
entry(überarbeitet, ['y:', b, '6', a, r, b, aI, t, @, t]).
entry(überblicken, ['y:', b, '6', b, l, 'I', k, @, n]).
entry(überblicke, ['y:', b, '6', b, l, 'I', k, @]).
entry(überbracht, ['y:', b, '6', b, r, a, x, t]).
entry(überbrücken, ['y:', b, '6', b, r, 'Y', k, @, n]).
entry(überbrückt, ['y:', b, '6', b, r, 'Y', k, t]).
entry(überbucht, ['y:', b, '6', b, 'u:', x, t]).
entry(übereinkommen, ['y:', b, '6', aI, n, k, 'O', m, @, n]).
entry('Übereinkunft', ['y:', b, '6', aI, n, k, 'U', n, f, t]).
entry(überein, ['y:', b, '6', aI, n]).
entry(übereinstimmen, ['y:', b, '6', aI, n, 'S', t, 'I', m, @, n]).
entry(übereinstimmt, ['y:', b, '6', aI, n, 'S', t, 'I', m, t]).
entry('Übereinstimmungen', ['y:', b, '6', aI, n, 'S', t, 'I', m, 'U', 'N', @, n]).
entry('Übereinstimmung', ['y:', b, '6', aI, n, 'S', t, 'I', m, 'U', 'N']).
entry(überfalle, ['y:', b, '6', f, a, l, @]).
entry(überflüssig, ['y:', b, '6', f, l, 'Y', s, 'I', 'C']).
entry(überfordert, ['y:', b, '6', f, 'O', '6', d, '6', t]).
entry(überfragt, ['y:', b, '6', f, r, 'a:', k, t]).
entry(überfüllt, ['y:', b, '6', f, 'Y', l, t]).
entry('Übergang', ['y:', b, '6', g, a, 'N']).
entry(übergeben, ['y:', b, '6', g, 'e:', b, @, n]).
entry(übergehen, ['y:', b, '6', g, 'e:', @, n]).
entry(überhaupt, ['y:', b, '6', h, aU, p, t]).
entry(überhau, ['y:', b, '6', h, aU]).
entry(überkomfortabel, ['y:', b, '6', k, 'O', m, f, 'O', '6', t, 'a:', b, @, l]).
entry(überlassen, ['y:', b, '6', l, a, s, @, n]).
entry(überlasse, ['y:', b, '6', l, a, s, @]).
entry(überlaufen, ['y:', b, '6', l, aU, f, @, n]).
entry(überleben, ['y:', b, '6', l, 'e:', b, @, n]).
entry(überlegen, ['y:', b, '6', l, 'e:', g, @, n]).
entry(überlege, ['y:', b, '6', l, 'e:', g, @]).
entry(überlegt, ['y:', b, '6', l, 'e:', k, t]).
entry(überl, ['y:', b, '6', l]).
entry(übermorgen, ['y:', b, '6', m, 'O', '6', g, @, n]).
entry(übermüdet, ['y:', b, '6', m, 'y:', d, @, t]).
entry(übernächsten, ['y:', b, '6', n, 'E:', 'C', s, t, @, n]).
entry(übernächste, ['y:', b, '6', n, 'E:', 'C', s, t, @]).
entry(übernachten, ['y:', b, '6', n, a, x, t, @, n]).
entry(übernachte, ['y:', b, '6', n, a, x, t, @]).
entry(übernachtete, ['y:', b, '6', n, a, x, t, @, t, @]).
entry(übernachtet, ['y:', b, '6', n, a, x, t, @, t]).
entry('Übernachtungen', ['y:', b, '6', n, a, x, t, 'U', 'N', @, n]).
entry('Übernachtung', ['y:', b, '6', n, a, x, t, 'U', 'N']).
entry('Übernachtungskosten', ['y:', b, '6', n, a, x, t, 'U', 'N', s, k, 'O', s, t, @, n]).
entry('Übernachtungsmöglichkeiten', ['y:', b, '6', n, a, x, t, 'U', 'N', s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry('Übernachtungsmöglichkeit', ['y:', b, '6', n, a, x, t, 'U', 'N', s, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Übernachtungsmöglichk', ['y:', b, '6', n, a, x, t, 'U', 'N', s, m, '2:', k, l, 'I', 'C', k]).
entry(übernehmen, ['y:', b, '6', n, 'e:', m, @, n]).
entry(übernehme, ['y:', b, '6', n, 'e:', m, @]).
entry(übernimmt, ['y:', b, '6', n, 'I', m, t]).
entry(übernommen, ['y:', b, '6', n, 'O', m, @, n]).
entry(übern, ['y:', b, '6', n]).
entry(überprüfen, ['y:', b, '6', p, r, 'y:', f, @, n]).
entry(über, ['y:', b, '6']).
entry(überragend, ['y:', b, '6', r, 'a:', g, @, n, t]).
entry(überraschen, ['y:', b, '6', r, a, 'S', @, n]).
entry(überrascht, ['y:', b, '6', r, a, 'S', t]).
entry(überreden, ['y:', b, '6', r, 'e:', d, @, n]).
entry(überschaubarer, ['y:', b, '6', 'S', aU, b, 'a:', r, '6']).
entry(überschaue, ['y:', b, '6', 'S', aU, @]).
entry(überschlagen, ['y:', b, '6', 'S', l, 'a:', g, @, n]).
entry(überschlage, ['y:', b, '6', 'S', l, 'a:', g, @]).
entry(überschneidenden, ['y:', b, '6', 'S', n, aI, d, @, n, d, @, n]).
entry(überschneidend, ['y:', b, '6', 'S', n, aI, d, @, n, t]).
entry(überschneiden, ['y:', b, '6', 'S', n, aI, d, @, n]).
entry(überschneidet, ['y:', b, '6', 'S', n, aI, d, @, t]).
entry('Überschneidungen', ['y:', b, '6', 'S', n, aI, d, 'U', 'N', @, n]).
entry('Überschneidung', ['y:', b, '6', 'S', n, aI, d, 'U', 'N']).
entry('Überschneidungsprobleme', ['y:', b, '6', 'S', n, aI, d, 'U', 'N', s, p, r, o, b, l, 'e:', m, @]).
entry('Überschneidungspunkt', ['y:', b, '6', 'S', n, aI, d, 'U', 'N', s, p, 'U', 'N', k, t]).
entry('Überschneidungstermine', ['y:', b, '6', 'S', n, aI, d, 'U', 'N', s, t, 'E', '6', m, 'i:', n, @]).
entry(überschreiten, ['y:', b, '6', 'S', r, aI, t, @, n]).
entry(übersehen, ['y:', b, '6', z, 'e:', @, n]).
entry('Übersetzungshilfe', ['y:', b, '6', z, 'E', ts, 'U', 'N', s, h, 'I', l, f, @]).
entry('Übersicht', ['y:', b, '6', z, 'I', 'C', t]).
entry(übers, ['y:', b, '6', s]).
entry(überstanden, ['y:', b, '6', 'S', t, a, n, d, @, n]).
entry(übertreiben, ['y:', b, '6', t, r, aI, b, @, n]).
entry(übertrieben, ['y:', b, '6', t, r, 'i:', b, @, n]).
entry(überübermorgen, ['y:', b, '6', 'y:', b, '6', m, 'O', '6', g, @, n]).
entry(überweisen, ['y:', b, '6', v, aI, z, @, n]).
entry(überweise, ['y:', b, '6', v, aI, z, @]).
entry(überwinden, ['y:', b, '6', v, 'I', n, d, @, n]).
entry(überzeugend, ['y:', b, '6', ts, 'OY', g, @, n, t]).
entry(überzeugen, ['y:', b, '6', ts, 'OY', g, @, n]).
entry(überzeugt, ['y:', b, '6', ts, 'OY', k, t]).
entry('Überzeugung', ['y:', b, '6', ts, 'OY', g, 'U', 'N']).
entry(überziehen, ['y:', b, '6', ts, 'i:', @, n]).
entry(üblichen, ['y:', p, l, 'I', 'C', @, n]).
entry(übliche, ['y:', p, l, 'I', 'C', @]).
entry(üblicherweise, ['y:', p, l, 'I', 'C', '6', v, aI, z, @]).
entry(üblich, ['y:', p, l, 'I', 'C']).
entry(übrich, ['y:', b, r, 'I', 'C']).
entry(übrigbleiben, ['y:', b, r, 'I', 'C', b, l, aI, b, @, n]).
entry(übrigen, ['y:', b, r, 'I', g, @, n]).
entry(übrigens, ['y:', b, r, 'I', g, @, n, s]).
entry(übriggebliebene, ['y:', b, r, 'I', 'C', g, @, b, l, 'i:', b, @, n, @]).
entry(übri, ['y:', b, r, 'I']).
entry('Übung', ['y:', b, 'U', 'N']).
entry('Übungsblätter', ['y:', b, 'U', 'N', s, b, l, 'E', t, '6']).
entry('Übungsblatt', ['y:', b, 'U', 'N', s, b, l, a, t]).
entry('Übungs', ['y:', b, 'U', 'N', s]).
entry('Übungssaal', ['y:', b, 'U', 'N', s, z, 'a:', l]).
entry(üb, ['y:', p]).
entry('Udelhofen', ['u:', d, @, l, h, 'o:', f, @, n]).
entry('Udo', ['u:', d, o]).
entry('Ueding', ['y:', d, 'I', 'N']).
entry(uff, ['U', f]).
entry('Uhle', ['u:', l, @]).
entry(uh, ['u:']).
entry('Uhr', ['u:', '6']).
entry('Uhrzeiten', ['u:', '6', ts, aI, t, @, n]).
entry('Uhrzeit', ['u:', '6', ts, aI, t]).
entry(ui, ['u:', i]).
entry('Ulf', ['U', l, f]).
entry('Uli', ['U', l, i]).
entry(uli, ['u:', l, i]).
entry('Ulm', ['U', l, m]).
entry('Ulrich', ['U', l, r, 'I', 'C']).
entry(umändern, [u, m, 'E', n, d, '6', n]).
entry('Umbau', ['U', m, b, aU]).
entry(umbuchen, ['U', m, b, 'u:', x, @, n]).
entry(umdisponieren, ['U', m, d, 'I', s, p, o, n, 'i:', r, @, n]).
entry(umem, ['U', m, @, m]).
entry(umentscheiden, ['U', m, 'E', n, tS, aI, d, @, n]).
entry(ümen, ['y:', m, @, n]).
entry(umfangreichen, ['U', m, f, a, 'N', r, aI, 'C', @, n]).
entry(umfangreiches, ['U', m, f, a, 'N', r, aI, 'C', @, s]).
entry(umfangreich, ['U', m, f, a, 'N', r, aI, 'C']).
entry(umfaßt, ['U', m, f, a, s, t]).
entry('Umgebung', ['U', m, g, 'e:', b, 'U', 'N']).
entry(umgehen, ['U', m, g, 'e:', @, n]).
entry(umgehört, ['U', m, g, @, h, '2:', '6', t]).
entry(umgekehrt, ['U', m, g, @, k, 'e:', '6', t]).
entry(umgerechnet, ['U', m, g, @, r, 'E', 'C', n, @, t]).
entry(umgeschmissen, ['U', m, g, @, 'S', m, 'I', s, @, n]).
entry(umgesehen, ['U', m, g, @, z, 'e:', @, n]).
entry(umgetan, ['U', m, g, @, t, 'a:', n]).
entry(umhören, ['U', m, h, '2:', r, @, n]).
entry(umkucken, ['U', m, k, 'U', k, @, n]).
entry('Umland', ['U', m, l, a, n, t]).
entry('Umlauf', ['U', m, l, aU, f]).
entry('Umlaut-Anton', ['U', m, l, aU, t, a, n, t, 'o:', n]).
entry(umlegen, ['U', m, l, 'e:', g, @, n]).
entry(ummäntelt, ['U', m, m, 'E', n, t, @, l, t]).
entry(umplanen, ['U', m, p, l, 'a:', n, @, n]).
entry(um, ['U', m]).
entry(umschauen, ['U', m, 'S', aU, @, n]).
entry(umschaue, ['U', m, 'S', aU, @]).
entry(umschlagplätze, ['U', m, 'S', l, 'a:', k, p, l, 'E', ts, @]).
entry(umschreiben, ['U', m, 'S', r, aI, b, @, n]).
entry(ums, ['U', m, s]).
entry('Umständen', ['U', m, 'S', t, 'E', n, d, @, n]).
entry('Umstände', ['U', m, 'S', t, 'E', n, d, @]).
entry(umständlich, ['U', m, 'S', t, 'E', n, t, l, 'I', 'C']).
entry(umsteigen, ['U', m, 'S', t, aI, g, @, n]).
entry('Umtrunk', ['U', m, t, r, 'U', 'N', k]).
entry('Umweg', ['U', m, v, 'e:', k]).
entry('Umweltaspekten', ['U', m, v, 'E', l, t, a, s, p, 'E', k, t, @, n]).
entry(umweltfreundlicher, ['U', m, v, 'E', l, t, f, r, 'OY', n, t, l, 'I', 'C', '6']).
entry('Umweltgründen', ['U', m, v, 'E', l, t, g, r, 'Y', n, d, @, n]).
entry('Umwelt', ['U', m, v, 'E', l, t]).
entry(umweltschonender, ['U', m, v, 'E', l, tS, 'o:', n, @, n, d, '6']).
entry('Umweltseminar', ['U', m, v, 'E', l, t, z, e, m, i, n, 'a:', r]).
entry(umweltunfreundlicher, ['U', m, v, 'E', l, t, 'U', n, f, r, 'OY', n, t, l, 'I', 'C', '6']).
entry('Umweltverschmutzung', ['U', m, v, 'E', l, t, f, 'E', '6', 'S', m, 'U', ts, 'U', 'N']).
entry(umziehe, ['U', m, ts, 'i:', @]).
entry(umzubuchen, ['U', m, ts, u, b, 'u:', x, @, n]).
entry('Umzug', ['U', m, ts, 'u:', k]).
entry(umzuhören, ['U', m, ts, u, h, '2:', r, @, n]).
entry(umzukrempeln, ['U', m, ts, u, k, r, 'E', m, p, @, l, n]).
entry(umzu, ['U', m, ts, u]).
entry(umzusteigen, ['U', m, ts, u, 'S', t, aI, g, @, n]).
entry(unabhängiger, ['U', n, a, p, h, 'E', 'N', 'I', g, '6']).
entry(unabhängig, ['U', n, a, p, h, 'E', 'N', 'I', 'C']).
entry(unabkömmlich, ['U', n, a, p, k, '9', m, l, 'I', 'C']).
entry('Unangenehme', ['U', n, a, n, g, @, n, 'e:', m, @]).
entry('Unangenehmes', ['U', n, a, n, g, @, n, 'e:', m, @, s]).
entry(unangenehm, ['U', n, a, n, g, @, n, 'e:', m]).
entry(unausgeschlafen, ['U', n, aU, s, g, @, 'S', l, 'a:', f, @, n]).
entry(unbedarft, ['U', n, b, @, d, a, r, f, t]).
entry(unbedingt, ['U', n, b, @, d, 'I', 'N', t]).
entry(unbelegt, ['U', n, b, @, l, 'e:', k, t]).
entry(unbequeme, ['U', n, b, @, k, v, 'e:', m, @]).
entry(unbequem, ['U', n, b, @, k, v, 'e:', m]).
entry(unberücksichtigt, ['U', n, b, @, r, 'Y', k, z, 'I', 'C', t, 'I', 'C', t]).
entry(unbeschränkt, ['U', n, b, @, 'S', r, 'E', 'N', k, t]).
entry(unchristliche, ['U', n, k, r, 'I', s, t, l, 'I', 'C', @]).
entry(unddreißigsten, ['U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(underbar, ['U', n, d, '6', b, 'a:', r]).
entry(und, ['U', n, t]).
entry(undzwanzigster, ['U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(uneigennützig, ['U', n, aI, g, @, n, n, 'Y', ts, 'I', 'C']).
entry(uneingeschränkt, ['U', n, aI, n, g, @, 'S', r, 'E', 'N', k, t]).
entry(unerheblich, ['U', n, 'E', '6', h, 'e:', p, l, 'I', 'C']).
entry(unerhört, ['U', n, 'E', '6', h, '2:', '6', t]).
entry(unerträglich, ['U', n, 'E', '6', t, r, 'E:', k, l, 'I', 'C']).
entry(unerwartet, ['U', n, 'E', '6', v, a, r, t, @, t]).
entry('Unfällen', ['U', n, f, 'E', l, @, n]).
entry('Unfälle', ['U', n, f, 'E', l, @]).
entry('Unfall', ['U', n, f, a, l]).
entry('Unfallquoten', ['U', n, f, a, l, k, v, 'o:', t, @, n]).
entry(ungebunden, ['U', n, g, @, b, 'U', n, d, @, n]).
entry(ungefähr, ['U', n, g, @, f, 'E:', '6']).
entry(ungef, ['U', n, g, @, f]).
entry(ungelegen, ['U', n, g, @, l, 'e:', g, @, n]).
entry(ungenau, ['U', n, g, @, n, aU]).
entry(ungerne, ['U', n, g, 'E', '6', n, @]).
entry(ungern, ['U', n, g, 'E', '6', n]).
entry(ungeschickt, ['U', n, g, @, 'S', 'I', k, t]).
entry(ungestörtesten, ['U', n, g, @, 'S', t, '2:', '6', t, @, s, t, @, n]).
entry(ungestört, ['U', n, g, @, 'S', t, '2:', '6', t]).
entry(ungewöhnlich, ['U', n, g, @, v, '2:', n, l, 'I', 'C']).
entry(unglaubliche, ['U', n, g, l, aU, p, l, 'I', 'C', @]).
entry(unglaublich, ['U', n, g, l, aU, p, l, 'I', 'C']).
entry(unglücklicherweise, ['U', n, g, l, 'Y', k, l, 'I', 'C', '6', v, aI, z, @]).
entry(unglücklich, ['U', n, g, l, 'Y', k, l, 'I', 'C']).
entry('Unglück', ['U', n, g, l, 'Y', k]).
entry(ungülstich, ['U', n, g, 'Y', l, s, t, 'I', 'C']).
entry(ung, ['U', 'N']).
entry(ungünstiger, ['U', n, g, 'Y', n, s, t, 'I', g, '6']).
entry(ungünstig, ['U', n, g, 'Y', n, s, t, 'I', 'C']).
entry(unheimlich, ['U', n, h, aI, m, l, 'I', 'C']).
entry(uninteressant, ['U', n, 'I', n, t, @, r, 'E', s, a, n, t]).
entry('Uni', ['U', n, i]).
entry('Universi', [u, n, i, v, 'E', '6', z, i]).
entry('Universitäten', [u, n, i, v, 'E', '6', z, i, t, 'E:', t, @, n]).
entry('Universität', [u, n, i, v, 'E', '6', z, i, t, 'E:', t]).
entry('Universitätsmensa', [u, n, i, v, 'E', '6', z, i, t, 'E:', ts, m, 'E', n, z, a]).
entry('Universitätsstädten', [u, n, i, v, 'E', '6', z, i, t, 'E:', ts, 'S', t, 'E:', t, @, n]).
entry('Universitätsstädte', [u, n, i, v, 'E', '6', z, i, t, 'E:', ts, 'S', t, 'E:', t, @]).
entry('Universitschät', [u, n, i, v, 'E', '6', z, i, tS, 'E:', t]).
entry('Univerti', [u, n, i, v, 'E', '6', t, i]).
entry(unkompliziert, ['U', n, k, 'O', m, p, l, i, ts, 'i:', '6', t]).
entry(unkonzentrierter, ['U', n, k, 'O', n, ts, 'E', n, t, r, 'i:', '6', t, '6']).
entry('Unkosten', ['U', n, k, 'O', s, t, @, n]).
entry('Unk', ['U', 'N', k]).
entry(unmittelbar, ['U', n, m, 'I', t, @, l, b, 'a:', r]).
entry(unmöglich, ['U', n, m, '2:', k, l, 'I', 'C']).
entry('Unpäßlichkeiten', ['U', n, p, 'E', s, l, 'I', 'C', k, aI, t, @, n]).
entry(unpaß, ['U', n, p, a, s]).
entry(unpassend, ['U', n, p, a, s, @, n, t]).
entry(unpraktisch, ['U', n, p, r, a, k, t, 'I', 'S']).
entry(unproblematisch, ['U', n, p, r, o, b, l, e, m, 'a:', t, 'I', 'S']).
entry(un, ['U', n]).
entry(unrealistisch, ['U', n, r, e, a, l, 'I', s, t, 'I', 'S']).
entry(unrecht, ['U', n, r, 'E', 'C', t]).
entry(unsägliche, ['U', n, z, 'E:', k, l, 'I', 'C', @]).
entry(unse, ['U', n, z, @]).
entry(unserem, ['U', n, z, @, r, @, m]).
entry(unseren, ['U', n, z, @, r, @, n]).
entry(unsere, ['U', n, z, @, r, @]).
entry(unserer, ['U', n, z, @, r, '6']).
entry(unseres, ['U', n, z, @, r, @, s]).
entry(unserm, ['U', n, z, '6', m]).
entry(unsern, ['U', n, z, '6', n]).
entry(unser, ['U', n, z, '6']).
entry(unsicher, ['U', n, z, 'I', 'C', '6']).
entry('Unsinn', ['U', n, z, 'I', n]).
entry(uns, ['U', n, s]).
entry(unsrem, ['U', n, s, r, @, m]).
entry(unsren, ['U', n, s, r, @, n]).
entry(unsre, ['U', n, s, r, @]).
entry(unsrer, ['U', n, s, r, '6']).
entry(unsres, ['U', n, s, r, @, s]).
entry(unten, ['U', n, t, @, n]).
entry(unterbekommen, ['U', n, t, '6', b, @, k, 'O', m, @, n]).
entry(unterbrechen, ['U', n, t, '6', b, r, 'E', 'C', @, n]).
entry('Unterbrechung', ['U', n, t, '6', b, r, 'E', 'C', 'U', 'N']).
entry(unterbreite, ['U', n, t, '6', b, r, aI, t, @]).
entry(unterbringen, ['U', n, t, '6', b, r, 'I', 'N', @, n]).
entry('Unterbringung', ['U', n, t, '6', b, r, 'I', 'N', 'U', 'N']).
entry('Unterbringungsmöglichkeiten', ['U', n, t, '6', b, r, 'I', 'N', 'U', 'N', s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry(unterbrochen, ['U', n, t, '6', b, r, 'O', x, @, n]).
entry(unteren, ['U', n, t, @, r, @, n]).
entry(untere, ['U', n, t, @, r, @]).
entry('Untergebenen', ['U', n, t, '6', g, 'e:', b, @, n, @, n]).
entry(untergebracht, ['U', n, t, '6', g, @, b, r, a, x, t]).
entry(untergebra, ['U', n, t, '6', g, @, b, r, a]).
entry(untergekommen, ['U', n, t, '6', g, @, k, 'O', m, @, n]).
entry(unterhaltend, ['U', n, t, '6', h, a, l, t, @, n, t]).
entry(unterhalten, ['U', n, t, '6', h, a, l, t, @, n]).
entry(unterhaltsamer, ['U', n, t, '6', h, a, l, t, z, 'a:', m, '6']).
entry('Unterhaltsames', ['U', n, t, '6', h, a, l, t, z, 'a:', m, @, s]).
entry('Unterhaltungen', ['U', n, t, '6', h, a, l, t, 'U', 'N', @, n]).
entry('Unterhaltung', ['U', n, t, '6', h, a, l, t, 'U', 'N']).
entry(unterher, ['U', n, t, '6', h, 'e:', '6']).
entry(unterkommen, ['U', n, t, '6', k, 'O', m, @, n]).
entry(unterkomme, ['U', n, t, '6', k, 'O', m, @]).
entry(unterkriegen, ['U', n, t, '6', k, r, 'i:', g, @, n]).
entry('Unterkünften', ['U', n, t, '6', k, 'Y', n, f, t, @, n]).
entry('Unterkünfte', ['U', n, t, '6', k, 'Y', n, f, t, @]).
entry('Unterkunft', ['U', n, t, '6', k, 'U', n, f, t]).
entry(unterkunftsmäßig, ['U', n, t, '6', k, 'U', n, f, ts, m, 'E:', s, 'I', 'C']).
entry('Unterkunftsmöglichkeit', ['U', n, t, '6', k, 'U', n, f, ts, m, '2:', k, l, 'I', 'C', k, aI, t]).
entry('Unterkunfts', ['U', n, t, '6', k, 'U', n, f, ts]).
entry('Unterkunftsverzeichnis', ['U', n, t, '6', k, 'U', n, f, ts, f, 'E', '6', ts, aI, 'C', n, 'I', s]).
entry('Unterlagen', ['U', n, t, '6', l, 'a:', g, @, n]).
entry('Unterla', ['U', n, t, '6', l, a]).
entry(unternehmen, ['U', n, t, '6', n, 'e:', m, @, n]).
entry('Unternehmensberatung', ['U', n, t, '6', n, 'e:', m, @, n, s, b, @, r, 'a:', t, 'U', 'N']).
entry('Unternehmensberatungs-Gesellschaft', ['U', n, t, '6', n, 'e:', m, @, n, s, b, @, r, 'a:', t, 'U', 'N', s, g, @, z, 'E', l, 'S', a, f, t]).
entry('Unternehmungen', ['U', n, t, '6', n, 'e:', m, 'U', 'N', @, n]).
entry('Unternehmung', ['U', n, t, '6', n, 'e:', m, 'U', 'N']).
entry('Unternehmungsmöglichkeiten', ['U', n, t, '6', n, 'e:', m, 'U', 'N', s, m, '2:', k, l, 'I', 'C', k, aI, t, @, n]).
entry(untern, ['U', n, t, '6', n]).
entry(unter, ['U', n, t, '6']).
entry(unterrichten, ['U', n, t, '6', r, 'I', 'C', t, @, n]).
entry(unterscheiden, ['U', n, t, '6', 'S', aI, d, @, n]).
entry(unterschiede, ['U', n, t, '6', 'S', 'i:', d, @]).
entry(unterschiedlichen, ['U', n, t, '6', 'S', 'i:', t, l, 'I', 'C', @, n]).
entry(unterschiedliche, ['U', n, t, '6', 'S', 'i:', t, l, 'I', 'C', @]).
entry(unterschiedlich, ['U', n, t, '6', 'S', 'i:', t, l, 'I', 'C']).
entry('Unterschied', ['U', n, t, '6', 'S', 'i:', t]).
entry('Unterschrift', ['U', n, t, '6', 'S', r, 'I', f, t]).
entry(unters, ['U', n, t, '6', s]).
entry(unterstellen, ['U', n, t, '6', 'S', t, 'E', l, @, n]).
entry(unterste, ['U', n, t, '6', s, t, @]).
entry('Unterstützung', ['U', n, t, '6', 'S', t, 'Y', ts, 'U', 'N']).
entry('Unterteilung', ['U', n, t, '6', t, aI, l, 'U', 'N']).
entry(unterwegs, ['U', n, t, '6', v, 'e:', k, s]).
entry('Unterweisungsprobe', ['U', n, t, '6', v, aI, z, 'U', 'N', s, p, r, 'o:', b, @]).
entry(unterzubringen, ['U', n, t, '6', ts, u, b, r, 'I', 'N', @, n]).
entry(unterzukommen, ['U', n, t, '6', ts, u, k, 'O', m, @, n]).
entry(unübersichtlich, ['U', n, 'y:', b, '6', z, 'I', 'C', t, l, 'I', 'C']).
entry(unüblich, ['U', n, 'y:', p, l, 'I', 'C']).
entry(unverschämt, ['U', n, f, 'E', '6', 'S', 'E:', m, t]).
entry('Unves', ['U', n, v, @, s]).
entry(unvorbereitet, ['U', n, f, 'o:', '6', b, @, r, aI, t, @, t]).
entry(unwahrscheinlich, ['U', n, v, 'a:', r, 'S', aI, n, l, 'I', 'C']).
entry('Unweltschützgründen', ['U', n, v, 'E', l, tS, 'Y', ts, g, r, 'Y', n, d, @, n]).
entry(unwesentlich, ['U', n, v, 'e:', z, @, n, t, l, 'I', 'C']).
entry(unwichtig, ['U', n, v, 'I', 'C', t, 'I', 'C']).
entry(unwirksam, ['U', n, v, 'I', '6', k, z, 'a:', m]).
entry(unzufrieden, ['U', n, ts, u, f, r, 'i:', d, @, n]).
entry(unzumutbar, ['U', n, ts, u, m, 'u:', t, b, 'a:', r]).
entry('up-to-date', [a, p, t, u, d, 'e:', t]).
entry(ürd, ['y:', '6', t]).
entry('Urgroßvater', ['u:', '6', g, r, 'o:', s, f, 'a:', t, '6']).
entry('Urlaub', ['u:', '6', l, aU, p]).
entry('Urlaubserinnerungen', ['u:', '6', l, aU, p, s, 'E', '6', 'I', n, '6', r, 'U', 'N', @, n]).
entry('Urlaubsmonat', ['u:', '6', l, aU, p, s, m, 'o:', n, a, t]).
entry('Urlaubs', ['u:', '6', l, aU, p, s]).
entry('Urlaubsreise', ['u:', '6', l, aU, p, s, r, aI, z, @]).
entry('Urlaubstages', ['u:', '6', l, aU, p, s, t, 'a:', g, @, s]).
entry('Urlaubstag', ['u:', '6', l, aU, p, s, t, 'a:', k]).
entry('Urlaubszeit', ['u:', '6', l, aU, p, s, ts, aI, t]).
entry('Ursache', ['u:', '6', z, a, x, @]).
entry(ür, ['y:', '6']).
entry(üß, ['y:', s]).
entry('Uta', ['u:', t, a]).
entry(uten, ['u:', t, @, n]).
entry('Ute', ['u:', t, @]).
entry(ut, ['u:', t]).
entry(ü, ['y:']).
entry(vage, [v, 'a:', g, @]).
entry('Vahrenwald', [f, 'a:', r, @, n, v, a, l, t]).
entry('Valentinstag', [v, a, l, 'E', n, t, 'i:', n, s, t, 'a:', k]).
entry('Vantroyen', [f, a, n, t, r, 'OY', @, n]).
entry(variabel, [v, a, r, j, 'a:', b, @, l]).
entry('Variante', [v, a, r, j, a, n, t, @]).
entry('Variationen', [v, a, r, j, a, ts, j, 'o:', n, @, n]).
entry('Variete', [v, a, r, j, @, t, 'e:']).
entry(variieren, [v, a, r, i, 'i:', r, @, n]).
entry(variiert, [v, a, r, i, 'i:', '6', t]).
entry('Vater', [f, 'a:', t, '6']).
entry('Vatertag', [f, 'a:', t, '6', t, 'a:', k]).
entry('Vegetarier', [v, e, g, e, t, 'a:', r, j, '6']).
entry(vegetarisches, [v, e, g, e, t, 'a:', r, 'I', 'S', @, s]).
entry('Veilchen', [f, aI, l, 'C', @, n]).
entry('Venedig', [v, e, n, 'e:', d, 'I', 'C']).
entry(verabein, [f, 'E', '6', 'a:', b, aI, n]).
entry(verab, [f, 'E', '6', a, p]).
entry(verabreden, [f, 'E', '6', a, p, r, 'e:', d, @, n]).
entry(verabredeten, [f, 'E', '6', a, p, r, 'e:', d, @, t, @, n]).
entry(verabredet, [f, 'E', '6', a, p, r, 'e:', d, @, t]).
entry('Verabredungen', [f, 'E', '6', a, p, r, 'e:', d, 'U', 'N', @, n]).
entry('Verabredung', [f, 'E', '6', a, p, r, 'e:', d, 'U', 'N']).
entry(verabre, [f, 'E', '6', a, p, r, e]).
entry(verabschiede, [f, 'E', '6', a, p, 'S', 'i:', d, @]).
entry(verabschieden, [f, 'E', '6', a, p, 'S', 'i:', d, @, n]).
entry(verachten, [f, 'E', '6', a, x, t, @, n]).
entry(verändern, [f, 'E', '6', 'E', n, d, '6', n]).
entry(verändert, [f, 'E', '6', 'E', n, d, '6', t]).
entry(veranlasse, [f, 'E', '6', a, n, l, a, s, @]).
entry(veranlassen, [f, 'E', '6', a, n, l, a, s, @, n]).
entry(veranschlagen, [f, 'E', '6', a, n, 'S', l, 'a:', g, @, n]).
entry(veranschlagt, [f, 'E', '6', a, n, 'S', l, 'a:', k, t]).
entry('Veranschlagung', [f, 'E', '6', a, n, 'S', l, 'a:', g, 'U', 'N']).
entry(veranstalten, [f, 'E', '6', a, n, 'S', t, a, l, t, @, n]).
entry('Veranstalter', [f, 'E', '6', a, n, 'S', t, a, l, t, '6']).
entry('Veranstaltungen', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', @, n]).
entry(veranstaltung, [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N']).
entry('Veranstaltungskalender', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', s, k, a, l, 'E', n, d, '6']).
entry('Veranstaltungskalenders', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', s, k, a, l, 'E', n, d, '6', s]).
entry('Veranstaltungsplan', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', s, p, l, 'a:', n]).
entry('Veranstaltungsprogramm', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', s, p, r, o, g, r, a, m]).
entry('Veranstaltungstermine', [f, 'E', '6', a, n, 'S', t, a, l, t, 'U', 'N', s, t, 'E', '6', m, 'i:', n, @]).
entry(verarbeiten, [f, 'E', '6', a, r, b, aI, t, @, n]).
entry(verbessern, [f, 'E', '6', b, 'E', s, '6', n]).
entry(verbinden, [f, 'E', '6', b, 'I', n, d, @, n]).
entry(verbindet, [f, 'E', '6', b, 'I', n, d, @, t]).
entry(verbindlich, [f, 'E', '6', b, 'I', n, t, l, 'I', 'C']).
entry('Verbindungen', [f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry(verbindung, [f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry(verbirgt, [f, 'E', '6', b, 'I', '6', k, t]).
entry(verbleibe, [f, 'E', '6', b, l, aI, b, @]).
entry(verbleibende, [f, 'E', '6', b, l, aI, b, @, n, d, @]).
entry(verbleiben, [f, 'E', '6', b, l, aI, b, @, n]).
entry(verbleibt, [f, 'E', '6', b, l, aI, p, t]).
entry(verbl, [f, 'E', '6', b, l]).
entry('Verbmobil', [v, 'E', '6', p, m, o, b, 'i:', l]).
entry(verbracht, [f, 'E', '6', b, r, a, x, t]).
entry(verbraten, [f, 'E', '6', b, r, 'a:', t, @, n]).
entry(verbrauchen, [f, 'E', '6', b, r, aU, x, @, n]).
entry(verbringe, [f, 'E', '6', b, r, 'I', 'N', @]).
entry(verbringen, [f, 'E', '6', b, r, 'I', 'N', @, n]).
entry(verbucht, [f, 'E', '6', b, 'u:', x, t]).
entry(verbundenen, [f, 'E', '6', b, 'U', n, d, @, n, @, n]).
entry(verbunden, [f, 'E', '6', b, 'U', n, d, @, n]).
entry(verdammt, [f, 'E', '6', d, a, m, t]).
entry(verdeckt, [f, 'E', '6', d, 'E', k, t]).
entry(verdient, [f, 'E', '6', d, 'i:', n, t]).
entry(verdreht, [f, 'E', '6', d, r, 'e:', t]).
entry(verei, [f, 'E', '6', aI]).
entry(vereinba, [f, 'E', '6', aI, n, b, a]).
entry(vereinbaren, [f, 'E', '6', aI, n, b, 'a:', r, @, n]).
entry(vereinbar, [f, 'E', '6', aI, n, b, 'a:', r]).
entry(vereinbarten, [f, 'E', '6', aI, n, b, 'a:', r, t, @, n]).
entry(vereinbart, [f, 'E', '6', aI, n, b, 'a:', r, t]).
entry('Vereinbarungen', [f, 'E', '6', aI, n, b, 'a:', r, 'U', 'N', @, n]).
entry('Vereinbarung', [f, 'E', '6', aI, n, b, 'a:', r, 'U', 'N']).
entry(verein, [f, 'E', '6', aI, n]).
entry(vereinzelt, [f, 'E', '6', aI, n, ts, @, l, t]).
entry(vereinzubaren, [f, 'E', '6', aI, n, ts, u, b, 'a:', r, @, n]).
entry('Verena', [v, e, r, 'e:', n, a]).
entry(vereulnbaren, [f, 'E', '6', 'OY', l, n, b, 'a:', r, @, n]).
entry(verfahre, [f, 'E', '6', f, 'a:', r, @]).
entry(verfallen, [f, 'E', '6', f, a, l, @, n]).
entry(verfassen, [f, 'E', '6', f, a, s, @, n]).
entry(ver, [f, 'E', '6']).
entry(verfehlt, [f, 'E', '6', f, 'e:', l, t]).
entry(verflixte, [f, 'E', '6', f, l, 'I', k, s, t, @]).
entry(verfrüht, [f, 'E', '6', f, r, 'y:', t]).
entry(verfügbaren, [f, 'E', '6', f, 'y:', k, b, 'a:', r, @, n]).
entry(verfügbar, [f, 'E', '6', f, 'y:', k, b, 'a:', r]).
entry('Verfügbarkeit', [f, 'E', '6', f, 'y:', k, b, 'a:', r, k, aI, t]).
entry(verfügen, [f, 'E', '6', f, 'y:', g, @, n]).
entry(verfügten, [f, 'E', '6', f, 'y:', k, t, @, n]).
entry(verfügt, [f, 'E', '6', f, 'y:', k, t]).
entry('Verfügung', [f, 'E', '6', f, 'y:', g, 'U', 'N']).
entry('Vergangenheit', [f, 'E', '6', g, a, 'N', @, n, h, aI, t]).
entry(vergaß, [f, 'E', '6', g, 'a:', s]).
entry(vergeben, [f, 'E', '6', g, 'e:', b, @, n]).
entry(vergeht, [f, 'E', '6', g, 'e:', t]).
entry(vergesse, [f, 'E', '6', g, 'E', s, @]).
entry(vergessen, [f, 'E', '6', g, 'E', s, @, n]).
entry(vergeuden, [f, 'E', '6', g, 'OY', d, @, n]).
entry(verg, [f, 'E', '6', g]).
entry(vergiß, [f, 'E', '6', g, 'I', s]).
entry(vergißt, [f, 'E', '6', g, 'I', s, t]).
entry(vergleichbar, [f, 'E', '6', g, l, aI, 'C', b, 'a:', r]).
entry(vergleichen, [f, 'E', '6', g, l, aI, 'C', @, n]).
entry('Vergleich', [f, 'E', '6', g, l, aI, 'C']).
entry(vergleichsweise, [f, 'E', '6', g, l, aI, 'C', s, v, aI, z, @]).
entry(vergnüge, [f, 'E', '6', g, n, 'y:', g, @]).
entry(vergnügen, [f, 'E', '6', g, n, 'y:', g, @, n]).
entry('Vergnügungen', [f, 'E', '6', g, n, 'y:', g, 'U', 'N', @, n]).
entry(vergraulen, [f, 'E', '6', g, r, aU, l, @, n]).
entry(verguckt, [f, 'E', '6', g, 'U', k, t]).
entry(vergünstigt, [f, 'E', '6', g, 'Y', n, s, t, 'I', 'C', t]).
entry(verhält, [f, 'E', '6', h, 'E', l, t]).
entry('Verhältnis', [f, 'E', '6', h, 'E', l, t, n, 'I', s]).
entry(verhältnismäßig, [f, 'E', '6', h, 'E', l, t, n, 'I', s, m, 'E:', s, 'I', 'C']).
entry(verhandeln, [f, 'E', '6', h, a, n, d, @, l, n]).
entry(verhandlungen, [f, 'E', '6', h, a, n, d, l, 'U', 'N', @, n]).
entry(verhandlungsfähig, [f, 'E', '6', h, a, n, d, l, 'U', 'N', s, f, 'E:', 'I', 'C']).
entry(verhaspeln, [f, 'E', '6', h, a, s, p, @, l, n]).
entry(verheiratet, [f, 'E', '6', h, aI, r, a, t, @, t]).
entry(verhext, [f, 'E', '6', h, 'E', k, s, t]).
entry(verhindern, [f, 'E', '6', h, 'I', n, d, '6', n]).
entry(verhindert, [f, 'E', '6', h, 'I', n, d, '6', t]).
entry(verhört, [f, 'E', '6', h, '2:', '6', t]).
entry(verkappter, [f, 'E', '6', k, a, p, t, '6']).
entry(verkaufen, [f, 'E', '6', k, aU, f, @, n]).
entry('Verkaufsgespräch', [f, 'E', '6', k, aU, f, s, g, @, 'S', p, r, 'E:', 'C']).
entry('Verkaufsschlager', [f, 'E', '6', k, aU, f, s, 'S', l, 'a:', g, '6']).
entry(verkehren, [f, 'E', '6', k, 'e:', r, @, n]).
entry('Verkehr', [f, 'E', '6', k, 'e:', '6']).
entry('Verkehrsanbindungen', [f, 'E', '6', k, 'e:', '6', s, a, n, b, 'I', n, d, 'U', 'N', @, n]).
entry('Verkehrsanbindung', [f, 'E', '6', k, 'e:', '6', s, a, n, b, 'I', n, d, 'U', 'N']).
entry('Verkehrs', [f, 'E', '6', k, 'e:', '6', s]).
entry(verkehrsgünstig, [f, 'E', '6', k, 'e:', '6', s, g, 'Y', n, s, t, 'I', 'C']).
entry('Verkehrsmittel', [f, 'E', '6', k, 'e:', '6', s, m, 'I', t, @, l]).
entry('Verkehrsmitteln', [f, 'E', '6', k, 'e:', '6', s, m, 'I', t, @, l, n]).
entry('Verkehrsmittels', [f, 'E', '6', k, 'e:', '6', s, m, 'I', t, @, l, s]).
entry(verkehrstechnisch, [f, 'E', '6', k, 'e:', '6', s, t, 'E', 'C', n, 'I', 'S']).
entry('Verkehrsverbindungen', [f, 'E', '6', k, 'e:', '6', s, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry('Verkehrsverbindung', [f, 'E', '6', k, 'e:', '6', s, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry(verkehrt, [f, 'E', '6', k, 'e:', '6', t]).
entry(verkraften, [f, 'E', '6', k, r, a, f, t, @, n]).
entry(verkrümeln, [f, 'E', '6', k, r, 'y:', m, @, l, n]).
entry(verkuckt, [f, 'E', '6', k, 'U', k, t]).
entry(verkürzen, [f, 'E', '6', k, 'Y', '6', ts, @, n]).
entry('Verlag', [f, 'E', '6', l, 'a:', k]).
entry('Verlagsleiter', [f, 'E', '6', l, 'a:', k, s, l, aI, t, '6']).
entry(verlangen, [f, 'E', '6', l, a, 'N', @, n]).
entry(verlängern, [f, 'E', '6', l, 'E', 'N', '6', n]).
entry(verlängerte, [f, 'E', '6', l, 'E', 'N', '6', t, @]).
entry(verlängertes, [f, 'E', '6', l, 'E', 'N', '6', t, @, s]).
entry('Verlängerung', [f, 'E', '6', l, 'E', 'N', '6', r, 'U', 'N']).
entry(verlangt, [f, 'E', '6', l, a, 'N', t]).
entry(verlasse, [f, 'E', '6', l, a, s, @]).
entry(verlassen, [f, 'E', '6', l, a, s, @, n]).
entry(verläßt, [f, 'E', '6', l, 'E', s, t]).
entry('Verlaufe', [f, 'E', '6', l, aU, f, @]).
entry(verlaufen, [f, 'E', '6', l, aU, f, @, n]).
entry('Verlauf', [f, 'E', '6', l, aU, f]).
entry(verläuft, [f, 'E', '6', l, 'OY', f, t]).
entry(verle, [f, 'E', '6', l, e]).
entry(verlegbar, [f, 'E', '6', l, 'e:', k, b, 'a:', r]).
entry(verlegen, [f, 'E', '6', l, 'e:', g, @, n]).
entry('Verlegenheit', [f, 'E', '6', l, 'e:', g, @, n, h, aI, t]).
entry(verlegt, [f, 'E', '6', l, 'e:', k, t]).
entry('Verlegung', [f, 'E', '6', l, 'e:', g, 'U', 'N']).
entry(verlesen, [f, 'E', '6', l, 'e:', z, @, n]).
entry(verliere, [f, 'E', '6', l, 'i:', r, @]).
entry(verlieren, [f, 'E', '6', l, 'i:', r, @, n]).
entry(verlockend, [f, 'E', '6', l, 'O', k, @, n, t]).
entry(verloren, [f, 'E', '6', l, 'o:', r, @, n]).
entry(verlorengehen, [f, 'E', '6', l, 'o:', r, @, n, g, 'e:', @, n]).
entry(vermeiden, [f, 'E', '6', m, aI, d, @, n]).
entry(vermerke, [f, 'E', '6', m, 'E', '6', k, @]).
entry(vermerken, [f, 'E', '6', m, 'E', '6', k, @, n]).
entry(vermerkt, [f, 'E', '6', m, 'E', '6', k, t]).
entry(vermute, [f, 'E', '6', m, 'u:', t, @]).
entry(vermuten, [f, 'E', '6', m, 'u:', t, @, n]).
entry(vermutlich, [f, 'E', '6', m, 'u:', t, l, 'I', 'C']).
entry('Vermutung', [f, 'E', '6', m, 'u:', t, 'U', 'N']).
entry(verneinen, [f, 'E', '6', n, aI, n, @, n]).
entry(vernommen, [f, 'E', '6', n, 'O', m, @, n]).
entry(vernünftige, [f, 'E', '6', n, 'Y', n, f, t, 'I', g, @]).
entry(vernünftigen, [f, 'E', '6', n, 'Y', n, f, t, 'I', g, @, n]).
entry(vernünftiger, [f, 'E', '6', n, 'Y', n, f, t, 'I', g, '6']).
entry(vernünftig, [f, 'E', '6', n, 'Y', n, f, t, 'I', 'C']).
entry(vernünftigste, [f, 'E', '6', n, 'Y', n, f, t, 'I', 'C', s, t, @]).
entry('Veröffentlichung', [f, 'E', '6', '9', f, @, n, t, l, 'I', 'C', 'U', 'N']).
entry(verpassen, [f, 'E', '6', p, a, s, @, n]).
entry(verpflegen, [f, 'E', '6', pf, l, 'e:', g, @, n]).
entry('Verpflegung', [f, 'E', '6', pf, l, 'e:', g, 'U', 'N']).
entry(verpflichtet, [f, 'E', '6', pf, l, 'I', 'C', t, @, t]).
entry('Verpflichtun', [f, 'E', '6', pf, l, 'I', 'C', t, 'U', n]).
entry('Verpflichtungen', [f, 'E', '6', pf, l, 'I', 'C', t, 'U', 'N', @, n]).
entry('Verpflichtung', [f, 'E', '6', pf, l, 'I', 'C', t, 'U', 'N']).
entry(verplanen, [f, 'E', '6', p, l, 'a:', n, @, n]).
entry(verplant, [f, 'E', '6', p, l, 'a:', n, t]).
entry(verplempern, [f, 'E', '6', p, l, 'E', m, p, '6', n]).
entry(verraten, [f, 'E', '6', r, 'a:', t, @, n]).
entry(verraucht, [f, 'E', '6', r, aU, x, t]).
entry(verräumt, [f, 'E', '6', r, 'OY', m, t]).
entry(verrechne, [f, 'E', '6', r, 'E', 'C', n, @]).
entry(verrechnet, [f, 'E', '6', r, 'E', 'C', n, @, t]).
entry(verreise, [f, 'E', '6', r, aI, z, @]).
entry(verreisen, [f, 'E', '6', r, aI, z, @, n]).
entry(verreist, [f, 'E', '6', r, aI, s, t]).
entry(verrückter, [f, 'E', '6', r, 'Y', k, t, '6']).
entry(verrückt, [f, 'E', '6', r, 'Y', k, t]).
entry(verrutscht, [f, 'E', '6', r, 'U', tS, t]).
entry('Versammlung', [f, 'E', '6', z, a, m, l, 'U', 'N']).
entry(versäumen, [f, 'E', '6', z, 'OY', m, @, n]).
entry(verschärft, [f, 'E', '6', 'S', 'E', '6', f, t]).
entry(verschaut, [f, 'E', '6', 'S', aU, t]).
entry(versch, [f, 'E', '6', 'S']).
entry(verschiebbar, [f, 'E', '6', 'S', 'i:', p, b, 'a:', r]).
entry(verschiebe, [f, 'E', '6', 'S', 'i:', b, @]).
entry(verschieben, [f, 'E', '6', 'S', 'i:', b, @, n]).
entry(verschiebt, [f, 'E', '6', 'S', 'i:', p, t]).
entry(verschiedene, [f, 'E', '6', 'S', 'i:', d, @, n, @]).
entry(verschiedenen, [f, 'E', '6', 'S', 'i:', d, @, n, @, n]).
entry(verschlagen, [f, 'E', '6', 'S', l, 'a:', g, @, n]).
entry(verschlägt, [f, 'E', '6', 'S', l, 'E:', k, t]).
entry(verschlossenen, [f, 'E', '6', 'S', l, 'O', s, @, n, @, n]).
entry(verschmutzt, [f, 'E', '6', 'S', m, 'U', ts, t]).
entry(verschnörkelt, [f, 'E', '6', 'S', n, '9', '6', k, @, l, t]).
entry(verschnupft, [f, 'E', '6', 'S', n, 'U', pf, t]).
entry(verschoben, [f, 'E', '6', 'S', 'o:', b, @, n]).
entry(verschwenden, [f, 'E', '6', 'S', v, 'E', n, d, @, n]).
entry(verschwiegen, [f, 'E', '6', 'S', v, 'i:', g, @, n]).
entry(versehen, [f, 'E', '6', z, 'e:', @, n]).
entry(versetzen, [f, 'E', '6', z, 'E', ts, @, n]).
entry(versetzt, [f, 'E', '6', z, 'E', ts, t]).
entry('Vers', [f, 'E', '6', s]).
entry(versichere, [f, 'E', '6', z, 'I', 'C', @, r, @]).
entry(versichern, [f, 'E', '6', z, 'I', 'C', '6', n]).
entry('Version', [v, 'E', '6', z, j, 'o:', n]).
entry(versorgen, [f, 'E', '6', z, 'O', '6', g, @, n]).
entry(versorgt, [f, 'E', '6', z, 'O', '6', k, t]).
entry(verspätet, [f, 'E', '6', 'S', p, 'E:', t, @, t]).
entry('Verspätungen', [f, 'E', '6', 'S', p, 'E:', t, 'U', 'N', @, n]).
entry('Verspätung', [f, 'E', '6', 'S', p, 'E:', t, 'U', 'N']).
entry(versprechend, [f, 'E', '6', 'S', p, r, 'E', 'C', @, n, t]).
entry(versprechen, [f, 'E', '6', 'S', p, r, 'E', 'C', @, n]).
entry('Versprechung', [f, 'E', '6', 'S', p, r, 'E', 'C', 'U', 'N']).
entry(verspricht, [f, 'E', 'E', '6', 'S', p, r, 'I', 'C', t]).
entry(versprochen, [f, 'E', '6', 'S', p, r, 'O', x, @, n]).
entry(versta, [f, 'E', '6', 'S', t, a]).
entry(verstanden, [f, 'E', '6', 'S', t, a, n, d, @, n]).
entry(verständige, [f, 'E', '6', 'S', t, 'E', n, d, 'I', g, @]).
entry(verständigen, [f, 'E', '6', 'S', t, 'E', n, d, 'I', g, @, n]).
entry(verständlich, [f, 'E', '6', 'S', t, 'E', n, t, l, 'I', 'C']).
entry('Verständnis', [f, 'E', '6', 'S', t, 'E', n, t, n, 'I', s]).
entry(versteckt, [f, 'E', '6', 'S', t, 'E', k, t]).
entry(verstehe, [f, 'E', '6', 'S', t, 'e:', @]).
entry(verstehen, [f, 'E', '6', 'S', t, 'e:', @, n]).
entry(verstehst, [f, 'E', '6', 'S', t, 'e:', s, t]).
entry(versteigern, [f, 'E', '6', 'S', t, aI, g, '6', n]).
entry(verstreichen, [f, 'E', '6', 'S', t, r, aI, 'C', @, n]).
entry(verstunken, [f, 'E', '6', s, t, 'U', 'N', k, @, n]).
entry(versuche, [f, 'E', '6', z, 'u:', x, @]).
entry(versuchen, [f, 'E', '6', z, 'u:', x, @, n]).
entry(versuch, [f, 'E', '6', z, 'u:', x]).
entry(versucht, [f, 'E', '6', z, 'u:', x, t]).
entry(versumpfen, [f, 'E', '6', z, 'U', m, pf, @, n]).
entry(vertagen, [f, 'E', '6', t, 'a:', g, @, n]).
entry(vertan, [f, 'E', '6', t, 'a:', n]).
entry(vertauscht, [f, 'E', '6', t, aU, 'S', t]).
entry(verteilen, [f, 'E', '6', t, aI, l, @, n]).
entry(verteilte, [f, 'E', '6', t, aI, l, t, @]).
entry(verteilten, [f, 'E', '6', t, aI, l, t, @, n]).
entry(verteilt, [f, 'E', '6', t, aI, l, t]).
entry('Vert', [f, 'E', '6', t]).
entry('Vertrag', [f, 'E', '6', t, r, 'a:', k]).
entry(vertraue, [f, 'E', '6', t, r, aU, @]).
entry(vertrauen, [f, 'E', '6', t, r, aU, @, n]).
entry(vertreiben, [f, 'E', '6', t, r, aI, b, @, n]).
entry(vertretbar, [f, 'E', '6', t, r, 'e:', t, b, 'a:', r]).
entry(vertreten, [f, 'E', '6', t, r, 'e:', t, @, n]).
entry('Vertretung', [f, 'E', '6', t, r, 'e:', t, 'U', 'N']).
entry(vertr, [f, 'E', '6', t, r]).
entry('Vertriebsleiter', [f, 'E', '6', t, r, 'i:', p, s, l, aI, t, '6']).
entry('Vertriebssystem', [f, 'E', '6', t, r, 'i:', p, s, z, 'Y', s, t, 'e:', m]).
entry(verunglücken, [f, 'E', '6', 'U', n, g, l, 'Y', k, @, n]).
entry(verurlaubt, [f, 'E', '6', 'U', '6', l, aU, p, t]).
entry(vervollständigen, [f, 'E', '6', f, 'O', l, 'S', t, 'E', n, d, 'I', g, @, n]).
entry('Verwandten', [f, 'E', '6', v, a, n, t, @, n]).
entry('Verwandtschaft', [f, 'E', '6', v, a, n, tS, a, f, t]).
entry(verwechselt, [f, 'E', '6', v, 'E', k, s, @, l, t]).
entry(verweilen, [f, 'E', '6', v, aI, l, @, n]).
entry(verwenden, [f, 'E', '6', v, 'E', n, d, @, n]).
entry(verwendet, [f, 'E', '6', v, 'E', n, d, @, t]).
entry('Verwicklungen', [f, 'E', '6', v, 'I', k, l, 'U', 'N', @, n]).
entry(verwirrt, [f, 'E', '6', v, 'I', '6', t]).
entry(verwöhnen, [f, 'E', '6', v, '2:', n, @, n]).
entry(verzeihen, [f, 'E', '6', ts, aI, @, n]).
entry('Verzeihung', [f, 'E', '6', ts, aI, 'U', 'N']).
entry(verzichte, [f, 'E', '6', ts, 'I', 'C', t, @]).
entry(verzichten, [f, 'E', '6', ts, 'I', 'C', t, @, n]).
entry('Verzögerungen', [f, 'E', '6', ts, '2:', g, @, r, 'U', 'N', @, n]).
entry('Verzögerung', [f, 'E', '6', ts, '2:', g, @, r, 'U', 'N']).
entry(ve, [v, e]).
entry('V', [f]).
entry(v, [f, aU]).
entry(vibrations, [v, aI, b, r, 'e:', 'S', @, n, s]).
entry('Videoraum', [v, 'i:', d, e, o, r, aU, m]).
entry('Videos', [v, 'i:', d, e, o, s]).
entry('Video', [v, 'i:', d, e, o]).
entry(vielbeschäftigter, [f, 'i:', l, b, @, 'S', 'E', f, t, 'I', 'C', t, '6']).
entry(vielbeschäftigt, [f, 'i:', l, b, @, 'S', 'E', f, t, 'I', 'C', t]).
entry(vielen, [f, 'i:', l, @, n]).
entry('Vielfaches', [f, 'i:', l, f, a, x, @, s]).
entry(vielfältige, [f, 'i:', l, f, 'E', l, t, 'I', g, @]).
entry(vielfältig, [f, 'i:', l, f, 'E', l, t, 'I', 'C']).
entry('Vielflieger-Angebot', [f, 'i:', l, f, l, 'i:', g, '6', a, n, g, @, b, 'o:', t]).
entry(vielleich, [f, i, l, aI, 'C']).
entry(vielleicht, [f, i, l, aI, 'C', t]).
entry(viellei, [f, i, l, aI]).
entry(viell, [f, i, l]).
entry(vielmals, [f, 'i:', l, m, 'a:', l, s]).
entry(vielmehr, [f, 'i:', l, m, 'e:', '6']).
entry(vielversprechend, [f, 'i:', l, f, 'E', '6', 'S', p, r, 'E', 'C', @, n, t]).
entry(viereinhalb, [f, 'i:', '6', aI, n, h, a, l, p]).
entry(viereinhalbstündige, [f, 'i:', '6', aI, n, h, a, l, p, 'S', t, 'Y', n, d, 'I', g, @]).
entry(vieren, [f, 'i:', r, @, n]).
entry('Vierer-Platz', [f, 'i:', r, '6', p, l, a, ts]).
entry(vierhundert, [f, 'I', '6', h, 'U', n, d, '6', t]).
entry('Vier-Jahreszeiten', [f, 'i:', '6', j, 'a:', r, @, s, ts, aI, t, @, n]).
entry(viermal, [f, 'i:', r, m, 'a:', l]).
entry(viertägige, [f, 'i:', '6', t, 'E:', g, 'I', g, @]).
entry(viertägiges, [f, 'i:', '6', t, 'E:', g, 'I', g, @, s]).
entry(vierte, [f, 'i:', '6', t, @]).
entry(viertel, [f, 'I', '6', t, @, l]).
entry('Viertelstunde', [f, 'I', '6', t, @, l, 'S', t, 'U', n, d, @]).
entry(vierten, [f, 'i:', '6', t, @, n]).
entry(vierter, [f, 'i:', '6', t, '6']).
entry(viert, [f, 'i:', '6', t]).
entry(vierundachtzig, [f, 'i:', '6', 'U', n, t, a, x, ts, 'I', 'C']).
entry(vierunddreißig, [f, 'i:', '6', 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(vierunddreißigsten, [f, 'i:', '6', 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(vierund, [f, 'i:', '6', 'U', n, t]).
entry(vierundneunzig, [f, 'i:', '6', 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(vierundsechzig, [f, 'i:', '6', 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(vierundvierzigsten, [f, 'i:', '6', 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(vierundz, [f, 'i:', '6', 'U', n, t, ts]).
entry(vierundzwan, [f, 'i:', '6', 'U', n, t, ts, v, a, n]).
entry(vierundzwanz, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts]).
entry(vierundzwanzi, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I']).
entry(vierundzwanzig, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(vierundzwanzigste, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(vierundzwanzigstem, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, m]).
entry(vierundzwanzigsten, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(vierundzwanzigster, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(vierundzwanzigst, [f, 'i:', '6', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t]).
entry(vierun, [f, 'i:', '6', 'U', n]).
entry(vierzehn, [f, 'I', '6', ts, 'e:', n]).
entry('Vierzehn-Tage-Turnus', [f, 'I', '6', ts, 'e:', n, t, 'a:', g, @, t, 'U', '6', n, 'U', s]).
entry(vierzehntägig, [f, 'I', '6', ts, 'e:', n, t, 'E:', g, 'I', 'C']).
entry(vierzehnte, [f, 'I', '6', ts, 'e:', n, t, @]).
entry(vierzehnten, [f, 'I', '6', ts, 'e:', n, t, @, n]).
entry('Vierzehntens', [f, 'I', '6', ts, 'e:', n, t, @, n, s]).
entry(vierzehnter, [f, 'I', '6', ts, 'e:', n, t, '6']).
entry(vierzehnt, [f, 'I', '6', ts, 'e:', n, t]).
entry('Vierzehn-Uhr-Termin', [f, 'I', '6', ts, 'e:', n, 'u:', '6', t, 'E', '6', m, 'i:', n]).
entry(vierz, [f, 'I', '6', ts]).
entry(vierzig, [f, 'I', '6', ts, 'I', 'C']).
entry(vierzigsten, [f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(vie, [v, 'i:']).
entry(vi, [f, i]).
entry('Viktor', [v, 'I', k, t, 'o:', '6']).
entry('Virus', [v, 'i:', r, 'U', s]).
entry('Visa-Card', [v, 'i:', z, a, k, a, r, t]).
entry('Visa-Karte', [v, 'i:', z, a, k, a, r, t, @]).
entry('Visa', [v, 'i:', z, @]).
entry('Visier', [v, i, z, 'i:', '6']).
entry('Visitenkarte', [v, i, z, 'i:', t, @, n, k, a, r, t, @]).
entry(vö, [f, '9']).
entry(vo, [f, 'O']).
entry('Vogel', [f, 'o:', g, @, l]).
entry('Voggesser', [v, 'O', g, 'E', s, '6']).
entry('Vogsch', [f, 'O', g, 'S']).
entry('Vokalperzeption', [v, o, k, 'a:', l, p, 'E', '6', ts, 'E', p, ts, j, 'o:', n]).
entry('Vol', [f, 'O', l]).
entry('Volk', [f, 'O', l, k]).
entry('Volksbühne', [f, 'O', k, s, b, j, 'u:', n, @]).
entry('Volkssternwarte', [f, 'O', l, k, s, 'S', t, 'E', '6', n, v, a, r, t, @]).
entry('Volkstrauertag', [f, 'O', l, k, s, t, r, aU, '6', t, 'a:', k]).
entry('Vollack', [f, 'O', l, a, k]).
entry(vollaufen, [f, 'O', l, l, aU, f, @, n]).
entry(volle, [f, 'O', l, @]).
entry(vollenden, [f, 'O', l, 'E', n, d, @, n]).
entry(vollends, [f, 'O', l, 'E', n, ts]).
entry(vollen, [f, 'O', l, @, n]).
entry(voller, [f, 'O', l, '6']).
entry(volles, [f, 'O', l, @, s]).
entry(vollgepackt, [f, 'O', l, g, @, p, a, k, t]).
entry(vollgeplant, [f, 'O', l, g, @, p, l, 'a:', n, t]).
entry(vollgestopfter, [f, 'O', l, g, @, 'S', t, 'O', pf, t, '6']).
entry(vollgestopft, [f, 'O', l, g, @, 'S', t, 'O', pf, t]).
entry(völlig, [f, '9', l, 'I', 'C']).
entry(vollkommen, [f, 'O', l, k, 'O', m, @, n]).
entry(vollpacken, [f, 'O', l, p, a, k, @, n]).
entry('Vollpension', [f, 'O', l, p, 'E', n, z, j, 'o:', n]).
entry(vollständige, [f, 'O', l, 'S', t, 'E', n, d, 'I', g, @]).
entry(vollständigen, [f, 'O', l, 'S', t, 'E', n, d, 'I', g, @, n]).
entry(vollständig, [f, 'O', l, 'S', t, 'E', n, d, 'I', 'C']).
entry('Vollstedt', [f, 'O', l, 'S', t, 'E', t]).
entry('Volltreffer', [f, 'O', l, t, r, 'E', f, '6']).
entry(vom, [f, 'O', m]).
entry(voneinander, [f, 'O', n, aI, n, a, n, d, '6']).
entry(von, [f, 'O', n]).
entry(vonnöten, [f, 'O', n, n, '2:', t, @, n]).
entry(vonstatten, [f, 'O', n, 'S', t, a, t, @, n]).
entry('von-Sudiz', [f, 'O', n, z, 'u:', d, 'I', ts]).
entry('von-Sudniz', [f, 'O', n, z, 'U', t, n, 'I', ts]).
entry('von-Sundniz', [f, 'O', n, z, 'U', n, t, n, 'I', ts]).
entry('von-Suzniz', [f, 'O', n, z, 'U', s, n, 'I', ts]).
entry('Vorabend', [f, 'o:', '6', 'a:', b, @, n, t]).
entry(vorab, [f, 'o:', '6', a, p]).
entry(vorangehen, [f, o, r, a, n, g, 'e:', @, n]).
entry(vorangeht, [f, o, r, a, n, g, 'e:', t]).
entry('Voranschlag', [f, 'o:', '6', a, n, 'S', l, 'a:', k]).
entry(vorarbeiten, [f, 'o:', '6', a, r, b, aI, t, @, n]).
entry(vorauf, [f, o, r, aU, f]).
entry(vorausfahre, [f, o, r, aU, s, f, 'a:', r, @]).
entry(voraus, [f, o, r, aU, s]).
entry(vorausgegangenen, [f, o, r, aU, s, g, @, g, a, 'N', @, n, @, n]).
entry(vorausgesagt, [f, o, r, aU, s, g, @, z, 'a:', k, t]).
entry(vorausgesetzt, [f, o, r, aU, s, g, @, z, 'E', ts, t]).
entry(vorauslegen, [f, o, r, aU, s, l, 'e:', g, @, n]).
entry(vorausschicken, [f, o, r, aU, s, 'S', 'I', k, @, n]).
entry('Voraussetzung', [f, o, r, aU, s, z, 'E', ts, 'U', 'N']).
entry('Voraussicht', [f, o, r, aU, s, z, 'I', 'C', t]).
entry(voraussichtlich, [f, o, r, aU, s, z, 'I', 'C', t, l, 'I', 'C']).
entry('Vorauszahlung', [f, o, r, aU, s, ts, 'a:', l, 'U', 'N']).
entry(vorbehalten, [f, 'o:', '6', b, @, h, a, l, t, @, n]).
entry('Vorbehalt', [f, 'o:', '6', b, @, h, a, l, t]).
entry(vorbei, [f, 'o:', '6', b, aI]).
entry(vorbeigekommen, [f, 'o:', '6', b, aI, g, @, k, 'O', m, @, n]).
entry(vorbeigeschaut, [f, 'o:', '6', b, aI, g, @, 'S', aU, t]).
entry(vorbeikomme, [f, 'o:', '6', b, aI, k, 'O', m, @]).
entry(vorbeikommen, [f, 'o:', '6', b, aI, k, 'O', m, @, n]).
entry(vorbeikommt, [f, 'o:', '6', b, aI, k, 'O', m, t]).
entry(vorbeikucken, [f, 'o:', '6', b, aI, k, 'U', k, @, n]).
entry(vorbeischauen, [f, 'o:', '6', b, aI, 'S', aU, @, n]).
entry(vorbeizukommen, [f, 'o:', '6', b, aI, ts, u, k, 'O', m, @, n]).
entry(vorberei, [f, 'o:', '6', b, @, r, aI]).
entry(vorbereiten, [f, 'o:', '6', b, @, r, aI, t, @, n]).
entry(vorbereitet, [f, 'o:', '6', b, @, r, aI, t, @, t]).
entry(vorbereitungen, [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', @, n]).
entry(vorbereitung, [f, 'o:', '6', b, @, r, aI, t, 'U', 'N']).
entry('Vorbereitungs', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s]).
entry('Vorbereitungsgespräch', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, g, @, 'S', p, r, 'E:', 'C']).
entry('Vorbereitungsphase', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, f, 'a:', z, @]).
entry('Vorbereitungstermin', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, t, 'E', '6', m, 'i:', n]).
entry('Vorbereitungstermins', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, t, 'E', '6', m, 'i:', n, s]).
entry('Vorbereitungstreffen', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, t, r, 'E', f, @, n]).
entry('Vorbereitungstreffens', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, t, r, 'E', f, @, n, s]).
entry('Vorbereitungstreff', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, t, r, 'E', f]).
entry('Vorbereitungszeit', [f, 'o:', '6', b, @, r, aI, t, 'U', 'N', s, ts, aI, t]).
entry(vorbesprechen, [f, 'o:', '6', b, @, 'S', p, r, 'E', 'C', @, n]).
entry('Vorbesprechung', [f, 'o:', '6', b, @, 'S', p, r, 'E', 'C', 'U', 'N']).
entry('Vorbesprechungstermin', [f, 'o:', '6', b, @, 'S', p, r, 'E', 'C', 'U', 'N', s, t, 'E', '6', m, 'i:', n]).
entry(vorbestellen, [f, 'O', '6', b, @, 'S', t, 'E', l, @, n]).
entry(vorbildlich, [f, 'o:', '6', b, 'I', l, t, l, 'I', 'C']).
entry(vorbuchen, [f, 'o:', '6', b, 'u:', x, @, n]).
entry('Vordergrund', [f, 'O', '6', d, '6', g, r, 'U', n, t]).
entry('Vordermann', [f, 'O', '6', d, '6', m, a, n]).
entry(voreilig, [f, 'o:', '6', aI, l, 'I', 'C']).
entry(vorerst, [f, 'o:', '6', 'e:', '6', s, t]).
entry(vorfahren, [f, 'o:', '6', f, 'a:', r, @, n]).
entry('Vorfeld', [f, 'o:', '6', f, 'E', l, t]).
entry(vor, [f, 'o:', '6']).
entry('Vorführungen', [f, 'o:', '6', f, 'y:', r, 'U', 'N', @, n]).
entry('Vorgabe', [f, 'o:', '6', g, 'a:', b, @]).
entry('Vorgaben', [f, 'o:', '6', g, 'a:', b, @, n]).
entry('Vorgänge', [f, 'o:', '6', g, 'E', 'N', @]).
entry(vorgefunden, [f, 'o:', '6', g, @, f, 'U', n, d, @, n]).
entry(vorgegeben, [f, 'o:', '6', g, @, g, 'e:', b, @, n]).
entry(vorgego, [f, 'o:', '6', g, @, g, 'O']).
entry(vorgehabt, [f, 'o:', '6', g, @, h, 'a:', p, t]).
entry(vorgehen, [f, 'o:', '6', g, 'e:', @, n]).
entry(vorgelegt, [f, 'o:', '6', g, @, l, 'e:', k, t]).
entry(vorgemerkt, [f, 'o:', '6', g, @, m, 'E', '6', k, t]).
entry(vorgenommen, [f, 'o:', '6', g, @, n, 'O', m, @, n]).
entry(vorgeschlagenen, [f, 'o:', '6', g, @, 'S', l, 'a:', g, @, n, @, n]).
entry(vorgeschlagen, [f, 'o:', '6', g, @, 'S', l, 'a:', g, @, n]).
entry(vorgesehen, [f, 'o:', '6', g, @, z, 'e:', @, n]).
entry('Vorgesetzten', [f, 'o:', '6', g, @, z, 'E', ts, t, @, n]).
entry(vorgestellt, [f, 'o:', '6', g, @, 'S', t, 'E', l, t]).
entry(vorgestern, [f, 'o:', '6', g, 'E', s, t, '6', n]).
entry(vorhabe, [f, 'o:', '6', h, 'a:', b, @]).
entry(vorhaben, [f, 'o:', '6', h, 'a:', b, @, n]).
entry(vorhanden, [f, 'o:', '6', h, a, n, d, @, n]).
entry(vorhat, [f, 'o:', '6', h, a, t]).
entry(vorher, [f, 'o:', '6', h, 'e:', '6']).
entry(vorhergehende, [f, 'o:', '6', h, 'e:', '6', g, 'e:', @, n, d, @]).
entry(vorhergesagt, [f, 'o:', '6', h, 'e:', '6', g, @, z, 'a:', k, t]).
entry(vorherigen, [f, 'o:', '6', h, 'e:', r, 'I', g, @, n]).
entry(vorhin, [f, 'o:', '6', h, 'I', n]).
entry(vorige, [f, 'o:', r, 'I', g, @]).
entry(vorigen, [f, 'o:', r, 'I', g, @, n]).
entry(vorklären, [f, 'o:', '6', k, l, 'E:', r, @, n]).
entry(vorkommen, [f, 'o:', '6', k, 'O', m, @, n]).
entry('Vorlage', [f, 'o:', '6', l, 'a:', g, @]).
entry(vorläufig, [f, 'o:', '6', l, 'OY', f, 'I', 'C']).
entry('Vorlaufzeit', [f, 'o:', '6', l, aU, f, ts, aI, t]).
entry(vorlesen, [f, 'o:', '6', l, 'e:', z, @, n]).
entry('Vorlesungen', [f, 'o:', '6', l, 'e:', z, 'U', 'N', @, n]).
entry('Vorlesung', [f, 'o:', '6', l, 'e:', z, 'U', 'N']).
entry(vorlesungsfreie, [f, 'o:', '6', l, 'e:', z, 'U', 'N', s, f, r, aI, @]).
entry(vorletzte, [f, 'o:', '6', l, 'E', ts, t, @]).
entry(vorletzten, [f, 'o:', '6', l, 'E', ts, t, @, n]).
entry('Vorliebe', [f, 'o:', '6', l, 'i:', b, @]).
entry('Vorlieben', [f, 'o:', '6', l, 'i:', b, @, n]).
entry(vorliebnehmen, [f, 'o:', '6', l, 'i:', p, n, 'e:', m, @, n]).
entry(vorliegende, [f, 'o:', '6', l, 'i:', g, @, n, d, @]).
entry(vorliegen, [f, 'o:', '6', l, 'i:', g, @, n]).
entry(vorliegt, [f, 'o:', '6', l, 'i:', k, t]).
entry(vormachen, [f, 'o:', '6', m, a, x, @, n]).
entry(vormerken, [f, 'o:', '6', m, 'E', '6', k, @, n]).
entry(vorm, [f, 'o:', '6', m]).
entry('Vormittages', [f, 'o:', '6', m, 'I', t, 'a:', g, @, s]).
entry(vormittag, [f, 'o:', '6', m, 'I', t, 'a:', k]).
entry('Vormittägliches', [f, 'o:', '6', m, 'I', t, 'E:', k, l, 'I', 'C', @, s]).
entry(vormittags, [f, 'o:', '6', m, 'I', t, 'a:', k, s]).
entry('Vormittags-Maschine', [f, 'o:', '6', m, 'I', t, 'a:', k, s, m, a, 'S', 'i:', n, @]).
entry('Vormittagsstunden', [f, 'o:', '6', m, 'I', t, 'a:', k, s, 'S', t, 'U', n, d, @, n]).
entry('Vormittagstermin', [f, 'o:', '6', m, 'I', t, 'a:', k, s, t, 'E', '6', m, 'i:', n]).
entry('Vorname', [f, 'o:', '6', n, 'a:', m, @]).
entry(vorne, [f, 'O', '6', n, @]).
entry(vornehmen, [f, 'o:', '6', n, 'e:', m, @, n]).
entry(vornehmer, [f, 'o:', '6', n, 'e:', m, '6']).
entry(vornehmes, [f, 'o:', '6', n, 'e:', m, @, s]).
entry(vornehmlich, [f, 'o:', '6', n, 'e:', m, l, 'I', 'C']).
entry(vornherein, [f, 'O', '6', n, h, 'E', r, aI, n]).
entry('Vornotierungs-Kalender', [f, 'o:', '6', n, o, t, 'i:', r, 'U', 'N', s, k, a, l, 'E', n, d, '6']).
entry(vorplanen, [f, 'o:', '6', p, l, 'a:', n, @, n]).
entry('Vorplaner', [f, 'o:', '6', p, l, 'a:', n, '6']).
entry('Vorrang', [f, 'o:', '6', r, a, 'N']).
entry('Vorrat', [f, 'o:', '6', r, 'a:', t]).
entry(vorreservieren, [f, 'o:', '6', r, e, z, 'E', '6', v, 'i:', r, @, n]).
entry(vorschieben, [f, 'o:', '6', 'S', 'i:', b, @, n]).
entry(vorschla, [f, 'o:', '6', 'S', l, a]).
entry(vorschlage, [f, 'o:', '6', 'S', l, 'a:', g, @]).
entry(vorschläge, [f, 'o:', '6', 'S', l, 'E:', g, @]).
entry(vorschlagen, [f, 'o:', '6', 'S', l, 'a:', g, @, n]).
entry('Vorschlägen', [f, 'o:', '6', 'S', l, 'E:', g, @, n]).
entry(vorschlag, [f, 'o:', '6', 'S', l, 'a:', k]).
entry(vorschlegen, [f, 'o:', '6', 'S', l, 'e:', g, @, n]).
entry(vorschweben, [f, 'o:', '6', 'S', v, 'e:', b, @, n]).
entry(vorsehen, [f, 'o:', '6', z, 'e:', @, n]).
entry(vorsichtig, [f, 'o:', '6', z, 'I', 'C', t, 'I', 'C']).
entry('Vorstandschef', [f, 'o:', '6', 'S', t, a, n, ts, 'S', 'E', f]).
entry('Vorstandsrat', [f, 'o:', '6', 'S', t, a, n, ts, r, 'a:', t]).
entry('Vorstandssitzung', [f, 'o:', '6', 'S', t, a, n, ts, z, 'I', ts, 'U', 'N']).
entry(vorstelle, [f, 'o:', '6', 'S', t, 'E', l, @]).
entry(vorstellen, [f, 'o:', '6', 'S', t, 'E', l, @, n]).
entry('Vorstellungen', [f, 'o:', '6', 'S', t, 'E', l, 'U', 'N', @, n]).
entry('Vorstellung', [f, 'o:', '6', 'S', t, 'E', l, 'U', 'N']).
entry('Vorstellungsgespräch', [f, 'o:', '6', 'S', t, 'E', l, 'U', 'N', s, g, @, 'S', p, r, 'E:', 'C']).
entry('Vortages', [f, 'o:', '6', t, 'a:', g, @, s]).
entry(vortanzen, [f, 'o:', '6', t, a, n, ts, @, n]).
entry('Vorteile', [f, 'o:', '6', t, aI, l, @]).
entry('Vorteil', [f, 'o:', '6', t, aI, l]).
entry(vorteilhafter, [f, 'o:', '6', t, aI, l, h, a, f, t, '6']).
entry(vorteilhaft, [f, 'o:', '6', t, aI, l, h, a, f, t]).
entry('Vorträge', [f, 'o:', '6', t, r, 'E:', g, @]).
entry(vortragen, [f, 'o:', '6', t, r, 'a:', g, @, n]).
entry('Vortrag', [f, 'o:', '6', t, r, 'a:', k]).
entry('Vortragsreihe', [f, 'o:', '6', t, r, 'a:', k, s, r, aI, @]).
entry(vortreffen, [f, 'o:', '6', t, r, 'E', f, @, n]).
entry('Vortritt', [f, 'o:', '6', t, r, 'I', t]).
entry('Vorverkaufsgebühr', [f, 'o:', '6', f, 'E', '6', k, aU, f, s, g, @, b, 'y:', '6']).
entry(vorverlegen, [f, 'o:', '6', f, 'E', '6', l, 'e:', g, @, n]).
entry(vorverschieben, [f, 'o:', '6', f, 'E', '6', 'S', 'i:', b, @, n]).
entry(vorweg, [f, 'o:', '6', v, 'E', k]).
entry('Vorweihnachtszeit', [f, 'o:', '6', v, aI, n, a, x, ts, ts, aI, t]).
entry(vorzeigen, [f, 'o:', '6', ts, aI, g, @, n]).
entry(vorziehen, [f, 'o:', '6', ts, 'i:', @, n]).
entry('Vorzimmer', [f, 'o:', '6', ts, 'I', m, '6']).
entry(vorzuarbeiten, [f, 'o:', '6', ts, u, a, r, b, aI, t, @, n]).
entry(vorzubereiten, [f, 'o:', '6', ts, u, b, @, r, aI, t, @, n]).
entry(vorzubesprechen, [f, 'o:', '6', ts, u, b, @, 'S', p, r, 'E', 'C', @, n]).
entry('Vorzüge', [f, 'o:', '6', ts, 'y:', g, @]).
entry(vorzüglich, [f, 'o:', '6', ts, 'y:', k, l, 'I', 'C']).
entry(vorzugsweise, [f, 'o:', '6', ts, 'u:', k, s, v, aI, z, @]).
entry('Vorzugung', [f, 'o:', '6', ts, 'u:', g, 'U', 'N']).
entry(vorzulesen, [f, 'o:', '6', ts, u, l, 'e:', z, @, n]).
entry(vorzunehmen, [f, 'o:', '6', ts, u, n, 'e:', m, @, n]).
entry(vorzuschlagen, [f, 'o:', '6', ts, u, 'S', l, 'a:', g, @, n]).
entry(vorzustellen, [f, 'o:', '6', ts, u, 'S', t, 'E', l, @, n]).
entry(vorzuziehen, [f, 'o:', '6', ts, u, ts, 'i:', @, n]).
entry(vrauchen, [v, r, aU, x, @, n]).
entry(wächst, [v, 'E', k, s, t]).
entry(wach, [v, a, x]).
entry('Wagen', [v, 'a:', g, @, n]).
entry('Waggon', [v, a, g, 'O', 'N']).
entry('Wagner-Festspiele', [v, 'a:', g, n, '6', f, 'E', s, tS, p, 'i:', l, @]).
entry('Wagner', [v, 'a:', g, n, '6']).
entry(wählen, [v, 'E:', l, @, n]).
entry(wählerisch, [v, 'E:', l, @, r, 'I', 'S']).
entry(wahlfrei, [v, 'a:', l, f, r, aI]).
entry(wahl, [v, 'a:', l]).
entry(wahlweise, [v, 'a:', l, v, aI, z, @]).
entry(wahnsinnig, [v, 'a:', n, z, 'I', n, 'I', 'C']).
entry('Wahnsinn', [v, 'a:', n, z, 'I', n]).
entry('Wahn', [v, 'a:', n]).
entry(während, [v, 'E:', r, @, n, t]).
entry(währen, [v, 'E:', r, @, n]).
entry(wahrnehmen, [v, 'a:', r, n, 'e:', m, @, n]).
entry(wahrscheinlich, [v, 'a:', r, 'S', aI, n, l, 'I', 'C']).
entry(wahrscheinl, [v, 'a:', r, 'S', aI, n, l]).
entry(wahr, [v, 'a:', r]).
entry(wahrzunehmen, [v, 'a:', r, ts, u, n, 'e:', m, @, n]).
entry('Waiwoche', [v, aI, v, 'O', x, @]).
entry(wakaranakatta, [v, 'a:', k, a, r, a, n, a, k, a, t, a]).
entry('Walberg', [v, 'a:', l, b, 'E', '6', k]).
entry('Walch', [v, a, l, 'C']).
entry('Waldvogel', [v, a, l, t, f, 'o:', g, @, l]).
entry('Wallbaum', [v, a, l, b, aU, m]).
entry('Wallbrühl', [v, a, l, b, r, 'y:', l]).
entry('Walter', [v, a, l, t, '6']).
entry('Walzer-Restaurant', [v, a, l, ts, '6', r, 'E', s, t, o, r, 'a~:']).
entry(wandern, [v, a, n, d, '6', n]).
entry(wann, [v, a, n]).
entry(waren, [v, 'a:', r, @, n]).
entry(wäre, [v, 'E:', r, @]).
entry(warme, [v, a, r, m, @]).
entry(warm, [v, a, r, m]).
entry(wärst, [v, 'E:', '6', s, t]).
entry(warten, [v, a, r, t, @, n]).
entry(wartet, [v, a, r, t, @, t]).
entry(warte, [v, a, r, t, @]).
entry('Wartezeiten', [v, a, r, t, @, ts, aI, t, @, n]).
entry(warum, [v, a, r, 'U', m]).
entry(wär, [v, 'E:', '6']).
entry('Wäsche', [v, 'E', 'S', @]).
entry('Washington', [v, 'O', 'S', 'I', 'N', t, @, n]).
entry(wasserscheuen, [v, a, s, '6', 'S', 'OY', @, n]).
entry('Wasserskifahren', [v, a, s, '6', 'S', 'i:', f, 'a:', r, @, n]).
entry('Wasserstraße', [v, a, s, '6', 'S', t, r, 'a:', s, @]).
entry('Wasser', [v, a, s, '6']).
entry(was, [v, a, s]).
entry(wa, [v, a]).
entry('Wa', [v, 'a:']).
entry(wä, [v, 'E']).
entry(way, [v, 'e:']).
entry('Weber', [v, 'e:', b, '6']).
entry('Web-Seite', [v, 'E', p, z, aI, t, @]).
entry(wechseln, [v, 'E', k, s, @, l, n]).
entry(wechselseitigen, [v, 'E', k, s, @, l, z, aI, t, 'I', g, @, n]).
entry(wechselseitige, [v, 'E', k, s, @, l, z, aI, t, 'I', g, @]).
entry('Wechsel', [v, 'E', k, s, @, l]).
entry(weder, [v, 'e:', d, '6']).
entry(wegbekommen, [v, 'E', k, b, @, k, 'O', m, @, n]).
entry(wegbleiben, [v, 'E', k, b, l, aI, b, @, n]).
entry(wegbringen, [v, 'E', k, b, r, 'I', 'N', @, n]).
entry('Wegener', [v, 'e:', g, @, n, '6']).
entry(wegen, [v, 'e:', g, @, n]).
entry('Wege', [v, 'e:', g, @]).
entry(wegfahren, [v, 'E', k, f, 'a:', r, @, n]).
entry(wegfahre, [v, 'E', k, f, 'a:', r, @]).
entry(wegfährt, [v, 'E', k, f, 'E:', '6', t]).
entry(wegfallen, [v, 'E', k, f, a, l, @, n]).
entry(wegfällt, [v, 'E', k, f, 'E', l, t]).
entry(wegfliegen, [v, 'E', k, f, l, 'i:', g, @, n]).
entry(wegfliege, [v, 'E', k, f, l, 'i:', g, @]).
entry(weggefahren, [v, 'E', k, g, @, f, 'a:', r, @, n]).
entry(weggehen, [v, 'E', k, g, 'e:', @, n]).
entry(weggeht, [v, 'E', k, g, 'e:', t]).
entry(wegkommen, [v, 'E', k, k, 'O', m, @, n]).
entry(wegkomme, [v, 'E', k, k, 'O', m, @]).
entry(wegkönnen, [v, 'E', k, k, '9', n, @, n]).
entry(weglassen, [v, 'E', k, l, a, s, @, n]).
entry('Wegner', [v, 'e:', g, n, '6']).
entry(wegschnappen, [v, 'E', k, 'S', n, a, p, @, n]).
entry('Wegstrecke', [v, 'e:', k, 'S', t, r, 'E', k, @]).
entry(weg, [v, 'E', k]).
entry('Weg', [v, 'e:', k]).
entry(wegzufahren, [v, 'E', k, ts, u, f, 'a:', r, @, n]).
entry(wegzufliegen, [v, 'E', k, ts, u, f, l, 'i:', g, @, n]).
entry(wegzugehen, [v, 'E', k, ts, u, g, 'e:', @, n]).
entry('Weiberfastnacht', [v, aI, b, '6', f, a, s, t, n, a, x, t]).
entry(weichen, [v, aI, 'C', @, n]).
entry(weicht, [v, aI, 'C', t]).
entry('Weidmann', [v, aI, t, m, a, n]).
entry(weigern, [v, aI, g, '6', n]).
entry('Weihnachten', [v, aI, n, a, x, t, @, n]).
entry(weihnachtliche, [v, aI, n, a, x, t, l, 'I', 'C', @]).
entry('Weihnachtseinkäufen', [v, aI, n, a, x, ts, aI, n, k, 'OY', f, @, n]).
entry('Weihnachtsfeiern', [v, aI, n, a, x, ts, f, aI, '6', n]).
entry('Weihnachtsfeiertagen', [v, aI, n, a, x, ts, f, aI, '6', t, 'a:', g, @, n]).
entry('Weihnachtsfeiertage', [v, aI, n, a, x, ts, f, aI, '6', t, 'a:', g, @]).
entry('Weihnachtsfeier', [v, aI, n, a, x, ts, f, aI, '6']).
entry('Weihnachtsferien', [v, aI, n, a, x, ts, f, 'e:', '6', j, @, n]).
entry('Weihnachtsfest', [v, aI, n, a, x, ts, f, 'E', s, t]).
entry('Weihnachtsfrust', [v, aI, n, a, x, ts, f, r, 'U', s, t]).
entry('Weihnachtskonzerte', [v, aI, n, a, x, ts, k, 'O', n, ts, 'E', '6', t, @]).
entry('Weihnachtsmarkt', [v, aI, n, a, x, ts, m, a, r, k, t]).
entry('Weihnachtstage', [v, aI, n, a, x, ts, t, 'a:', g, @]).
entry('Weihnachtstag', [v, aI, n, a, x, ts, t, 'a:', k]).
entry('Weihnachtstrubel', [v, aI, n, a, x, ts, t, r, 'u:', b, @, l]).
entry('Weihnachtsvorbereitungen', [v, aI, n, a, x, ts, f, 'o:', '6', b, @, r, aI, t, 'U', 'N', @, n]).
entry('Weihnachtsvorbereitung', [v, aI, n, a, x, ts, f, 'o:', '6', b, @, r, aI, t, 'U', 'N']).
entry('Weihnachtszeit', [v, aI, n, a, x, ts, ts, aI, t]).
entry('Weihn', [v, aI, n]).
entry('Weile', [v, aI, l, @]).
entry('Weilhammer', [v, aI, l, h, a, m, '6']).
entry(weil, [v, aI, l]).
entry('Weimar', [v, aI, m, a, r]).
entry('Weim', [v, aI, m]).
entry('Weinchen', [v, aI, n, 'C', @, n]).
entry(weisen, [v, aI, z, @, n]).
entry(weiser, [v, aI, z, '6']).
entry(weise, [v, aI, z, @]).
entry('Weisheitszähnen', [v, aI, s, h, aI, ts, ts, 'E:', n, @, n]).
entry('Weisheitszähne', [v, aI, s, h, aI, ts, ts, 'E:', n, @]).
entry('Weißpflug', [v, aI, s, pf, l, 'u:', k]).
entry('Weiss', [v, aI, s, s]).
entry(weißt, [v, aI, s, t]).
entry(weiß, [v, aI, s]).
entry(weitaus, [v, aI, t, aU, s]).
entry(weiten, [v, aI, t, @, n]).
entry(weiterbilden, [v, aI, t, '6', b, 'I', l, d, @, n]).
entry(weiteren, [v, aI, t, @, r, @, n]).
entry(weiterer, [v, aI, t, @, r, '6']).
entry(weiteres, [v, aI, t, @, r, @, s]).
entry(weitere, [v, aI, t, @, r, @]).
entry(weiterfliegen, [v, aI, t, '6', f, l, 'i:', g, @, n]).
entry(weiterfliege, [v, aI, t, '6', f, l, 'i:', g, @]).
entry(weitergeben, [v, aI, t, '6', g, 'e:', b, @, n]).
entry(weitergehen, [v, aI, t, '6', g, 'e:', @, n]).
entry(weiterhelfen, [v, aI, t, '6', h, 'E', l, f, @, n]).
entry(weiterhin, [v, aI, t, '6', h, 'I', n]).
entry(weiterkomme, [v, aI, t, '6', k, 'O', m, @]).
entry(weiterleiten, [v, aI, t, '6', l, aI, t, @, n]).
entry(weitermachen, [v, aI, t, '6', m, a, x, @, n]).
entry(weitermelden, [v, aI, t, '6', m, 'E', l, d, @, n]).
entry(weiterreisen, [v, aI, t, '6', r, aI, z, @, n]).
entry(weiterreise, [v, aI, t, '6', r, aI, z, @]).
entry(weitersagen, [v, aI, t, '6', z, 'a:', g, @, n]).
entry(weiterschauen, [v, aI, t, '6', 'S', aU, @, n]).
entry('Weiterscheit', [v, aI, t, '6', 'S', aI, t]).
entry(weitersehen, [v, aI, t, '6', z, 'e:', @, n]).
entry(weiter, [v, aI, t, '6']).
entry(weiterziehen, [v, aI, t, '6', ts, 'i:', @, n]).
entry(weitesten, [v, aI, t, @, s, t, @, n]).
entry(weitestgehend, [v, aI, t, @, s, t, g, 'e:', @, n, t]).
entry(weite, [v, aI, t, @]).
entry(weitgehend, [v, aI, t, g, 'e:', @, n, t]).
entry(weit, [v, aI, t]).
entry(wei, [v, aI]).
entry('Weizbauer', [v, aI, ts, b, aU, '6']).
entry(welchem, [v, 'E', l, 'C', @, m]).
entry(welchen, [v, 'E', l, 'C', @, n]).
entry(welcher, [v, 'E', l, 'C', '6']).
entry(welches, [v, 'E', l, 'C', @, s]).
entry(welche, [v, 'E', l, 'C', @]).
entry(welch, [v, 'E', l, 'C']).
entry(wellness, [v, 'E', l, n, 'E', s]).
entry('Weltgeschichte', [v, 'E', l, t, g, @, 'S', 'I', 'C', t, @]).
entry('Weltmetropole', [v, 'E', l, t, m, e, t, r, o, p, 'o:', l, @]).
entry('Welt', [v, 'E', l, t]).
entry(wem, [v, 'e:', m]).
entry(wenden, [v, 'E', n, d, @, n]).
entry('Wende', [v, 'E', n, d, @]).
entry(wenicht, [v, 'e:', n, 'I', 'C', t]).
entry(wenigen, [v, 'e:', n, 'I', g, @, n]).
entry(weniger, [v, 'e:', n, 'I', g, '6']).
entry(wenige, [v, 'e:', n, 'I', g, @]).
entry(wenigstens, [v, 'e:', n, 'I', 'C', s, t, @, n, s]).
entry(wenigsten, [v, 'e:', n, 'I', 'C', s, t, @, n]).
entry(wenig, [v, 'e:', n, 'I', 'C']).
entry('Wennicke', [v, 'E', n, 'I', k, @]).
entry(wenn, [v, 'E', n]).
entry('Wenrich', [v, 'E', n, r, 'I', 'C']).
entry(wen, [v, 'e:', n]).
entry('Wenzel', [v, 'E', n, ts, @, l]).
entry(werben, [v, 'E', '6', b, @, n]).
entry(werden, [v, 'e:', '6', d, @, n]).
entry(werde, [v, 'e:', '6', d, @]).
entry(werd, [v, 'e:', '6', t]).
entry(werfen, [v, 'E', '6', f, @, n]).
entry('Werkstatt', [v, 'E', '6', k, 'S', t, a, t]).
entry('Werktagen', [v, 'E', '6', k, t, 'a:', g, @, n]).
entry('Werktage', [v, 'E', '6', k, t, 'a:', g, @]).
entry(werktags, [v, 'E', '6', k, t, 'a:', k, s]).
entry('Werktags-Woche', [v, 'E', '6', k, t, 'a:', k, s, v, 'O', x, @]).
entry('Werktag', [v, 'E', '6', k, t, 'a:', k]).
entry('Werk', [v, 'E', '6', k]).
entry('Werkzeug', [v, 'E', '6', k, ts, 'OY', k]).
entry('Werner', [v, 'E', '6', n, '6']).
entry(wertvolle, [v, 'e:', '6', t, f, 'O', l, @]).
entry(wer, [v, 'e:', '6']).
entry('Wesener', [v, 'e:', z, @, n, '6']).
entry('Wesenick', [v, 'e:', z, @, n, 'I', k]).
entry(wesentlichen, [v, 'e:', z, @, n, t, l, 'I', 'C', @, n]).
entry(wesentliche, [v, 'e:', z, @, n, t, l, 'I', 'C', @]).
entry(wesentlich, [v, 'e:', z, @, n, t, l, 'I', 'C']).
entry(weshalb, [v, 'E', s, h, a, l, p]).
entry(wessen, [v, 'E', s, @, n]).
entry(westdeutschen, [v, 'E', s, t, d, 'OY', tS, @, n]).
entry('Westensee', [v, 'E', s, t, @, n, z, 'e:']).
entry(weswegen, [v, 'E', s, v, 'e:', g, @, n]).
entry(wetterfühlig, [v, 'E', t, '6', f, 'y:', l, 'I', 'C']).
entry('Wetters', [v, 'E', t, '6', s]).
entry('Wetter', [v, 'E', t, '6']).
entry('Wetzlar', [v, 'E', ts, l, a, r]).
entry(whatsoever, [v, 'O', ts, o, 'E', v, '6']).
entry(where, [w, e, @, r]).
entry('Whirlpool', [v, '9', '6', l, p, 'u:', l]).
entry('Wichelshof', [v, 'I', 'C', @, l, s, h, 'o:', f]).
entry(wichtigen, [v, 'I', 'C', t, 'I', g, @, n]).
entry(wichtigere, [v, 'I', 'C', t, 'I', g, @, r, @]).
entry(wichtiger, [v, 'I', 'C', t, 'I', g, '6']).
entry(wichtiges, [v, 'I', 'C', t, 'I', g, @, s]).
entry(wichtige, [v, 'I', 'C', t, 'I', g, @]).
entry('Wichtigkeit', [v, 'I', 'C', t, 'I', 'C', k, aI, t]).
entry(wichtigsten, [v, 'I', 'C', t, 'I', 'C', s, t, @, n]).
entry(wichtigste, [v, 'I', 'C', t, 'I', 'C', s, t, @]).
entry(wichtig, [v, 'I', 'C', t, 'I', 'C']).
entry(wicht, [v, 'I', 'C', t]).
entry(wich, [v, 'I', 'C']).
entry('Wick', [v, 'I', k]).
entry(widerrufen, [v, 'i:', d, '6', r, 'u:', f, @, n]).
entry('Widerspruch', [v, 'i:', d, '6', 'S', p, r, 'U', x]).
entry(widmen, [v, 'I', t, m, @, n]).
entry('Wieczorek', [v, 'i:', ts, o, r, 'E', k]).
entry(wiedererkennen, [v, 'i:', d, '6', 'E', '6', k, 'E', n, @, n]).
entry(wiederholen, [v, 'i:', d, '6', h, 'o:', l, @, n]).
entry(wiederhole, [v, 'i:', d, '6', h, 'o:', l, @]).
entry(wiederholt, [v, 'i:', d, '6', h, 'o:', l, t]).
entry('Wiederhören', [v, 'i:', d, '6', h, '2:', r, @, n]).
entry('Wiederhör', [v, 'i:', d, '6', h, '2:', '6']).
entry('Wiederh', [v, 'i:', d, '6', h]).
entry(wiederkommen, [v, 'i:', d, '6', k, 'O', m, @, n]).
entry(wiederkomme, [v, 'i:', d, '6', k, 'O', m, @]).
entry('Wiederschauen', [v, 'i:', d, '6', 'S', aU, @, n]).
entry(wiedersehen, [v, 'i:', d, '6', z, 'e:', @, n]).
entry('Wiederseh', [v, 'i:', d, '6', z, e]).
entry(wiedersieht, [v, 'i:', d, '6', z, 'i:', t]).
entry('Wiedertreffen', [v, 'i:', d, '6', t, r, 'E', f, @, n]).
entry(wiederum, [v, 'i:', d, @, r, 'U', m]).
entry(wieder, [v, 'i:', d, '6']).
entry(wiederzusehen, [v, 'i:', d, '6', ts, u, z, 'e:', @, n]).
entry(wiederzutreffen, [v, 'i:', d, '6', ts, u, t, r, 'E', f, @, n]).
entry('Wieland', [v, 'i:', l, a, n, t]).
entry('Wiener', [v, 'i:', n, '6']).
entry('Wien', [v, 'i:', n]).
entry('Wiesbaden', [v, 'i:', s, b, 'a:', d, @, n]).
entry('Wiesn', [v, 'i:', s, n]).
entry(wieso, [v, i, z, 'o:']).
entry(wieviele, [v, i, f, 'i:', l, @]).
entry(wievielten, [v, 'i:', f, 'i:', l, t, @, n]).
entry(wievielte, [v, 'i:', f, 'i:', l, t, @]).
entry(wieviel, [v, i, f, 'i:', l]).
entry(wievie, [v, i, f, 'i:']).
entry(wiev, [v, i, f]).
entry(wild, [v, 'I', l, t]).
entry('Wilhelm-Busch-Museum', [v, i, l, h, 'E', l, m, b, 'U', 'S', m, u, z, 'e:', 'U', m]).
entry('Wilhelmsstraße', [v, 'I', l, h, 'E', l, m, 'S', t, r, 'a:', s, @]).
entry('Wilhelm', [v, 'I', l, h, 'E', l, m]).
entry('Wilkens', [v, 'I', l, k, @, n, s]).
entry('Wilkes', [v, 'I', l, k, @, s]).
entry('Wilke', [v, 'I', l, k, @]).
entry(willen, [v, 'I', l, @, n]).
entry(williger, [v, 'I', l, 'I', g, '6']).
entry('Willi', [v, 'I', l, i]).
entry(willkommene, [v, 'I', l, k, 'O', m, @, n, @]).
entry(willkommen, [v, 'I', l, k, 'O', m, @, n]).
entry('Willms', [v, 'I', l, m, s]).
entry(willst, [v, 'I', l, s, t]).
entry(will, [v, 'I', l]).
entry('Windrich', [v, 'I', n, d, r, 'I', 'C']).
entry('Wind', [v, 'I', n, t]).
entry(winkt, [v, 'I', 'N', k, t]).
entry('Winter', [v, 'I', n, t, '6']).
entry(wirde, [v, 'I', '6', d, @]).
entry(wird, [v, 'I', '6', t]).
entry(wirft, [v, 'I', '6', f, t]).
entry(wirken, [v, 'I', '6', k, @, n]).
entry(wirkliche, [v, 'I', '6', k, l, 'I', 'C', @]).
entry(wirklich, [v, 'I', '6', k, l, 'I', 'C']).
entry(wirst, [v, 'I', '6', s, t]).
entry('Wirtschaftsenglisch-Kurses', [v, 'I', '6', tS, a, f, ts, 'E', 'N', l, 'I', 'S', k, 'U', '6', z, @, s]).
entry('Wirtschaftsenglisch-Kursus', [v, 'I', '6', tS, a, f, ts, 'E', 'N', l, 'I', 'S', k, 'U', '6', z, 'U', s]).
entry('Wirtschaftsenglisch', [v, 'I', '6', tS, a, f, ts, 'E', 'N', l, 'I', 'S']).
entry('Wirtschafts-Staatssekretär', [v, 'I', '6', tS, a, f, ts, 'S', t, 'a:', ts, z, e, k, r, e, t, 'E:', '6']).
entry('Wirtschaft', [v, 'I', '6', tS, a, f, t]).
entry(wir, [v, 'i:', '6']).
entry(wissen, [v, 'I', s, @, n]).
entry(wis, [v, 'I', s]).
entry(wit, [v, 'I', t]).
entry(witzig, [v, 'I', ts, 'I', 'C']).
entry('Witz', [v, 'I', ts]).
entry(wi, [v, i]).
entry(wl, [v, l]).
entry(woanders, [v, o, a, n, d, '6', s]).
entry(wobei, [v, o, b, aI]).
entry('Wochenablauf', [v, 'O', x, @, n, a, b, l, aU, f]).
entry('Wochenanfang', [v, 'O', x, @, n, a, n, f, a, 'N']).
entry('Wochenendarbeit', [v, 'O', x, @, n, 'E', n, t, a, r, b, aI, t]).
entry('Wochenenden', [v, 'O', x, @, n, 'E', n, d, @, n]).
entry('Wochenendes', [v, 'O', x, @, n, 'E', n, d, @, s]).
entry(wochenende, [v, 'O', x, @, n, 'E', n, d, @]).
entry('Wochenendheimfahrer', [v, 'O', x, @, n, 'E', n, t, h, aI, m, f, 'a:', r, '6']).
entry('Wochenendseminaren', [v, 'O', x, @, n, 'E', n, t, z, e, m, i, n, 'a:', r, @, n]).
entry('Wochenendseminars', [v, 'O', x, @, n, 'E', n, t, z, e, m, i, n, 'a:', r, s]).
entry('Wochenendseminar', [v, 'O', x, @, n, 'E', n, t, z, e, m, i, n, 'a:', r]).
entry(wochenends, [v, 'O', x, @, n, 'E', n, ts]).
entry('Wochenendtage', [v, 'O', x, @, n, 'E', n, t, t, 'a:', g, @]).
entry('Wochenendtag', [v, 'O', x, @, n, 'E', n, t, t, 'a:', k]).
entry('Wochenendterminen', [v, 'O', x, @, n, 'E', n, t, t, 'E', '6', m, 'i:', n, @, n]).
entry('Wochenendtermin', [v, 'O', x, @, n, 'E', n, t, t, 'E', '6', m, 'i:', n]).
entry('Wochenendticket', [v, 'O', x, @, n, 'E', n, t, t, 'I', k, @, t]).
entry('Wochenend', [v, 'O', x, @, n, 'E', n, t]).
entry('Wochenen', [v, 'O', x, @, n, 'E', n]).
entry('Wochene', [v, 'O', x, @, n, 'E']).
entry('Wochenhälfte', [v, 'O', x, @, n, h, 'E', l, f, t, @]).
entry('Wochenkalender', [v, 'O', x, @, n, k, a, l, 'E', n, d, '6']).
entry('Wochenmitte', [v, 'O', x, @, n, m, 'I', t, @]).
entry('Wochenplanungs-Sitzung', [v, 'O', x, @, n, p, l, 'a:', n, 'U', 'N', s, z, 'I', ts, 'U', 'N']).
entry('Wochenplanung', [v, 'O', x, @, n, p, l, 'a:', n, 'U', 'N']).
entry('Wochenplan', [v, 'O', x, @, n, p, l, 'a:', n]).
entry('Wochentagen', [v, 'O', x, @, n, t, 'a:', g, @, n]).
entry('Wochentage', [v, 'O', x, @, n, t, 'a:', g, @]).
entry('Wochentag', [v, 'O', x, @, n, t, 'a:', k]).
entry('Wochenteil', [v, 'O', x, @, n, t, aI, l]).
entry(wöchentlichen, [v, '9', 'C', @, n, t, l, 'I', 'C', @, n]).
entry(wöchentliche, [v, '9', 'C', @, n, t, l, 'I', 'C', @]).
entry(wöchentlich, [v, '9', 'C', @, n, t, l, 'I', 'C']).
entry(wochen, [v, 'O', x, @, n]).
entry(wochenweise, [v, 'O', x, @, n, v, aI, z, @]).
entry(woche, [v, 'O', x, @]).
entry(wöchi, [v, '9', 'C', 'I']).
entry('Woch', [v, 'O', x]).
entry('Wocke', [v, 'O', k, @]).
entry(wofür, [v, o, f, 'y:', '6']).
entry(wohe, [v, o, h, 'e:']).
entry(wohin, [v, o, h, 'I', n]).
entry(wohler, [v, 'o:', l, '6']).
entry(wohlfühlen, [v, 'o:', l, f, 'y:', l, @, n]).
entry(wohlsten, [v, 'o:', l, s, t, @, n]).
entry(wohl, [v, 'o:', l]).
entry(wohnen, [v, 'o:', n, @, n]).
entry(wohne, [v, 'o:', n, @]).
entry(wohnst, [v, 'o:', n, s, t]).
entry(wohnt, [v, 'o:', n, t]).
entry('Wolfenbüttel', [v, 'O', l, f, @, n, b, 'Y', t, @, l]).
entry('Wolfgang', [v, 'O', l, f, g, a, 'N']).
entry('Wolfrum', [v, 'O', l, f, r, 'U', m]).
entry('Wolfsburg', [v, 'O', l, f, s, b, 'U', '6', k]).
entry(wollen, [v, 'O', l, @, n]).
entry('Wöllnatz', [v, '9', l, n, a, ts]).
entry(wollten, [v, 'O', l, t, @, n]).
entry(wolltest, [v, 'O', l, t, @, s, t]).
entry(wollte, [v, 'O', l, t, @]).
entry(wollt, [v, 'O', l, t]).
entry(woll, [v, 'O', l]).
entry('Wolters', [v, 'O', l, t, '6', s]).
entry(womit, [v, o, m, 'I', t]).
entry(womöglich, [v, o, m, '2:', k, l, 'I', 'C']).
entry(wonach, [v, o, n, 'a:', x]).
entry(wonschen, [v, 'O', n, 'S', @, n]).
entry(woran, [v, o, r, a, n]).
entry(worauf, [v, o, r, aU, f]).
entry('Wörb', [v, '9', '6', p]).
entry(worden, [v, 'O', '6', d, @, n]).
entry('Workaholic', [v, '9', '6', k, a, h, 'O', l, 'I', k]).
entry('Workshop', [v, '9', '6', k, 'S', 'O', p]).
entry('Worten', [v, 'O', '6', t, @, n]).
entry('Wörter', [v, '9', '6', t, '6']).
entry('Worte', [v, 'O', '6', t, @]).
entry('Wort', [v, 'O', '6', t]).
entry(worüber, [v, o, r, 'y:', b, '6']).
entry(worum, [v, 'o:', r, 'U', m]).
entry(wo, [v, 'o:']).
entry(wovon, [v, o, f, 'O', n]).
entry(wow, [v, aU]).
entry('Woyzek', [v, 'OY', ts, 'E', k]).
entry('Wübbena', [v, 'Y', b, @, n, a]).
entry('Wühr', [v, 'y:', '6']).
entry(wunden, [v, 'U', n, d, @, n]).
entry(wunderbaren, [v, 'U', n, d, '6', b, 'a:', r, @, n]).
entry(wunderbarer, [v, 'U', n, d, '6', b, 'a:', r, '6']).
entry(wunderbare, [v, 'U', n, d, '6', b, 'a:', r, @]).
entry(wunderbar, [v, 'U', n, d, '6', b, 'a:', r]).
entry(wunderba, [v, 'U', n, d, '6', b, a]).
entry(wundern, [v, 'U', n, d, '6', n]).
entry(wunderschönen, [v, 'U', n, d, '6', 'S', '2:', n, @, n]).
entry(wunderschönes, [v, 'U', n, d, '6', 'S', '2:', n, @, s]).
entry(wunderschöne, [v, 'U', n, d, '6', 'S', '2:', n, @]).
entry(wunderschön, [v, 'U', n, d, '6', 'S', '2:', n]).
entry(wundervoll, [v, 'U', n, d, '6', f, 'O', l, @]).
entry(wunder, [v, 'U', n, d, '6']).
entry(wünschen, [v, 'Y', n, 'S', @, n]).
entry(wünsche, [v, 'Y', n, 'S', @]).
entry('Wunschtermin', [v, 'U', n, 'S', t, 'E', '6', m, 'i:', n]).
entry('Wunsch', [v, 'U', n, 'S']).
entry(wünsch, [v, 'Y', n, 'S']).
entry(wun, [v, 'U', n]).
entry('Wuppertal', [v, 'U', p, '6', t, 'a:', l]).
entry(wurden, [v, 'U', '6', d, @, n]).
entry(würden, [v, 'Y', '6', d, @, n]).
entry(würderum, [v, 'Y', '6', d, @, r, 'U', m]).
entry(würdest, [v, 'Y', '6', d, @, s, t]).
entry(wurde, [v, 'U', '6', d, @]).
entry(würde, [v, 'Y', '6', d, @]).
entry('Wurscht', [v, 'U', '6', 'S', t]).
entry('Würstchen', [v, 'Y', '6', s, t, 'C', @, n]).
entry(wür, [v, 'Y', '6']).
entry('Würzburg', [v, 'Y', '6', ts, b, 'U', '6', k]).
entry('Würze', [v, 'Y', '6', ts, @]).
entry(wüssen, [v, 'Y', s, @, n]).
entry(wüßten, [v, 'Y', s, t, @, n]).
entry(wußte, [v, 'U', s, t, @]).
entry(wüßte, [v, 'Y', s, t, @]).
entry(wü, [v, 'Y']).
entry(w, [v]).
entry('Xaver', [k, s, 'a:', v, '6']).
entry('Xylophon', [k, s, y, l, o, f, 'o:', n]).
entry('Yasmin', [j, a, s, m, 'i:', n]).
entry(yes, [j, e, s]).
entry('Yoshimura', [j, o, s, i, m, 'u:', r, a]).
entry('Yps', ['Y', p, s]).
entry(zack, [ts, a, k]).
entry(zahlen, [ts, 'a:', l, @, n]).
entry(zählen, [ts, 'E:', l, @, n]).
entry(zahle, [ts, 'a:', l, @]).
entry(zähle, [ts, 'E:', l, @]).
entry(zahlreiche, [ts, 'a:', l, r, aI, 'C', @]).
entry('Zahl', [ts, 'a:', l]).
entry(zahlt, [ts, 'a:', l, t]).
entry(zählt, [ts, 'E:', l, t]).
entry('Zahlungs', [ts, 'a:', l, 'U', 'N', s]).
entry('Zählung', [ts, 'E:', l, 'U', 'N']).
entry('Zahnarzttermin', [ts, 'a:', n, a, r, ts, t, t, 'E', '6', m, 'i:', n]).
entry('Zahnarzt', [ts, 'a:', n, a, r, ts, t]).
entry('Zanker', [ts, a, 'N', k, '6']).
entry(zäumen, [ts, 'OY', m, @, n]).
entry(zaze, [ts, a, ts, @]).
entry('Zboril', [s, b, 'o:', r, i, l]).
entry('Zechnall', [ts, 'E', 'C', n, a, l]).
entry('Zehnpfennig', [ts, 'e:', n, pf, 'E', n, 'I', 'C']).
entry(zehntem, [ts, 'e:', n, t, @, m]).
entry(zehnten, [ts, 'e:', n, t, @, n]).
entry(zehnter, [ts, 'e:', n, t, '6']).
entry(zehnte, [ts, 'e:', n, t, @]).
entry(zehn, [ts, 'e:', n]).
entry(zeh, [ts, 'e:']).
entry(zeigen, [ts, aI, g, @, n]).
entry(zeige, [ts, aI, g, @]).
entry('Zeile', [ts, aI, l, @]).
entry('Zeise', [ts, aI, z, @]).
entry('Zeitangebot', [ts, aI, t, a, n, g, @, b, 'o:', t]).
entry('Zeitdruck', [ts, aI, t, d, r, 'U', k]).
entry('Zeiteinteilung', [ts, aI, t, aI, n, t, aI, l, 'U', 'N']).
entry(zeiten, [ts, aI, t, @, n]).
entry('Zeitersparnis', [ts, aI, t, 'E', '6', 'S', p, 'a:', r, n, 'I', s]).
entry('Zeitfrage', [ts, aI, t, f, r, 'a:', g, @]).
entry('Zeitgeschichte', [ts, aI, t, g, @, 'S', 'I', 'C', t, @]).
entry('Zeitgestaltung', [ts, aI, t, g, @, 'S', t, a, l, t, 'U', 'N']).
entry('Zeitgründen', [ts, aI, t, g, r, 'Y', n, d, @, n]).
entry(zeitig, [ts, aI, t, 'I', 'C']).
entry(zeitlichen, [ts, aI, t, l, 'I', 'C', @, n]).
entry(zeitliches, [ts, aI, t, l, 'I', 'C', @, s]).
entry(zeitlich, [ts, aI, t, l, 'I', 'C']).
entry(zeitmäßig, [ts, aI, t, m, 'E:', s, 'I', 'C']).
entry('Zeitnot', [ts, aI, t, n, 'o:', t]).
entry('Zeitpläne', [ts, aI, t, p, l, 'E:', n, @]).
entry('Zeitplan', [ts, aI, t, p, l, 'a:', n]).
entry('Zeitpunkt', [ts, aI, t, p, 'U', 'N', k, t]).
entry('Zeiträume', [ts, aI, t, r, 'OY', m, @]).
entry('Zeitraums', [ts, aI, t, r, aU, m, s]).
entry('Zeitraum', [ts, aI, t, r, aU, m]).
entry('Zeitreserven', [ts, aI, t, r, e, z, 'E', '6', v, @, n]).
entry('Zei', [ts, aI]).
entry('Zeitspanne', [ts, aI, tS, p, a, n, @]).
entry(zeitsparender, [ts, aI, tS, p, 'a:', r, @, n, d, '6']).
entry(zeitsparend, [ts, aI, tS, p, 'a:', r, @, n, t]).
entry('Zeittermin', [ts, aI, t, t, 'E', '6', m, 'i:', n]).
entry('Zeit', [ts, aI, t]).
entry('Zeitung', [ts, aI, t, 'U', 'N']).
entry('Zeitunterschied', [ts, aI, t, 'U', n, t, '6', 'S', 'i:', t]).
entry('Zeitverschwendung', [ts, aI, t, f, 'E', '6', 'S', v, 'E', n, d, 'U', 'N']).
entry('Zeitvorschlag', [ts, aI, t, f, 'o:', '6', 'S', l, 'a:', k]).
entry('Zell', [ts, 'E', l]).
entry('Zelthofer', [ts, 'E', l, t, h, 'o:', f, '6']).
entry('Zeltlhofer', [ts, 'E', l, t, l, h, 'o:', f, '6']).
entry('Zeltplatz', [ts, 'E', l, t, p, l, a, ts]).
entry('Zelt', [ts, 'E', l, t]).
entry('Zendert', [ts, 'E', n, d, '6', t]).
entry(zentralen, [ts, 'E', n, t, r, 'a:', l, @, n]).
entry(zentraler, [ts, 'E', n, t, r, 'a:', l, '6']).
entry(zentrales, [ts, 'E', n, t, r, 'a:', l, @, s]).
entry(zentrale, [ts, 'E', n, t, r, 'a:', l, @]).
entry('Zentral-Hotel', [ts, 'E', n, t, r, 'a:', l, h, o, t, 'E', l]).
entry(zentralste, [ts, 'E', n, t, r, 'a:', l, s, t, @]).
entry(zentral, [ts, 'E', n, t, r, 'a:', l]).
entry(zentra, [ts, 'E', n, t, r, a]).
entry(zentrier, [ts, 'E', n, t, r, 'i:', '6']).
entry('Zentrumsnähe', [ts, 'E', n, t, r, 'U', m, s, n, 'E:', @]).
entry(zentrumsnah, [ts, 'E', n, t, r, 'U', m, s, n, 'a:']).
entry('Zentrums', [ts, 'E', n, t, r, 'U', m, s]).
entry('Zentrum', [ts, 'E', n, t, r, 'U', m]).
entry(zen, [ts, 'E', n]).
entry(zeNzeN, [ts, 'E', n, ts, @, n]).
entry('Zeppelin', [ts, 'E', p, @, l, 'i:', n]).
entry(zerbrechen, [ts, 'E', '6', b, r, 'E', 'C', @, n]).
entry(zerhackt, [ts, 'E', '6', h, a, k, t]).
entry(zerrissen, [ts, 'E', '6', r, 'I', s, @, n]).
entry(zerschlagen, [ts, 'E', '6', 'S', l, 'a:', g, @, n]).
entry('Zerstreuung', [ts, 'E', '6', 'S', t, r, 'OY', 'U', 'N']).
entry(zerstückeln, [ts, 'E', '6', 'S', t, 'Y', k, @, l, n]).
entry(zerstückelt, [ts, 'E', '6', 'S', t, 'Y', k, @, l, t]).
entry(ze, [ts, e]).
entry('Zettel', [ts, 'E', t, @, l]).
entry(zeug, [ts, 'OY', k]).
entry(zich, [ts, 'I', 'C']).
entry(ziehen, [ts, 'i:', @, n]).
entry(ziehe, [ts, 'i:', @]).
entry(zieht, [ts, 'i:', t]).
entry(ziehungsweise, [ts, 'i:', 'U', 'N', s, v, aI, z, @]).
entry('Ziel', [ts, 'i:', l]).
entry(ziemlicher, [ts, 'i:', m, l, 'I', 'C', '6']).
entry(ziemliche, [ts, 'i:', m, l, 'I', 'C', @]).
entry(ziemlich, [ts, 'i:', m, l, 'I', 'C']).
entry(ziem, [ts, 'i:', m]).
entry('Zierdt', [ts, 'i:', '6', t]).
entry('Zigarettenpausen', [ts, i, g, a, r, 'E', t, @, n, p, aU, z, @, n]).
entry('Zigaretten', [ts, i, g, a, r, 'E', t, @, n]).
entry('Zigarre', [ts, i, g, a, r, @]).
entry(zigsten, [ts, 'I', 'C', s, t, @, n]).
entry(zigster, [ts, 'I', 'C', s, t, '6']).
entry('Zimmermann', [ts, 'I', m, '6', m, a, n]).
entry('Zimmern', [ts, 'I', m, '6', n]).
entry('Zimmernummer', [ts, 'I', m, '6', n, 'U', m, '6']).
entry('Zimmerpreise', [ts, 'I', m, '6', p, r, aI, z, @]).
entry('Zimmerreservierung', [ts, 'I', m, '6', r, e, z, 'E', '6', v, 'i:', r, 'U', 'N']).
entry(zimmer, [ts, 'I', m, '6']).
entry('Zimnak', [ts, 'I', m, n, a, k]).
entry('Zim', [ts, 'I', m]).
entry(zirka, [ts, 'I', '6', k, a]).
entry('Zi', [ts, i]).
entry(zivile, [ts, i, v, 'i:', l, @]).
entry(zo, [ts, o]).
entry('Zovember', [ts, o, v, 'E', m, b, '6']).
entry(zuallererst, [ts, u, a, l, '6', 'e:', '6', s, t]).
entry(zubringen, [ts, 'u:', b, r, 'I', 'N', @, n]).
entry(zücken, [ts, 'Y', k, @, n]).
entry(zücke, [ts, 'Y', k, @]).
entry(zudem, [ts, u, d, 'e:', m]).
entry(zuerst, [ts, u, 'e:', '6', s, t]).
entry(zufälligerweise, [ts, 'u:', f, 'E', l, 'I', g, '6', v, aI, z, @]).
entry(zufällig, [ts, 'u:', f, 'E', l, 'I', 'C']).
entry('Zufall', [ts, 'u:', f, a, l]).
entry(zufrieden, [ts, u, f, r, 'i:', d, @, n]).
entry('Zugangebot', [ts, 'u:', k, a, n, g, @, b, 'o:', t]).
entry(zugange, [ts, u, g, a, 'N', @]).
entry('Zuganreise', [ts, 'u:', k, a, n, r, aI, z, @]).
entry('Zugdauer', [ts, 'u:', k, d, aU, '6']).
entry(zugeben, [ts, 'u:', g, 'e:', b, @, n]).
entry(zugegebenermaßen, [ts, 'u:', g, @, g, 'e:', b, @, n, '6', m, 'a:', s, @, n]).
entry(zugekommen, [ts, 'u:', g, @, k, 'O', m, @, n]).
entry(zugelassen, [ts, 'u:', g, @, l, a, s, @, n]).
entry('Zügen', [ts, 'y:', g, @, n]).
entry(zugeordnet, [ts, 'u:', g, @, 'O', '6', d, n, @, t]).
entry(zugepackt, [ts, 'u:', g, @, p, a, k, t]).
entry(zugepflastert, [ts, 'u:', g, @, pf, l, a, s, t, '6', t]).
entry(zugeplant, [ts, 'u:', g, @, p, l, 'a:', n, t]).
entry(zugereicht, [ts, 'u:', g, @, r, aI, 'C', t]).
entry(zugesandt, [ts, 'u:', g, @, z, a, n, t]).
entry(zugeschickt, [ts, 'u:', g, @, 'S', 'I', k, t]).
entry('Zugeständnisse', [ts, 'u:', g, @, 'S', t, 'E', n, t, n, 'I', s, @]).
entry(zugestopft, [ts, 'u:', g, @, 'S', t, 'O', pf, t]).
entry('Zuges', [ts, 'u:', g, @, s]).
entry(zugetragen, [ts, 'u:', g, @, t, r, 'a:', g, @, n]).
entry('Züge', [ts, 'y:', g, @]).
entry('Zugfahren', [ts, 'u:', k, f, 'a:', r, @, n]).
entry('Zugfahrkarten', [ts, 'u:', k, f, 'a:', r, k, a, r, t, @, n]).
entry('Zugfahrpläne', [ts, 'u:', k, f, 'a:', r, p, l, 'E:', n, @]).
entry('Zugfahrplan', [ts, 'u:', k, f, 'a:', r, p, l, 'a:', n]).
entry('Zugfahrten', [ts, 'u:', k, f, 'a:', r, t, @, n]).
entry('Zugfahr', [ts, 'u:', k, f, 'a:', r]).
entry('Zugfahrt', [ts, 'u:', k, f, 'a:', r, t]).
entry('Zugfa', [ts, 'u:', k, f, a]).
entry(zügig, [ts, 'y:', g, 'I', 'C']).
entry(zugmäßig, [ts, 'u:', k, m, 'E:', s, 'I', 'C']).
entry('Zugnummer', [ts, 'u:', k, n, 'U', m, '6']).
entry('Zugplätze', [ts, 'u:', k, p, l, 'E', ts, @]).
entry('Zugreise', [ts, 'u:', k, r, aI, z, @]).
entry('Zugreservierungen', [ts, 'u:', k, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N', @, n]).
entry('Zugreservierung', [ts, 'u:', k, r, e, z, 'E', '6', v, 'i:', r, 'U', 'N']).
entry('Zugstrecke', [ts, 'u:', k, 'S', t, r, 'E', k, @]).
entry('Zugsystem', [ts, 'u:', k, z, 'Y', s, t, 'e:', m]).
entry('Zugtickets', [ts, 'u:', k, t, 'I', k, @, ts]).
entry('Zugticket', [ts, 'u:', k, t, 'I', k, @, t]).
entry(zug, [ts, 'u:', k]).
entry('Zugunglück', [ts, 'u:', k, 'U', n, g, l, 'Y', k]).
entry(zugunsten, [ts, u, g, 'U', n, s, t, @, n]).
entry(zugute, [ts, u, g, 'u:', t, @]).
entry('Zugverbindungen', [ts, 'u:', k, f, 'E', '6', b, 'I', n, d, 'U', 'N', @, n]).
entry('Zugverbindung', [ts, 'u:', k, f, 'E', '6', b, 'I', n, d, 'U', 'N']).
entry('Zugverbin', [ts, 'u:', k, f, 'E', '6', b, 'I', n]).
entry('Zugverb', [ts, 'u:', k, f, 'E', '6', p]).
entry('Zugv', [ts, 'u:', k, f]).
entry('Zugzeiten', [ts, 'u:', k, ts, aI, t, @, n]).
entry('Zugzeit', [ts, 'u:', k, ts, aI, t]).
entry(zuklären, [ts, u, k, l, 'E:', r, @, n]).
entry(zukommen, [ts, 'u:', k, 'O', m, @, n]).
entry(zukommt, [ts, 'u:', k, 'O', m, t]).
entry('Zukunft', [ts, 'u:', k, 'U', n, f, t]).
entry(zuliebe, [ts, u, l, 'i:', b, @]).
entry(zumal, [ts, u, m, 'a:', l]).
entry('Zum-Funkturm', [ts, 'U', m, f, 'U', 'N', k, t, 'U', '6', m]).
entry('Zum-Goldenen-Löwen', [ts, 'U', m, g, 'O', l, d, @, n, @, n, l, '2:', v, @, n]).
entry(zumindestens, [ts, u, m, 'I', n, d, @, s, t, @, n, s]).
entry(zumindest, [ts, u, m, 'I', n, d, @, s, t]).
entry('Zum-Löwen', [ts, 'U', m, l, '2:', v, @, n]).
entry(zum, [ts, 'U', m]).
entry(zumutbar, [ts, 'u:', m, 'u:', t, b, 'a:', r]).
entry(zumuten, [ts, 'u:', m, 'u:', t, @, n]).
entry(zumute, [ts, u, m, 'u:', t, @]).
entry('Zumutung', [ts, 'u:', m, 'u:', t, 'U', 'N']).
entry(zunächst, [ts, u, n, 'E:', 'C', s, t]).
entry(zun, [ts, 'u:', n]).
entry(zupaß, [ts, u, p, a, s]).
entry(zurechtfinden, [ts, u, r, 'E', 'C', t, f, 'I', n, d, @, n]).
entry(zurechtgelegt, [ts, u, r, 'E', 'C', t, g, @, l, 'e:', k, t]).
entry(zurechtkommen, [ts, u, r, 'E', 'C', t, k, 'O', m, @, n]).
entry(zurechtmachen, [ts, u, r, 'E', 'C', t, m, a, x, @, n]).
entry('Zürich', [ts, 'y:', r, 'I', 'C']).
entry(zur, [ts, 'u:', '6']).
entry(zurückfahren, [ts, u, r, 'Y', k, f, 'a:', r, @, n]).
entry(zurückfliegen, [ts, u, r, 'Y', k, f, l, 'i:', g, @, n]).
entry(zurückfliegt, [ts, u, r, 'Y', k, f, l, 'i:', k, t]).
entry('Zurückflug', [ts, u, r, 'Y', k, f, l, 'u:', k]).
entry(zurückführen, [ts, u, r, 'Y', k, f, 'y:', r, @, n]).
entry(zurückgeben, [ts, u, r, 'Y', k, g, 'e:', b, @, n]).
entry(zurückgefahren, [ts, u, r, 'Y', k, g, @, f, 'a:', r, @, n]).
entry(zurückgehen, [ts, u, r, 'Y', k, g, 'e:', @, n]).
entry(zurückgeht, [ts, u, r, 'Y', k, g, 'e:', t]).
entry(zurückgreifen, [ts, u, r, 'Y', k, g, r, aI, f, @, n]).
entry(zurückhalten, [ts, u, r, 'Y', k, h, a, l, t, @, n]).
entry(zurückkehren, [ts, u, r, 'Y', k, k, 'e:', r, @, n]).
entry(zurückkommen, [ts, u, r, 'Y', k, k, 'O', m, @, n]).
entry(zurückkomme, [ts, u, r, 'Y', k, k, 'O', m, @]).
entry(zurückko, [ts, u, r, 'Y', k, k, 'O']).
entry(zurücklegen, [ts, u, r, 'Y', k, l, 'e:', g, @, n]).
entry(zurücknehmen, [ts, u, r, 'Y', k, n, 'e:', m, @, n]).
entry(zurückrechnen, [ts, u, r, 'Y', k, r, 'E', 'C', n, @, n]).
entry(zurückrechnet, [ts, u, r, 'Y', k, r, 'E', 'C', n, @, t]).
entry(zurückreisen, [ts, u, r, 'Y', k, r, aI, z, @, n]).
entry(zurückrufen, [ts, u, r, 'Y', k, r, 'u:', f, @, n]).
entry(zurückrufe, [ts, u, r, 'Y', k, r, 'u:', f, @]).
entry(zurück, [ts, u, r, 'Y', k]).
entry('Zurückziehen', [ts, u, r, 'Y', k, ts, 'i:', @, n]).
entry(zurückzufahren, [ts, u, r, 'Y', k, ts, u, f, 'a:', r, @, n]).
entry(zurückzufliegen, [ts, u, r, 'Y', k, ts, u, f, l, 'i:', g, @, n]).
entry(zurückzugehen, [ts, u, r, 'Y', k, ts, u, g, 'e:', @, n]).
entry(zurückzukommen, [ts, u, r, 'Y', k, ts, u, k, 'O', m, @, n]).
entry(zurückzurufen, [ts, u, r, 'Y', k, ts, u, r, 'u:', f, @, n]).
entry(zurü, [ts, u, r, 'Y']).
entry(zusagen, [ts, 'u:', z, 'a:', g, @, n]).
entry(zusagt, [ts, 'u:', z, a, k, t]).
entry(zusammenarbeiten, [ts, u, z, a, m, @, n, a, r, b, aI, t, @, n]).
entry('Zusammenarbeit', [ts, u, z, a, m, @, n, a, r, b, aI, t]).
entry(zusammenbekommen, [ts, u, z, a, m, @, n, b, @, k, 'O', m, @, n]).
entry(zusammendrängt, [ts, u, z, a, m, @, n, d, r, 'E', 'N', t]).
entry(zusammenfassen, [ts, u, z, a, m, @, n, f, a, s, @, n]).
entry('Zusammenfassung', [ts, u, z, a, m, @, n, f, a, s, 'U', 'N']).
entry(zusammenfinden, [ts, u, z, a, m, @, n, f, 'I', n, d, @, n]).
entry(zusammengedrückt, [ts, u, z, a, m, @, n, g, @, d, r, 'Y', k, t]).
entry(zusammengefaßt, [ts, u, z, a, m, @, n, g, @, f, a, s, t]).
entry(zusammengekommen, [ts, u, z, a, m, @, n, g, @, k, 'O', m, @, n]).
entry(zusammengerechnet, [ts, u, z, a, m, @, n, g, @, r, 'E', 'C', n, @, t]).
entry(zusammengerufen, [ts, u, z, a, m, @, n, g, @, r, 'u:', f, @, n]).
entry(zusammengesammelt, [ts, u, z, a, m, @, n, g, @, z, a, m, 'E', l, t]).
entry(zusammengestellt, [ts, u, z, a, m, @, n, g, @, 'S', t, 'E', l, t]).
entry(zusammenhängenden, [ts, u, z, a, m, @, n, h, 'E', 'N', @, n, d, @, n]).
entry(zusammenhängende, [ts, u, z, a, m, @, n, h, 'E', 'N', @, n, d, @]).
entry(zusammenhängend, [ts, u, z, a, m, @, n, h, 'E', 'N', @, n, t]).
entry('Zusammenhang', [ts, u, z, a, m, @, n, h, a, 'N']).
entry(zusammenkämen, [ts, u, z, a, m, @, n, k, 'E:', m, @, n]).
entry(zusammenklauben, [ts, u, z, a, m, @, n, k, l, aU, b, @, n]).
entry(zusammenkommen, [ts, u, z, a, m, @, n, k, 'O', m, @, n]).
entry(zusammenkriegen, [ts, u, z, a, m, @, n, k, r, 'i:', g, @, n]).
entry('Zusammenkunft', [ts, u, z, a, m, @, n, k, 'U', n, f, t]).
entry(zusammenlegen, [ts, u, z, a, m, @, n, l, 'e:', g, @, n]).
entry(zusammenlegt, [ts, u, z, a, m, @, n, l, 'e:', k, t]).
entry(zusammenpaßt, [ts, u, z, a, m, @, n, p, a, s, t]).
entry(zusammenrechnet, [ts, u, z, a, m, @, n, r, 'E', 'C', n, @, t]).
entry(zusammenreden, [ts, u, z, a, m, @, n, r, 'e:', d, @, n]).
entry(zusammenrufen, [ts, u, z, a, m, @, n, r, 'u:', f, @, n]).
entry(zusammensetzen, [ts, u, z, a, m, @, n, z, 'E', ts, @, n]).
entry(zusammensetzt, [ts, u, z, a, m, @, n, z, 'E', ts, t]).
entry(zusammensitzen, [ts, u, z, a, m, @, n, z, 'I', ts, @, n]).
entry(zusammenstellen, [ts, u, z, a, m, @, n, 'S', t, 'E', l, @, n]).
entry('Zusammenstellung', [ts, u, z, a, m, @, n, 'S', t, 'E', l, 'U', 'N']).
entry(zusammenstreichen, [ts, u, z, a, m, @, n, 'S', t, r, aI, 'C', @, n]).
entry(zusammentreffen, [ts, u, z, a, m, @, n, t, r, 'E', f, @, n]).
entry(zusammen, [ts, u, z, a, m, @, n]).
entry(zusammentun, [ts, u, z, a, m, @, n, t, 'u:', n]).
entry(zusammenzulegen, [ts, u, z, a, m, @, n, ts, u, l, 'e:', g, @, n]).
entry(zusammenzurufen, [ts, u, z, a, m, @, n, ts, u, r, 'u:', f, @, n]).
entry(zusammenzusetzen, [ts, u, z, a, m, @, n, ts, u, z, 'E', ts, @, n]).
entry(zusa, [ts, u, z, a]).
entry(zusätzliche, [ts, 'u:', z, 'E', ts, l, 'I', 'C', @]).
entry(zusätzlich, [ts, 'u:', z, 'E', ts, l, 'I', 'C']).
entry(zuschicken, [ts, 'u:', 'S', 'I', k, @, n]).
entry(zuschicke, [ts, 'u:', 'S', 'I', k, @]).
entry(zuschlagen, [ts, 'u:', 'S', l, 'a:', g, @, n]).
entry('Zuschlag', [ts, 'u:', 'S', l, 'a:', k]).
entry(zuschneiden, [ts, 'u:', 'S', n, aI, d, @, n]).
entry(zusehen, [ts, 'u:', z, 'e:', @, n]).
entry(zusenden, [ts, 'u:', z, 'E', n, d, @, n]).
entry(zustande, [ts, u, 'S', t, a, n, d, @]).
entry(zuständigen, [ts, 'u:', 'S', t, 'E', n, d, 'I', g, @, n]).
entry(zuständige, [ts, 'u:', 'S', t, 'E', n, d, 'I', g, @]).
entry(zuständig, [ts, 'u:', 'S', t, 'E', n, d, 'I', 'C']).
entry('Zustand', [ts, 'u:', 'S', t, a, n, t]).
entry(zusteigen, [ts, 'u:', 'S', t, aI, g, @, n]).
entry(zusteige, [ts, 'u:', 'S', t, aI, g, @]).
entry(zustimmen, [ts, 'u:', 'S', t, 'I', m, @, n]).
entry('Zustimmung', [ts, 'u:', 'S', t, 'I', m, 'U', 'N']).
entry(zus, [ts, u, z]).
entry(zutreffend, [ts, 'u:', t, r, 'E', f, @, n, t]).
entry(zutreffen, [ts, 'u:', t, r, 'E', f, @, n]).
entry(zu, [ts, 'u:']).
entry('Zu', [ts, u]).
entry(zuverlässiger, [ts, 'u:', f, 'E', '6', l, 'E', s, 'I', g, '6']).
entry(zuviel, [ts, u, f, 'i:', l]).
entry(zuvorkommen, [ts, u, f, 'o:', '6', k, 'O', m, @, n]).
entry(zuvor, [ts, u, f, 'o:', '6']).
entry(zuwenig, [ts, u, v, 'e:', n, 'I', 'C']).
entry(zuzahlen, [ts, 'u:', ts, 'a:', l, @, n]).
entry('Zuzahlung', [ts, 'u:', ts, 'a:', l, 'U', 'N']).
entry(zuzüglich, [ts, 'u:', ts, 'y:', k, l, 'I', 'C']).
entry(zuzusagen, [ts, 'u:', ts, u, z, 'a:', g, @, n]).
entry(zwangsläufig, [ts, v, a, 'N', s, l, 'OY', f, 'I', 'C']).
entry(zwängt, [ts, v, 'E', 'N', t]).
entry(zwan, [ts, v, a, n]).
entry(zwanzigminütigen, [ts, v, a, n, ts, 'I', 'C', m, 'I', n, 'y:', t, 'I', g, @, n]).
entry(zwanzigstem, [ts, v, a, n, ts, 'I', 'C', s, t, @, m]).
entry(zwanzigsten, [ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(zwanzigster, [ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(zwanzigste, [ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(zwanzigs, [ts, v, a, n, ts, 'I', 'C', s]).
entry(zwanzig, [ts, v, a, n, ts, 'I', 'C']).
entry(zwar, [ts, v, 'a:', r]).
entry(zwa, [ts, v, a]).
entry('Zwecke', [ts, v, 'E', k, @]).
entry(zwecks, [ts, v, 'E', k, s]).
entry('Zweck', [ts, v, 'E', k]).
entry(zweieinhalb, [ts, v, aI, aI, n, h, a, l, p]).
entry(zweien, [ts, v, aI, @, n]).
entry('Zweierrhythmus', [ts, v, aI, '6', r, 'Y', t, m, 'U', s]).
entry('Zweifelsfalle', [ts, v, aI, f, @, l, s, f, a, l, @]).
entry('Zweifelsfall', [ts, v, aI, f, @, l, s, f, a, l]).
entry('Zweifel', [ts, v, aI, f, @, l]).
entry('Zweigstelle', [ts, v, aI, k, 'S', t, 'E', l, @]).
entry('Zweig', [ts, v, aI, k]).
entry(zweihundert, [ts, v, aI, h, 'U', n, d, '6', t]).
entry(zweimal, [ts, v, aI, m, 'a:', l]).
entry(zweimonatsweise, [ts, v, aI, m, 'o:', n, a, ts, v, aI, z, @]).
entry('Zwei-Stunden-Takt', [ts, v, aI, 'S', t, 'U', n, d, @, n, t, a, k, t]).
entry(zweitägigen, [ts, v, aI, t, 'E:', g, 'I', g, @, n]).
entry(zweitägiger, [ts, v, aI, t, 'E:', g, 'I', g, '6']).
entry(zweitägiges, [ts, v, aI, t, 'E:', g, 'I', g, @, s]).
entry(zweitägige, [ts, v, aI, t, 'E:', g, 'I', g, @]).
entry(zweitägig, [ts, v, aI, t, 'E:', g, 'I', 'C']).
entry(zweitägi, [ts, v, aI, t, 'E:', g, 'I']).
entry(zweitausend, [ts, v, aI, t, aU, z, @, n, t]).
entry(zweiteilen, [ts, v, aI, t, aI, l, @, n]).
entry(zweitens, [ts, v, aI, t, @, n, s]).
entry(zweiten, [ts, v, aI, t, @, n]).
entry(zweiter, [ts, v, aI, t, '6']).
entry(zweites, [ts, v, aI, t, @, s]).
entry(zweite, [ts, v, aI, t, @]).
entry(zweitgenannte, [ts, v, aI, t, g, @, n, a, n, t, @]).
entry(zwei, [ts, v, aI]).
entry(zweit, [ts, v, aI, t]).
entry(zweiunddreißigsten, [ts, v, aI, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @, n]).
entry(zweiunddreißigste, [ts, v, aI, 'U', n, t, d, r, aI, s, 'I', 'C', s, t, @]).
entry(zweiunddreißig, [ts, v, aI, 'U', n, t, d, r, aI, s, 'I', 'C']).
entry(zweiundneunzig, [ts, v, aI, 'U', n, t, n, 'OY', n, ts, 'I', 'C']).
entry(zweiundsechzig, [ts, v, aI, 'U', n, t, z, 'E', 'C', ts, 'I', 'C']).
entry(zweiundsiebzig, [ts, v, aI, 'U', n, t, z, 'i:', p, ts, 'I', 'C']).
entry(zweiund, [ts, v, aI, 'U', n, t]).
entry(zweiundvierzigsten, [ts, v, aI, 'U', n, t, f, 'I', '6', ts, 'I', 'C', s, t, @, n]).
entry(zweiundvierzig, [ts, v, aI, 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(zweiundz, [ts, v, aI, 'U', n, t, ts]).
entry(zweiundzwanzigsten, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(zweiundzwanzigster, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(zweiundzwanzigste, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(zweiundzwanzigs, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C', s]).
entry(zweiundzwanzig, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(zweiundzwanzingns, [ts, v, aI, 'U', n, t, ts, v, a, n, ts, 'I', 'N', j, s]).
entry(zweiundzwanz, [ts, v, aI, 'U', n, t, ts, v, a, n, ts]).
entry(zweiun, [ts, v, aI, 'U', n]).
entry(zwe, [ts, v, 'E']).
entry(zwingende, [ts, v, 'I', 'N', @, n, d, @]).
entry(zwingen, [ts, v, 'I', 'N', @, n]).
entry(zwischendrin, [ts, v, 'I', 'S', @, n, d, r, 'I', n]).
entry(zwischendurch, [ts, v, 'I', 'S', @, n, d, 'U', '6', 'C']).
entry('Zwischenfrage', [ts, v, 'I', 'S', @, n, f, r, 'a:', g, @]).
entry(zwischenliegen, [ts, v, 'I', 'S', @, n, l, 'i:', g, @, n]).
entry('Zwischenraum', [ts, v, 'I', 'S', @, n, r, aU, m]).
entry('Zwischenstop', [ts, v, 'I', 'S', @, n, 'S', t, 'O', p]).
entry(zwischen, [ts, v, 'I', 'S', @, n]).
entry('Zwischenzeit', [ts, v, 'I', 'S', @, n, ts, aI, t]).
entry(zwisch, [ts, v, 'I', 'S']).
entry(zwisn, [ts, v, 'I', s, n]).
entry(zwi, [ts, v, 'I']).
entry(zwohundert, [ts, v, 'o:', h, 'U', n, d, '6', t]).
entry(zwölftem, [ts, v, '9', l, f, t, @, m]).
entry(zwölften, [ts, v, '9', l, f, t, @, n]).
entry(zwölfter, [ts, v, '9', l, f, t, '6']).
entry(zwölfte, [ts, v, '9', l, f, t, @]).
entry(zwölf, [ts, v, '9', l, f]).
entry(zwölft, [ts, v, '9', l, f, t]).
entry(zwomal, [ts, v, 'o:', m, 'a:', l]).
entry(zwom, [ts, v, 'o:', m]).
entry(zwoten, [ts, v, 'o:', t, @, n]).
entry(zwoter, [ts, v, 'o:', t, '6']).
entry(zwote, [ts, v, 'o:', t, @]).
entry(zwo, [ts, v, 'o:']).
entry(zwoundvierzig, [ts, v, 'o:', 'U', n, t, f, 'I', '6', ts, 'I', 'C']).
entry(zwoundzwanzigsten, [ts, v, 'o:', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @, n]).
entry(zwoundzwanzigster, [ts, v, 'o:', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, '6']).
entry(zwoundzwanzigste, [ts, v, 'o:', 'U', n, t, ts, v, a, n, ts, 'I', 'C', s, t, @]).
entry(zwoundzwanzig, [ts, v, 'o:', 'U', n, t, ts, v, a, n, ts, 'I', 'C']).
entry(zw, [ts, v]).
entry('Zyklus', [ts, 'y:', k, l, 'U', s]).
entry('Zylinder', [ts, y, l, 'I', n, d, '6']).