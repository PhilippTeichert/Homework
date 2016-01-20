%%%%%%%%%%%%%
% Aufgabe 1 %
%%%%%%%%%%%%%

%%%%% Aufgabe 1

% gibt eine Beschreibung für einen Vogel aus
vogel(Vogel, Beschreibung) :-
    kannFliegen(Vogel),
    Beschreibung = kann_fliegen,
    !.
vogel(_, kann_nicht_fliegen).

% prüft, ob ein Vogel fliegen kann
kannFliegen(Vogel) :-
    member(Vogel, [strauss, pinguin]),
    !,
    false.
kannFliegen(_).


% Des ist ein Fall von nichtmonototnem Schließen, da durch zusätzliche Klauseln, weniger wahre Ergebnisse produziert werden.


%%%%%%%%%%%%%
% Aufgabe 2 %
%%%%%%%%%%%%%

%%%%% Aufgabe 2.1

% prüft, ob ein Objekt mit den Eigenschaften "Eigenschaften" alle Merkmale "Merkmale" besitzt
% "Eigenschaften" und "Merkmale" sind Listen
besitzt_merkmale(Eigenschaften, [Merkmal| Restliste]) :-
    member(Merkmal, Eigenschaften),
    besitzt_merkmale(Eigenschaften, Restliste),
    !.
besitzt_merkmale(_, []).


%%%%% Aufgabe 2.2

% gibt aus, ob ein Pilz essbar ist, anhand seiner Eigenschaften
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_roehren, hat_lamellen]),
    Name = so_ein_pilz_existiert_nicht,
    !,
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_roehren, waechst_am_baum, hat_roten_saft]),
    !,
    Name = leberpilz.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_roehren, waechst_am_baum]),
    !,
    Name = baumpilz,
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_roehren, hat_roten_stiel]),
    !,
    Name = hexenroehrling,
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_lamellen, hat_braune_kappe, hat_abwischbare_punkte]),
    !,
    Name = perlpilz.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_lamellen, hat_punkte]),
    !,
    Name = [fliegenpilz, pantherpilz| _],
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [stinkt, hat_braune_lamellen]),
    !,
    Name = karbolchampignon,
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_braune_lamellen, hat_gelbe_flecken]),
    !,
    Name = giftchampignon,
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_braune_lamellen]),
    !,
    Name = champignogn.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_lamellen]),
    !,
    Name = [knollenblaetterpilz, fliegenpilz| _],
    false.
ist_essbar(Name, Eigenschaften) :-
    besitzt_merkmale(Eigenschaften, [hat_roehren]),
    !,
    Name = [steinpilz, marone, butterpilz| _].

%%%%%%%%%%%%%
% Aufgabe 3 %
%%%%%%%%%%%%%

%%%%% Aufgabe 3

% Datenbank
woerterbuch(er, he).
woerterbuch(fing, caught).
woerterbuch(ein, a).
woerterbuch(schmetterling, butterfly).
woerterbuch(macht, makes).
woerterbuch(einen, a).

redewendung([er, hat, sich, erkaeltet], [he, caught, a, cold]).
redewendung([er, ergreift, einen, beruf], [he, takes, up, a, career]).

% übersetzt eine Liste von Worten vom Deutschen ins Englische oder umgekehrt
translate([], []).
translate(Deutsch, Englisch) :-
    redewendung(Deutsch, Englisch),
    !.
translate([Deutsch| Restliste1], [Englisch| Restliste2]) :-
    woerterbuch(Deutsch, Englisch),
    translate(Restliste1, Restliste2).
translate([Deutsch| Restliste1], [Englisch| Restliste2]) :-
    \+ woerterbuch(Deutsch, Englisch),
    Deutsch = Englisch,
    translate(Restliste1, Restliste2).

% wir haben uns für eine Datenbank entschieden, da wir so jedes Wort einzeln überprüfen können.
% zusätzlich wird geprüft, ob es sich um eine Redewendung handelt, um dies auch abzudecken.
% das translate-Prädikat ist nur Manager und Listenabbauer