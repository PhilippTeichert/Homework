:- dynamic (coin/1,
			funktion_2a/2,
			produkt/2,
			bezahlbar/2).

coin(X) :-
	integer(X).

	
funktion_2a([], 0).
funktion_2a([coin(X)|Restliste], Value) :-
	funktion_2a(Restliste, Value2),
	Value is X + Value2.
	
produkt(java, 2).
produkt(py, 3).
produkt(rkt, -12).
produkt(pl, 2).
produkt(cpp, 42).
produkt(cs, 2).
produkt(c, 1).
produkt(yml, 4).
produkt(ja, 1).
produkt(nein, 0).
produkt(bond, 007).
produkt(vodka, 10).

produkt(Zahl, Name) :-
	integer(Zahl),
	produkt(Name, Zahl).

bezahlbar(Wert, Beschaffbares) :-
	findall(Produkt, (produkt(Produkt, X), X =< Wert), Beschaffbares).

