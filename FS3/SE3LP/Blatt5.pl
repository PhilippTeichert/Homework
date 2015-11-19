p(1, 2).
p(2, 3).
p(3, 4).
p(A, C) :-
	p(A, B),
	p(B, C).


p2(1, 2).
p2(2, 3).
p2(3, 4).
p3(A, C) :-
	p2(A, B),
	p3(B, C).


peano2int/2

peano2int(Peano, Int) :-
	Peano = 0.

peano2int(s(Peano), Int - 1) :-
	peano2int(Peano, Int).