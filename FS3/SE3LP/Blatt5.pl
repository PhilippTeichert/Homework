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
	
%%%%
% Stuff aus der VL
%%%%
	
%i2p(+Integer,?Peano)

% i2p(0,0).
% i2p(1,s(0)).
% i2p(2,s(s(0))).
% i2p(3,s(s(s(0)))).

i2p(0,0).
i2p(I,s(X)) :-
	I>0,
	I1 is I - 1,
	i2p(I1,X).

% (?,+)
i22p(0,0).
i22p(I,s(X)) :-
	i22p(I1,X),
	I is I1 + 1.
	
	
% ggt(A,B,GGT)
% A, B aus |N
ggt(A,A,A).
ggt(A,B,GGT) :-
	B > A,
	ggt(B,A,GGT).
ggt(A,B,GGT) :-
	R is A - B,
	ggt(R,B,GGT).
	
	
% rbm(+Fakt1,+Fakt2,?Produkt)

rbm(1,B,B).
rbm(A,B,P) :-
	A > 1,
	odd(A),
	AA is A -1,
	rbm(AA,B,PP),
	P is PP + B.
rbm(A,B,P) :-
	even(A),
	AA is A / 2,
	BB is B * 2,
	rbm(AA,BB,P).
	
odd(X) :- 1 is X mod 2.
even(X) :- \+ odd(X).
% even(X) :- 0 is X mod 2.