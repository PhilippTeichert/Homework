:- dynamic nextto/3.

% nextto(?X, ?Y, ?List)
% "Y ist der Nachfolger von X in List".

"Gönnt euch hart!" -[Louis]

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|T]) :-
	nextto(X, Y, T).
	
	
	
	
	
	
	
nth0(0, [Element|_], Element).
	
nth0(Index, [_|T], Element) :-
	nth0(Index2, T, Element),
	Index is Index2 + 1.
	
	
	
	
max_member(_, []).
max_member(Max, [Max]).
max_member(Max, [H|T]) :-
	max(Max, H, Max2),
	max_member(Max2, T).
	
max(Ele1, Ele2, Erg) :-
	Ele1 > Ele2,
	Erg = Ele1.
max(Ele1, Ele2, Erg) :-
	Ele2 <= Ele2,
	Erg = Ele2.
	
	
	
	
merge([], [], []).
merge([A], [], [A]).
merge([], [B], [B]).
merge([H1|T1], [H2|T2], Union) :-
	H1 < H2,
	merge(T1, [H2|T2], Union2),
	Union is [H1|Union2].
merge([H1|T1], [H2|T2], Union) :-
	H1 >= H2,
	merge([H1|T1], T2, Union2),
	Union is [H2|Union2].