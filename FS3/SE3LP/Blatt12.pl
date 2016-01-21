%%%%%%%% Aufgabe1

%%%%% Aufgabe 1.1

%(list (car (cdr (quote (1 2 3 4))))
%    (cdr (quote (1 2 3 4))) )
%
%    (car (cdr (quote (1 2 3 4))))
%        (cdr (quote (1 2 3 4)))
%            (quote (1 2 3 4))
%            ==> (1 2 3 4)
%        ==> (2 3 4)
%    ==> 2
%    (cdr (quote (1 2 3 4)))
%        (quote (1 2 3 4))
%        ==> (1 2 3 4)
%    ==> (2 3 4)
%==> (2 2 3 4)

%%%% Aufgabe 1.2

%(if (< (car (cdr (quote (5 -3 4 -2)))) (- 2 6)) 0 1)
%
%    (< (car (cdr (quote (5 -3 4 -2)))) (- 2 6))
%        (car (cdr (quote (5 -3 4 -2))))
%            (cdr (quote (5 -3 4 -2)))
%                (quote (5 -3 4 -2))
%                ==> (5 -3 4 -2)
%            ==> (-3 4 -2)
%        ==> -3
%        (- 2 6)
%        ==> -4
%    ==> #f
%==> 1

%%%% Aufgabe 1.3

%(cons (cdr (quote (1 2 3 4)))
%    (car (quote (1 2 3 4))))
%
%    (cdr (quote (1 2 3 4)))
%        (quote (1 2 3 4))
%        ==> (1 2 3 4)
%    ==> (2 3 4)
%    (car (quote (1 2 3 4)))
%        (quote (1 2 3 4))
%        ==> (1 2 3 4)
%    ==> 1
%==> (2 3 4 1)

%%%% Aufgabe 1.4

%(map (lambda (x) (if (pair? x) (car x) x))
%    (quote (lambda (x) (if (pair? x) (car x) x))))
%
%    ==> x = lambda
%    (lambda (lambda) (if (pair? lambda) (car lambda) lambda))
%        (if (pair? lambda) (car lambda) lambda)
%            (pair? lambda)
%            ==> #f
%        ==> lambda
%    ==> lambda
%    (lambda ((x)) (if (pair? (x)) (car (x)) (x)))
%        (if (pair? (x)) (car (x)) (x))
%            (pair? (x))
%            ==> #t
%        ==> (car (x))
%    ==> x
%    (lambda ((if (pair? x) (car x) x)) (if (pair? (if (pair? x) (car x) x)) (car (if (pair? x) (car x) x)) (if (pair? x) (car x) x)))
%        (if (pair? (if (pair? x) (car x) x)) (car (if (pair? x) (car x) x)) (if (pair? x) (car x) x))
%            (pair? (if (pair? x) (car x) x))
%            ==> #t
%        ==> (car (if (pair? x) (car x) x))
%    ==> if
%==> (lambda x if)

%%%% Aufgabe 1.5

%(filter (curry > 5)
%    (reverse (quote (1 3 5 7 9))))
%
%    (curry > 5)
%    ==> (lambda (x) (> 5 x))
%    (reverse (quote (1 3 5 7 9)))
%        (quote (1 3 5 7 9))
%        ==> (1 3 5 7 9)
%    ==> (9 7 5 3 1)
%    (lambda (9) (> 5 9))
%    ==> #f
%    (lambda (7) (> 5 7))
%    ==> #f
%    (lambda (5) (> 5 5))
%    ==> #f
%    (lambda (3) (> 5 3))
%    ==> #t
%    (lambda (1) (> 5 1))
%    ==> #t
%==> (3 1)

%%%% Aufgabe 1.6

%(filter (compose positive?
%        (lambda (x) (- x 5)))
%    (quote (1 3 5 7 9)))
%
%    (compose positive?
%        (lambda (x) (- x 5)))
%    ==> (positive? (lambda (x) (- x 5)))
%    (positive? (lambda (1) (- 1 5)))
%        (lambda (1) (- 1 5))
%            (- 1 5)
%            ==> -4
%        ==> -4
%    ==> #f
%    (positive? (lambda (3) (- 3 5)))
%        (lambda (3) (- 3 5))
%            (- 3 5)
%            ==> -2
%        ==> -2
%    ==> #f
%    (positive? (lambda (5) (- 5 5)))
%        (lambda (5) (- 5 5))
%            (- 5 5)
%            ==> 0
%        ==> 0
%    ==> #f
%    (positive? (lambda (7) (- 7 5)))
%        (lambda (7) (- 7 5))
%            (- 7 5)
%            ==> 2
%        ==> 2
%    ==> #t
%    (positive? (lambda (9) (- 9 5)))
%        (lambda (9) (- 9 5))
%            (- 9 5)
%            ==> 4
%        ==> 4
%    ==> #t
%==> (7 9)

%%%%%%%% Aufgabe 2

%%%% Aufgabe 2.1

% Die folgende Funktion prüft, ob in beiden Listen dieselben Atome enthalten sind.

foobar([], []).
foobar([XH|XT], [YH|YT]) :-
    XH = YH,
    foobar(XT, YT).

%%%% Aufgabe 2.2

% Die folgende Funktion entfernt Duplikate aus einer Liste.

foo2([], []) :-
    !. % weil Racket nur ein Ergebnis ausspuckt.
foo2([H|T], R) :-
    member(H, T),
    foo2(T, R).
foo2([H|T], [H|R]) :-
    \+ member(H, T),
    foo2(T, R).

%%%% Aufgabe 2.3

% Die folgende Funktion prüft, welche Elemente aus der Liste X in der Liste Y enthalten sind.

foo3([], _, []) :-
    !. % weil Racket nur ein Ergebnis ausspuckt.
foo3([XH|XT], Y, [XH|R]) :-
    member(XH, Y),
    foo3(XT, Y, R).
foo3([XH|XT], Y, R) :-
    \+ member(XH, Y),
    foo3(XT, Y, R).

%%%% Aufgabe 2.4

% Die folgende Funktion gibt eine Liste rückwärts wieder aus.

foo4([], []).
foo4([XH|XT], R) :-
    foo4(XT, R2),
    append(R2, [XH], R).

%%%% Aufgabe 2.5

% Die folgende Funktion wandelt eine Matrix in eine Liste um.

foo5([], []) :-
    !. % weil Racket nur ein Ergebnis ausspuckt.
foo5([XH|XT], R) :-
    XH = [_|_],
    foo5(XH, R2),
    foo5(XT, R3),
    append(R2, R3, R).
foo5([XH|XT], R) :-
    \+ XH = [_|_],
    foo5(XT, R3),
    append([XH], R3, R).

%%%%%%%% Aufgabe 3

%(define (peano? x)
%    (if (null? x)
%        #t
%        (cond [(eq? (car x) 's) (peano? (cdr x))]
%              [else (error "Dies ist keine Peano Zahl")])))

%(define (lt x y)
%    (if (and (peano? x) (peano? y))
%        (< (length x) (length y))
%        (error "Die Eingabe besteht nicht aus Peano Zahlen")))

%; für integer2peano wurden 2 Funktionen geschrieben, um die gleiche Funktionalität wie in Prolog gewährleisten zu können
%(define (integer2peano x)
%    (if (> x 0)
%        (cons 's (integer2peano (- x 1)))
%        '()))

%(define (peano2integer x)
%    (if (peano? x)
%        (length x)
%        (error "Dies ist keine Peano Zahl")))

%(define add (lambda (x y)
%    (if (and (peano? x) (peano? y))
%        (append x y)
%        (error "Die Eingabe besteht nicht aus Peano Zahlen"))))

%; alternative Idee:
%(define (add2 x y)
%    (integer2peano (+ (peano2integer x) (peano2integer y))))