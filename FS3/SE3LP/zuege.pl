:- dynamic zug/6, bahnhof/2.        % ermoeglicht dynamische Veraenderung
% :- multifile zug/6, bahnhof/2.      % ermoeglicht verteilte Definition in mehreren Files

% zug(Startbahnhof, Zielbahnhof, Startzeit, Zielzeit, Linie, Eiweihungsjahr)
% 'Startbahnhof', 'Zielbahnhof', 'Startzeit', 'Zielzeit', 'Linie' und 'Eiweihungsjahr' sind Argumentpositionen,
% die selbsterklärend sind.

zug(kuchendorf, tortenhausen, 13:37, 14:42, z1, 2004).
zug(tortenahusen, kuchendorf, 13:37, 14:42, z1, 2005).
zug(kuchendorf, candyville, 11:11, 11:12, 2006).
zug(kuchendorf, cookie_city, 4:44, 5:55, z3, 666).
zug(kuchendorf, brownie_town, 7:59, 8:17, z4, 0).
zug(kuchendorf, zuckerstangenwald, 20:02, 20:01, z5, 2016).

% bahnhof(Zuege, Bahnhofsname)
% 'Zuege' und 'Name' sind Argumentpositionen,
% so dass 'Zuege' eine Liste an Zuegen sind, die den Bahnhof benutzen.

bahnhof([z1], tortenhausen).
bahnhof([z1, z2, z3, z4, z5], kuchendorf).
bahnhof([z2], candyville).
bahnhof([z3], cookie_city).
bahnhof([z4], brownie_town).
bahnhof([z5], zuckerstangenwald).