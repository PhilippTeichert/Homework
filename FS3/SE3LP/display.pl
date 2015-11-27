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