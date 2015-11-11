:- dynamic mutter_von/2, vater_von/2, kind_von/2, enkelkind_von/2.        % ermoeglicht dynamische Veraenderung
% :- multifile mutter_von/2, vater_von/2, kind_von/2, enkelkind_von/2.      % ermoeglicht verteilte Definition in mehreren Files

% mutter_von( Mutter , Kind ).
% 'Mutter' und 'Kind' sind Argumentpositionen,
% sodass 'Mutter' die Mutter von 'Kind' ist.

mutter_von( julia , otto ).
mutter_von( charlotte , barbara ).
mutter_von( charlotte , magdalena ).
mutter_von( marie , hans ).
mutter_von( marie , helga ).
mutter_von( marie , anneliese ).
mutter_von( barbara , andrea ).
mutter_von( barbara , klaus ).
mutter_von( magdalena , peter ).
mutter_von( anneliese , justin ).
mutter_von( anneliese , lotta ).


% vater_von( Vater , Kind ).
% 'Vater' und 'Kind' sind Argumentpositionen,
% sodass 'Vater' der Vater von 'Kind' ist.

vater_von( gerd , otto ).
vater_von( gerd , peter ).
vater_von( walter , barbara ).
vater_von( walter , magdalena ).
vater_von( otto , hans ).
vater_von( otto , helga ).
vater_von( johannes , anneliese ).
vater_von( johannes , klaus ).
vater_von( johannes , andrea).
vater_von( peter, justin ).
vater_von( peter, lotta ).

kind_von(Kind, Von) :- vater_von(Von, Kind); mutter_von(Von, Kind).
enkelkind_von(Enkel, Von) :- kind_von(Kind, Von),  kind_von(Enkel, Kind).