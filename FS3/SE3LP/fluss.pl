% liegt_stromaufwaerts_von(Fluss,Ort1,Ort2,Distanz)
% ist wahr, wenn fuer zwei an einem Fluss liegende
% Ortschaften die natuerliche Fliessrichtung des Wassers von
% Ort1 zu Ort2 verlaeuft 
% Distanz ist die Entfernung zwischen diesen Orten in Kilometern

stromaufwaerts(moldau,praha,muendung_moldau,38).

stromaufwaerts(elbe,muendung_moldau,usti,70). 
stromaufwaerts(elbe,usti,dresden,93).
stromaufwaerts(elbe,dresden,meissen,26).
stromaufwaerts(elbe,meissen,torgau,73).
stromaufwaerts(elbe,torgau,rosslau,102).
stromaufwaerts(elbe,rosslau,muendung_saale,33).
stromaufwaerts(elbe,muendung_saale,magdeburg,35).
stromaufwaerts(elbe,magdeburg,tangermuende,63).
stromaufwaerts(elbe,tangermuende,muendung_havel,34).
stromaufwaerts(elbe,muendung_havel,wittenberge,31).
stromaufwaerts(elbe,wittenberge,schnackenburg,21).
stromaufwaerts(elbe,schnackenburg,geesthacht,111).
stromaufwaerts(elbe,geesthacht,hamburg,22).
stromaufwaerts(elbe,hamburg,muendung_elbe,125).

stromaufwaerts(saale,calbe,muendung_saale,20).
stromaufwaerts(saale,bernburg,calbe,16).
stromaufwaerts(saale,halle,bernburg,57).

stromaufwaerts(mulde,bitterfeld,rosslau,27).
stromaufwaerts(mulde,wurzen,bitterfeld,47).

stromaufwaerts(havel,havelberg,muendung_havel,3).
stromaufwaerts(havel,rathenow,havelberg,42).
stromaufwaerts(havel,brandenburg,rathenow,47).
stromaufwaerts(havel,muendung_spree,brandenburg,55).

stromaufwaerts(spree,berlin_mitte,muendung_spree,14).


stromaufwaerts(oder,muendung_neisse,eisenhuettenstadt,11).
stromaufwaerts(oder,eisenhuettenstadt,frankfurt_oder,33).
stromaufwaerts(oder,frankfurt_oder,schwedt,111).
stromaufwaerts(oder,schwedt,szczecin,42).
stromaufwaerts(oder,szczecin,swinoujscie,61).
