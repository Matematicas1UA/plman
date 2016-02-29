%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapafase1-5.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 3
%Puntuaci�n: 200

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '.', '.', '.', '.', '.', ' ', ' ', '.', '.', '.', ' ', '.', '.', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '#'],
['#', ' ', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#'],
['#', ' ', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(24, 11).
num_dots(108).
pacman_start(1, 7).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('+', object, 7, 1, inactive, norule, 
	                 data(llave, not_solid, not_static, llaveUsoRule, 'Llave')), 
	createGameEntity('?', object, 10, 5, active, pasadizoRule, 
	                 data(pasadizo, not_solid, not_static, norule, 'Teletransporte')), 
	createGameEntity('-', object, 18, 4, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('!', object, 15, 5, inactive, norule, 
	                 data(dinamita, not_solid, static, dinamitaUsoRule, 'Dinamita')), 
	createGameEntity('_', object, 1, 8, inactive, norule, 
	                 data(puertab, solid, not_static, norule, 'Puerta blindada')), 
	createGameEntity('F', mortal, 11, 5, active, ghostRule, 0).
norule(_).

pasadizoRule(EID):-
	entityLocation(EID, X, Y, _),
	entityLocation(PacId,X, Y,'@'),
	getDMap(Map),
	moveEntity(PacId, Map, 1, 7),
	msgWindowWriteln('Oh! No es posible! Has sido teletransportado!').
	

llaveUsoRule(LlaveId,_, X, Y, _):-
        entityLocation(PtaEID,X,Y,'-'),
	changeEntityAppearance(PtaEID,'/',_),
	makeObjectNotSolid(PtaEID),
	changeObjectName(PtaEID,puerta_abierta,puerta),
	destroyGameEntity(LlaveId),
	msgWindowWriteln('Puerta abierta!'), !.
llaveUsoRule(_,_, _, _, _):-
	msgWindowWriteln('No puedo usar la llave asi!'), !.

dinamitaUsoRule(DinaId,_, X, Y, _):-
        entityLocation(PtaEID,X,Y,'_'),
	changeEntityAppearance(PtaEID,'',_),
	makeObjectNotSolid(PtaEID),
	changeObjectName(PtaEID,puertab_abierta,puertab),
	destroyGameEntity(DinaId),
	msgWindowWriteln('KABOOOM!!!!! Vaya, la puerta se ha abierto :)'), !.
dinamitaUsoRule(_,_, _, _, _):-
	msgWindowWriteln('No puedo usar la llave asi!'), !.


seedir(left).
seedir(right).
seedir(up).
seedir(down).

ghostRule(EID):-
    seedir(DIR),
    see(EID, list, DIR, SEELIST),
    member('@', SEELIST),
    assertz(d_doAction(EID, move(DIR))), !.
ghostRule(_).
norule(_).
norule(_,_,_,_,_).
