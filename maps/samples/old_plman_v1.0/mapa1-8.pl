%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapafase1-4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 3
%Puntuaci�n:150

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', '#', '.', '#', '#', '#', '#', '.', '#', '#', '.', '#', '#', '#', '#', '#', '.', '#'],
['#', '.', '#', '#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '.', '#', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '.', '#', '.', '#', '.', '#', ' ', '#'],
['#', '.', ' ', '.', '.', '.', '.', '#', '.', '#', '.', '#', '#', '#', '#', '#', '.', '#', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '#', '.', '#'],
['#', '.', '#', '#', '#', '#', '#', '#', '.', '#', '.', '#', '.', '.', '.', '.', '.', '#', '#', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', ' ', '.', '.', '.', '.', '#', '.', '.', '.', '.', ' ', '.', '.', '.', ' ', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(24, 11).
num_dots(126).
pacman_start(14, 9).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('+', object, 22, 5, inactive, norule, 
	                 data(llave, not_solid, not_static, llaveUsoRule, 'Llave')), 
	createGameEntity('|', object, 2, 6, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('E', mortal, 4, 9, active, fantasmaRule, 0), 
	createGameEntity('E', mortal, 18, 9, active, fantasmaRule, 0),
	createGameEntity('#', solid, 13, 9, active, paredRule, 0),
        retractall(wallmoving(_,_,_,_,_)),
	retractall(next_movement(_,_)).

norule(_).

:-dynamic next_movement/2.
:-dynamic wallmoving/5.

nextto(right, up).
nextto(up, left).
nextto(left, down).
nextto(down, right).

fantasmaRule(EID):-
	not(next_movement(EID,_)),
	assert(next_movement(EID,right)),!.
fantasmaRule(EID):-
	next_movement(EID,X),
	see(EID,normal,X,'#'),
	%msgWindowWriteln('Veo pared \'O_O'),
	retract(next_movement(EID, _)),
	nextto(X, NEXT),
	assert(next_movement(EID,NEXT)), !.
fantasmaRule(EID):-
	next_movement(EID, DIR),
	assert(d_doAction(EID,move(DIR))).

llaveUsoRule(_,_, X, Y, _):-
	not(d_entity(_, object, _, _, location(X, Y, _), data(puerta, _, _, _, _))),
	writeln('No puedo usar la llave as�!'), !.

llaveUsoRule(LlaveId,_, X, Y, _):-
        entityLocation(PtaEID,X,Y,'|'),
	changeEntityAppearance(PtaEID,']',_),
	makeObjectNotSolid(PtaEID),
	changeObjectName(PtaEID,puerta_abierta,puerta),
	destroyGameEntity(LlaveId),
	writeln('Puerta abierta!'), !.


paredRule(EID):-
	wallmoving(EID, DIR, NextD, N, TotN),
	retract(wallmoving(EID, DIR, NextD, N, TotN)),
	(N = 0
	 -> (assert(wallmoving(EID, NextD, DIR, TotN, TotN)), MOV=NextD)
	  ; (N1 is N-1, assert(wallmoving(EID, DIR, NextD, N1, TotN)), MOV=DIR)
	),
	assert(d_doAction(EID, move(MOV))), !.
paredRule(EID):-
	not(wallmoving(EID, _, _, _, _)),
	assert(wallmoving(EID, up, down, 3, 3)).



