:-dynamic wallmoving/5.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '#', '#', '#', '#', '.', '.', '.', '.', '.', '#', '#', '#', '#', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '#', '#', '.', '#', '#', '#', '#', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '.', '.', '#'],
['#', '.', '#', '#', '.', '.', '.', '#', '#', '#', '#', '.', '.', '.', '.', '#', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(0, 9).
num_dots(97).
pacman_start(1,1).
initMap:-
	addSolidObject('#'),
	createGameEntity('F', mortal, 10, 1, active, ghostRule, 0),
	createGameEntity('#', solid, 10, 4, active, wallRule, 0),
	createGameEntity('-', object, 18, 7, inactive, norule, 
			 data(key, not_solid, not_static, keyUseRule, 'Llave de la puerta al más allá')),
	createGameEntity('|', object, 2, 7, inactive, norule, 
			 data(door, solid, static, norule, 'Puerta hacia el más allá')),
	retractall(wallmoving(_,_,_,_,_)).
/*
ghostRule(EID):-
       X is random(4),
	((X=0, A=up); (X=1, A=down);
  	 (X=2, A=left);  (X=3, A=right)),
	assertz(d_doAction(EID, move(A))).
*/

writelist([]):-!.
writelist([H]):- msgWindowWrite(H), !.
writelist([H|T]):- msgWindowWrite(H), msgWindowWrite(', '), writelist(T).

seedir(left).
seedir(right).

ghostRule(EID):-
	seedir(DIR),
	see(EID, list, DIR, SEELIST),
	member('@', SEELIST),
	msgWindowWrite('Veo a pacman ('), msgWindowWrite(DIR),
	msgWindowWriteln(') y voy tras el!'),
        assertz(d_doAction(EID, move(DIR))), !.
ghostRule(_).


wallRule(EID):-
	wallmoving(EID, DIR, NextD, N, TotN),
	retract(wallmoving(EID, DIR, NextD, N, TotN)),
	(N = 0
	 -> (assert(wallmoving(EID, NextD, DIR, TotN, TotN)), MOV=NextD)
	  ; (N1 is N-1, assert(wallmoving(EID, DIR, NextD, N1, TotN)), MOV=DIR)
	),
	assert(d_doAction(EID, move(MOV))), !.
wallRule(EID):-
	not(wallmoving(EID, _, _, _, _)),
	assert(wallmoving(EID, up, down, 3, 3)).
	
keyUseRule(_, _, X, Y, _):-
	not(d_entity(_, object, _, _, location(X, Y, _), data(door, _, _, _, _))),
	writeln('No puedo usar la llave ahí!'), !.
keyUseRule(_, EID, X, Y, _):-
	retract(d_entity(ObjId, object, Status, RULE, location(X, Y, _), data(door, _, S2, R, D))),
	assert(d_entity(ObjId, object, Status, RULE, location(X, Y, '['), data(door_open, not_solid, S2, R, D))),
	retractall(d_object(EID, _)),
	writeln('Puerta abierta!'), !.

