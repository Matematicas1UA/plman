%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapafase1-3.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad:2
%Puntuaci�n:150

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '#', ' ', ' ', '#', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '#'],
['#', '.', '#', ' ', ' ', '#', '.', '#', '.', '#', '#', '#', '.', '.', '#', '#', '.', '#', '#', '#', '#', '#', '#', '.', '.', '#'],
['#', '.', '#', ' ', ' ', '.', '.', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#', '.', '.', '#'],
['#', '.', '#', ' ', ' ', '#', '.', '#', '.', '.', '.', '#', '.', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#', '.', '.', '#'],
['#', '.', '#', ' ', ' ', '#', '.', '.', '.', '#', '.', '#', '.', ' ', '.', '.', '.', '#', '.', '#', '.', '.', '#', '.', ' ', '#'],
['#', '.', ' ', ' ', ' ', '#', '.', '#', '.', '#', '.', '#', '#', '#', '#', '.', '.', '#', '.', '#', '.', '.', '#', '.', '.', '#'],
['#', '.', '#', ' ', ' ', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '.', '#', '.', '.', '#', '#', '#', '#'],
['#', '.', '#', ' ', ' ', '#', '.', '#', '.', '#', '.', '#', '.', '.', '.', '.', '#', '.', '.', ' ', '.', '.', '#', '#', '#', '#'],
['#', '.', '#', ' ', ' ', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(26, 11).
num_dots(123).
pacman_start(13, 5).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('+', object,  1, 1, inactive, norule, 
	                 data(llave, not_solid, non_static, llaveUsoRule, 'Llave')), 
	createGameEntity('+', object, 24, 5, inactive, norule, 
	                 data(llave, not_solid, non_static, llaveUsoRule, 'Llave')), 
	createGameEntity('|', object,  2, 6, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('|', object, 19, 8, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('E', mortal, 21, 1, active, patrullar(horizontal), 0), 
	createGameEntity('F', mortal,  4, 9, active, ghostRule, 0),
	createGameEntity('F', mortal,  3, 1, active, ghostRule, 0),
	createGameEntity('F', mortal, 18, 9, active, ghostRule, 0).
norule(_).

:- dynamic d_direccion/2.

direccion(vertical,up,down).
direccion(vertical,down,up).
direccion(horizontal,left,right).
direccion(horizontal,right,left).

% CASO INICIAL
patrullar(SENT,ID):- 
	not(d_direccion(ID,_)),
	direccion(SENT,DIR,_),
	assert(d_direccion(ID,DIR)), !.

% VEO PARED
patrullar(SENT, ID):- 
	d_direccion(ID,DIR),
	see(ID, normal, DIR, '#'),
	retract(d_direccion(ID,_)),
	direccion(SENT,DIR,NEXTDIR),
%	assert(d_doAction(ID,move(NEXTDIR))),
	assert(d_direccion(ID,NEXTDIR)), !.

% NO VEO PARED
patrullar(_,ID):- 
	d_direccion(ID,DIR),
	assert(d_doAction(ID,move(DIR))), !.

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


llaveUsoRule(_,_, X, Y, _):-
	not(d_entity(_, object, _, _, location(X, Y, _), data(puerta, _, _, _, _))),
	writeln('No puedo usar la llave asi!'), !.

llaveUsoRule(LlaveId,_, X, Y, _):-
        entityLocation(PtaEID,X,Y,'|'),
	changeEntityAppearance(PtaEID,']',_),
	makeObjectNotSolid(PtaEID),
	changeObjectName(PtaEID,puerta_abierta,puerta),
	destroyGameEntity(LlaveId),
	writeln('Puerta abierta!'), !.







