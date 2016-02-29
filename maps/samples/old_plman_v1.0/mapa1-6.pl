%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapafase1-2.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 2
%Puntuaci�n: 150

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '#'],
['#', '.', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '#', '.', '.', '#', '#', '#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', ' ', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', '.', '.', '.', ' ', '.', '#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '#', '.', '.', '#', '#', '#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(23, 11).
num_dots(139).
pacman_start(7, 5).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('E', mortal, 11, 1, active, patrullar(horizontal), 0), 
	createGameEntity('E', mortal, 21, 6, active, patrullar(vertical), 0), 
	createGameEntity('E', mortal,  2, 6, active, patrullar(vertical), 0), 
	createGameEntity('F', mortal, 16, 3, active, ghostRule, 0),
	createGameEntity('F', mortal, 19, 9, active, ghostRule, 0),
	createGameEntity('F', mortal, 10, 7, active, ghostRule, 0).
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

ghostRule(EID):-
    seedir(DIR),
    see(EID, list, DIR, SEELIST),
    member('@', SEELIST),
    assertz(d_doAction(EID, move(DIR))), !.
ghostRule(_).
