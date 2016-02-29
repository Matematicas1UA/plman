%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa0-5.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dificultad: 2
% puntuacion: 80

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(20, 6).
num_dots(43).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('F', mortal, 6, 1, active, patrullar(vertical), 0), 
	createGameEntity('F', mortal, 13, 3, active, patrullar(vertical), 0).

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
