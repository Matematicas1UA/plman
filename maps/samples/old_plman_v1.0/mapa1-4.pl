%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-chus.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 1
%Puntuación: 100

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', ' ', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(20, 9).
num_dots(93).
pacman_start(4, 6).
initMap:- 
	retractall(d_direccion(_,_)),
	addSolidObject('#'), 
	createGameEntity('E', mortal, 13, 2, active, patrullar(vertical), 0), 
	createGameEntity('E', mortal, 14, 5, active, patrullar(horizontal), 0).

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
	assert(d_doAction(ID,move(NEXTDIR))),
	assert(d_direccion(ID,NEXTDIR)), !.

% NO VEO PARED
patrullar(_,ID):- 
	d_direccion(ID,DIR),
	assert(d_doAction(ID,move(DIR))), !.

norule(_).
