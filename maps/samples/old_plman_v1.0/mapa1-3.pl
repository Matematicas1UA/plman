%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa1-chus.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 1
%Puntuaci�n: 100

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', ' ', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(20, 9).
num_dots(95).
pacman_start(2, 1).
initMap:- 
	retractall(d_posicion(_,_)),
	addSolidObject('#'), 
	createGameEntity('E', mortal, 13, 2, active, reglaControlFantasma, 0).

:- dynamic d_posicion/2.

listamov([left,left,left,left,down,down,right,down,down,left,left,left,left,left,up,up,up,right,right,right,right,right,down,right,right,right,up,up]).

reglaControlFantasma(ID):-
	not(d_posicion(ID,_)),
	assert(d_posicion(ID,0)), !.

reglaControlFantasma(ID):-
        retract(d_posicion(ID, N)),
	listamov(Mov),
	length(Mov, Max), 
	(N < Max -> THIS=N ; THIS=0),
	nth0(THIS, Mov, DIR),
	assert(d_doAction(ID, move(DIR))),
	NEXT is THIS+1,
	assert(d_posicion(ID,NEXT)), !.

	
