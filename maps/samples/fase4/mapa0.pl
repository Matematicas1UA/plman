%%%
%%% En este mapa, el enemigo se mueve aleatoriamente a izquierda y derecha, 
%%% quedandose quieto a veces. Nunca se mueve más de 8 casillas a la izquierda.
%%% Siempre vuelve hasta la esquina derecha al menos 1 vez en cada ciclo
%%% de 7 a 22 movimientos.
%%%

map_format_version(1.0).
load_behaviour(entitySequentialMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 3).
num_dots(9).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#'), 
	randomBetween(3,8,L), randomBetween(1,6,N),
	appendRepeatNTimes(  l,  L, [], L1),
	appendRepeatNTimes(  r,  L, L1, L2),
	appendRepeatNTimes(  n,  N, L2, L3),
	randomPermutation(L3, L3P),
	createGameEntity(EID_0, 'E', mortal, 12, 1, active, entitySequentialMovement, [appearance(attribs(normal, red, default))]),
	entitySequentialMovement(init, EID_0, L3P, [ no_repeat_moves ]).
norule(_).
norule(_,_,_,_,_).
