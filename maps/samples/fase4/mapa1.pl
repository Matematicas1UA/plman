%%%
%%% En este mapa pl-man comienza en posición aleatoria en la zona central del mapa,
%%% entre las columnas 3-13 y las filas 2-5. Los enemigos empiezan siempre en 2 esquinas,
%%% de forma determinista, pero su comportamiento es completamente caótico. Sus
%%% movimientos son 100% aleatorios.
%%%

map_format_version(1.0).
load_behaviour(entitySequentialMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(17, 8).
num_dots(75).
pacman_start(X, Y) :- randomBetween(3,13,X), randomBetween(2,5,Y).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'E', mortal, 1, 1, active,  entitySequentialMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_1, 'E', mortal, 15, 5, active, entitySequentialMovement, [appearance(attribs(normal, red, default))]),
	entitySequentialMovement(init, EID_0, [ x ], [ no_repeat_moves ]),
	entitySequentialMovement(init, EID_1, [ x ], [ no_repeat_moves ]).
norule(_).
norule(_,_,_,_,_).
