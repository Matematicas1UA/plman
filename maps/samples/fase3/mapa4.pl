%
% Los 3 enemigos aparecen siempre en las mismas columnas, pero en filas aleatorias.
%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', ' ', ' ', ' ', '.', ' ', '.', ' ', '.', ' ', '.', ' ', '.', '#'],
['#', ' ', '.', ' ', '.', ' ', '.', ' ', '.', ' ', ' ', ' ', '.', ' ', '#'],
['#', '.', ' ', ' ', ' ', '.', ' ', '.', ' ', '.', ' ', '.', ' ', '.', '#'],
['#', ' ', '.', ' ', '.', ' ', '.', ' ', '.', ' ', ' ', ' ', '.', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(15, 7).
num_dots(22).
pacman_start(1, 2).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'E', mortal,  8, rnd(1,4), active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_1, 'E', mortal,  5, rnd(1,4), active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_2, 'E', mortal, 11, rnd(1,4), active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, up-down, ['#']),
	enemyBasicMovement(init, EID_1, down-up, ['#']),
	enemyBasicMovement(init, EID_2, up-down, ['#']).
norule(_).
norule(_,_,_,_,_).
