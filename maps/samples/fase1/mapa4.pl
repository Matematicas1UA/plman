%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', ' ', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '#', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '.', '#'],
['#', '.', '#', ' ', '.', '.', ' ', '#', '.', '#'],
['#', '.', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '#', '.', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(10, 9).
num_dots(44).
pacman_start(4, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'E', mortal, 3, 4, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	createGameEntity(EID_1, 'E', mortal, 6, 4, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, left-right, ['#']),
	enemyBasicMovement(init, EID_1, down-up, ['#']).
norule(_).
norule(_,_,_,_,_).
