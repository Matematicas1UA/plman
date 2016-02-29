%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej7.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', '#', '#', '#', ' ', '#', '#', '#', ' ', '#', '#', ' ', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 5).
num_dots(24).
pacman_start(1, 2).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'E', mortal, 5, 2, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_1, 'E', mortal, 9, 2, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_2, 'E', mortal, 12, 2, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, up-down, ['#']),
	enemyBasicMovement(init, EID_1, down-up, ['#']),
	enemyBasicMovement(init, EID_2, up-down, ['#']).
norule(_).
norule(_,_,_,_,_).
