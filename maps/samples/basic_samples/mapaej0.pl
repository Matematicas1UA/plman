%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej0.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(16, 5).
num_dots(13).
pacman_start(1, 2).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'E', mortal, 5, 1, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_1, 'E', mortal, 10, 3, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, up-down, ['#']),
	enemyBasicMovement(init, EID_1, up-down, ['#']).
norule(_).
norule(_,_,_,_,_).
