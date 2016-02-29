%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej8.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(gunBasic).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '.', '#', '.', '#', '.', '#', '.', '#', '.', '#'],
['#', ' ', '#', '.', '#', ' ', '#', '.', '#', '.', '#', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(13, 5).
num_dots(21).
pacman_start(1, 3).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_GUN, 'l', object, 5, 3, inactive, norule, 
			[name(derringer), solid(false), static(false), use_rule(gunBasic),
			description('Pistola de una sola bala. Usala bien.'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(EID_0, 'E', mortal, 11, 1, inactive, norule, [appearance(attribs(normal, red, default))]),
	gunBasic(init, OID_GUN, 1, ['E'], destroy).
norule(_).
norule(_,_,_,_,_).
