%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej3.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 5).
num_dots(22).
pacman_start(12, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_K, 'a', object, 1, 1, inactive, norule, 
			[name(llave_azul), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta azul'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_D, '-', object, 2, 2, inactive, norule, 
			[name(puerta_azul), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave azul'), appearance(attribs(bold, black, cyan))]),
	basicDoorKey(init, OID_D, [ 'pl-man':destroyGameEntity(OID_D) ], [ OID_K ]).
norule(_).
norule(_,_,_,_,_).
