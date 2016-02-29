%
% La llave roja puede aparecer en cualquier punto del rango ([2,9], [1,4])
%

map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 6).
num_dots(25).
pacman_start(1, 2).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_K, 'r', object, rnd(2,9), rnd(1,4), inactive, norule, 
			[name(llave_roja), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta roja'), appearance(attribs(bold, red, default))]),
	createGameEntity(OID_D, '|', object, 10, 4, inactive, norule, 
			[name(puerta_roja), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave roja'), appearance(attribs(normal, black, red))]),
	basicDoorKey(init, OID_D, [ 'pl-man':destroyGameEntity(OID_D) ], [ OID_K ]).
norule(_).
norule(_,_,_,_,_).
