map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(17, 3).
num_dots(1).
pacman_start(3, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_0, '|', object, 2, 1, inactive, norule, 
			[name(puerta), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave azul'), appearance(attribs(normal, black, cyan))]), 
	createGameEntity(OID_1, 'a', object, 15, 1, inactive, norule, 
			[name(llave), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta azul'), appearance(attribs(bold, cyan, default))]),
	basicDoorKey(init, OID_0, [ 'pl-man':destroyGameEntity(OID_0)  ], [ OID_1 ]).
norule(_).
norule(_,_,_,_,_).
