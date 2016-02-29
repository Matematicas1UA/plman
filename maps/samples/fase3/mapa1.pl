map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#'],
['#', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', ' ', '#'],
['#', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', ' ', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '.', '#', '#', '#', '#', '#', '#', '#', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(18, 8).
num_dots(25).
pacman_start(8, 6).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_K, 'a', object, 8, 1, inactive, norule, 
			[name(llave_azul), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta azul'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_D, '-', object, 8, 5, inactive, norule, 
			[name(puerta_azul), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave azul'), appearance(attribs(normal, black, cyan))]),
	basicDoorKey(init, OID_D, [ 'pl-man':destroyGameEntity(OID_D) ], [ OID_K ]).
norule(_).
norule(_,_,_,_,_).
