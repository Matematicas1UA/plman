map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#'],
['#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 11).
num_dots(11).
pacman_start(7, 7).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_RD, '|', object, 2, 1, inactive, norule, 
			[name(puerta_roja), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave roja'), appearance(attribs(normal, black, red))]), 
	createGameEntity(OID_BD, '|', object, 2, 9, inactive, norule, 
			[name(puerta_azul), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave azul'), appearance(attribs(normal, black, cyan))]), 
	createGameEntity(OID_BK, 'a', object, 11, 4, inactive, norule, 
			[name(llave_azul), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta azul'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_RK, 'r', object, 12, 8, inactive, norule, 
			[name(llave_roja), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta roja'), appearance(attribs(bold, red, default))]), 
	basicDoorKey(init, OID_BD, [ 'pl-man':destroyGameEntity(OID_BD) ], [ OID_BK ]),
	basicDoorKey(init, OID_RD, [ 'pl-man':destroyGameEntity(OID_RD) ], [ OID_RK ]).
norule(_).
norule(_,_,_,_,_).
