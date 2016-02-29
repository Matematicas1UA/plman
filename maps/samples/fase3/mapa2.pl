%
% La llave naranja aparece siempre en la 4 fila del mapa, pero puede aparecer en la columna 1 o en la 16
% (En alguna de las dos esquinas).
%

map_format_version(1.0).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(18, 7).
num_dots(1).
pacman_start(8, 3).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_D, '-', object, 8, 2, inactive, norule, 
			[name(puerta_naranja), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave naranja'), appearance(attribs(normal, black, yellow))]), 
	createGameEntity(OID_K, 'n', object, rnd([1,16]), 4, inactive, norule, 
			[name(llave_naranja), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta naranja'), appearance(attribs(normal, yellow, default))]),
	basicDoorKey(init, OID_D, [ 'pl-man':destroyGameEntity(OID_D) ], [ OID_K ]).
norule(_).
norule(_,_,_,_,_).
