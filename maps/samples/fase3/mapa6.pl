%
% Las 3 puertas de este mapa siempre aparecen en orden aleatorio. Las llaves para abrirlas
% están siempre en la cámara de la derecha, pero en posición aleatoria (pueden estar incluso
% varias de ellas en la misma casilla)
%
% Los arqueros automáticos están siempre uno arriba a la derecha y otro abajo a la izquierda,
% pero pueden aparecer en una columna aleatoria. El de abajo a la izquierda puede estar en las 
% columnas 7 y 8, y el de arriba a la derecha puede estar en las columnas 10 y 11 
%

map_format_version(1.0).
load_behaviour(automaticArcher).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '#'],
['#', '#', '#', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '#'],
['#', '#', '#', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(15, 7).
num_dots(26).
pacman_start(6, 3).
initMap:- 
	addSolidObject('#'), 
	% Doors
	randomPermutation([1,3,5], [RDY, BDY, MDY]),
	createGameEntity(OID_RD, '|', object, 2, RDY, inactive, norule, 
			[name(puerta_roja), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave roja'), appearance(attribs(normal, black, red))]), 
	createGameEntity(OID_BD, '|', object, 2, BDY, inactive, norule, 
			[name(puerta_azul), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave azul'), appearance(attribs(normal, black, cyan))]), 
	createGameEntity(OID_MD, '|', object, 2, MDY, inactive, norule, 
			[name(puerta_magenta), solid(true), static(true), use_rule(norule),
			description('Puerta que se abre con la llave magenta'), appearance(attribs(normal, black, magenta))]), 
	% Keys
	createGameEntity(OID_RK, 'r', object, 13, rnd(1,5), inactive, norule, 
			[name(llave_roja), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta roja. Se volatiliza al usarla.'), appearance(attribs(bold, red, default))]), 
	createGameEntity(OID_BK, 'a', object, 13, rnd(1,5), inactive, norule, 
			[name(llave_azul), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta azul. Se volatiliza al usarla.'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_MK, 'm', object, 13, rnd(1,5), inactive, norule, 
			[name(llave_magenta), solid(false), static(false), use_rule(basicDoorKey),
			description('Llave que abre la puerta magenta. Se volatiliza al usarla.'), appearance(attribs(bold, magenta, default))]), 
	basicDoorKey(init, OID_RD, [ 'pl-man':destroyGameEntity(OID_RD), 'pl-man':destroyGameEntity(OID_RK) ], [ OID_RK ]),
	basicDoorKey(init, OID_MD, [ 'pl-man':destroyGameEntity(OID_MD), 'pl-man':destroyGameEntity(OID_MK) ], [ OID_MK ]),
	basicDoorKey(init, OID_BD, [ 'pl-man':destroyGameEntity(OID_BD), 'pl-man':destroyGameEntity(OID_BK) ], [ OID_BK ]),

	% Automatic Archers
	createGameEntity(OID_AR1, 'v', object, rnd(10,11), 1, active, automaticArcher, 
			[name(arquero1), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_AR2, '^', object, rnd(7,8), 5, active, automaticArcher, 
			[name(arquero1), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
   automaticArcher(init, OID_AR1, ['@'], down, 4, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_AR2, ['@'],   up, 5, [ continuous, bullet_appearance('|', bold, red, default) ]).
norule(_).
norule(_,_,_,_,_).
