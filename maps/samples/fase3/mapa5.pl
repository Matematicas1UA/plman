%%
%% En este mapa los enemigos pueden aparecer en varias columnas aleatorias. 
%% Enemigo 1: columnas 13-14, Enemigo 2: columnas 15-16, Enemigo 3: columnas 17-18
%%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
load_behaviour(gunBasic).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(21, 4).
num_dots(17).
pacman_start(2, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_G, 'l', object, 1, 1, inactive, norule,
			[name(pistola), solid(false), static(false), use_rule(gunBasic),
			description('Pistola cargada con 3 balas'), appearance(attribs(bold, cyan, default))]), 
	gunBasic(init, OID_G, 3, ['E'], keep),
	createGameEntity(EID_0, 'E', mortal, rnd(13,14), 1, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_1, 'E', mortal, rnd(15,16), 1, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_2, 'E', mortal, rnd(17,18), 1, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, left-right, ['#']),
	enemyBasicMovement(init, EID_1, left-right, ['#']),
	enemyBasicMovement(init, EID_2, left-right, ['#']).
norule(_).
norule(_,_,_,_,_).
