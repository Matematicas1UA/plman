%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DIF:	2
%%% PT:	00:07	[STS: 00:21]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(entitySequentialMovement).
load_behaviour(enemyBasicMovement).
load_behaviour(basicDoorKey).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', ' ', ' ', ' ', '.', '.', '.', '.', '.', ' ', '#'],
['#', '.', '.', '.', '#', '#', '#', '#', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', ' ', ' ', '#', ' ', ' ', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '.', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', ' ', ' ', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(15, 9).
num_dots(49).
pacman_start(13, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_KEY, 'a', object, 5, 3, inactive, norule, 
			[ name(llave_azul), solid(false), static(false), use_rule(basicDoorKey),
			  description('Llave que abre puertas azules'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_DOOR1, '|', object, 5, 7, inactive, norule, 
			[name(puerta_azul), solid(true), static(true), use_rule(norule),
			description('Puerta azul 2'), appearance(attribs(bold, black, cyan))]), 
	createGameEntity(EID_0, 'E', mortal, 6, 3, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]), 
	createGameEntity(EID_2, 'E', mortal, 8, 7, active, entitySequentialMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, down-up, ['#']),
	entitySequentialMovement(init, EID_2, [ r,r,u,l,l,l,d,r,n ], []),
	basicDoorKey(init, OID_DOOR1, [ 'pl-man':destroyGameEntity(OID_DOOR1) ], [ OID_KEY ]).
norule(_).
norule(_,_,_,_,_).
