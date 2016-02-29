%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DIF:	5
%%% PT:	00:20	[STS: 01:00]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(enemyBasicMovement).
load_behaviour(pushBlocks).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '.', '.', '#', ' ', '#', '#', '.', '.', '#'],
['#', '.', '.', '#', '.', '.', '#', '.', '.', '#'],
['#', '.', '.', ' ', '.', '.', ' ', '.', '.', '#'],
['#', '.', '.', '#', '.', '.', '#', '.', '.', '#'],
['#', '.', '.', '#', '#', '.', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(10, 9).
num_dots(42).
pacman_start(8, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_BL, '%', object, 5, 6, inactive, norule, 
			[name(bloque_movil), solid(true), static(true), use_rule(norule),
			description('Bloque pesado que puede ser movido con una palanca'), appearance(attribs(bold, black, green))]), 
	createGameEntity(OID_P, 'p', object, 4, 2, inactive, norule, 
			[name(palanca), solid(false), static(false), use_rule(pushBlocks),
			description('Palanca con la que empujar bloques pesados'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(EID_0, 'E', mortal, 3, 4, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	createGameEntity(EID_1, 'E', mortal, 6, 4, active, enemyBasicMovement, [appearance(attribs(normal, red, default))]),
	enemyBasicMovement(init, EID_0, right-left, [ '#', '%' ]),
	enemyBasicMovement(init, EID_1, left-right, [ '#', '%' ]),
	pushBlocks(init, OID_P, [OID_BL]).
norule(_).
norule(_,_,_,_,_).
