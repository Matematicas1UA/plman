%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej9.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(entitySequentialMovement).
load_behaviour(launchBall).
load_behaviour(soccerGoal).
map([['#', '#', '#', '#', '#', '\\', '#', '#', '#', '#', '#', '#', '#'],
[' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '/', '#', '#', '#', '#', '#', '#', '#']]).
map_size(13, 4).
num_dots(10).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#'), 
	addSolidObject('\\'), 
	addSolidObject('/'), 
	createGameEntity(OID_W, '#', object, 5, 1, active, entitySequentialMovement, 
			[name(pared_movil), solid(true), static(true), use_rule(norule),
			description('Pared que hace las veces de portero'), appearance(attribs(normal, default, default))]), 
	createGameEntity(OID_SB, 0, object, 0, 1, active, soccerGoal, 
			[name(marcador), solid(true), static(true), use_rule(norule),
			description('Marcador de goles anotados'), appearance(attribs(bold, yellow, green))]), 
	createGameEntity(OID_N1, '|', object, 6, 1, inactive, norule, 
			[name(red1), solid(true), static(true), use_rule(norule),
			description('Red de la porteria'), appearance(attribs(bold, cyan, default))]), 
	createGameEntity(OID_N2, '|', object, 6, 2, inactive, norule, 
			[name(red2), solid(true), static(true), use_rule(norule),
			description('Red de la porteria'), appearance(attribs(bold, cyan, default))]),
	createGameEntity(OID_B, 'o', object, 1, 2, inactive, launchBall, 
			[name(pelota), solid(false), static(false), use_rule(launchBall),
			description('Pelota que puede ser lanzada a puerta'), appearance(attribs(bold, cyan, default))]),
	entitySequentialMovement(init, OID_W, [ n, n, d, n, n, u ], [ ]),
	soccerGoal(init, OID_SB, 1, ['o'], [OID_N1, OID_N2], []),
   launchBall(init, OID_B, []).

norule(_).
norule(_,_,_,_,_).
