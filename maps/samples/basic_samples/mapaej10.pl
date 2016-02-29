%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej10.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(automaticArcher).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(13, 6).
num_dots(22).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_1, 'v', object, 5, 1, active, automaticArcher, 
			[name(arquero1), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_2, 'v', object, 7, 1, active, automaticArcher, 
			[name(arquero2), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_5, '^', object, 2, 4, active, automaticArcher, 
			[name(arquero5), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_6, '^', object, 4, 4, active, automaticArcher, 
			[name(arquero6), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_8, '^', object, 8, 4, active, automaticArcher, 
			[name(arquero8), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_9, '^', object, 10, 4, active, automaticArcher, 
			[name(arquero9), solid(false), static(true), use_rule(norule),
			description('Arquero automatico del rey'), appearance(attribs(bold, yellow, default))]), 
   automaticArcher(init, OID_1, ['@'], down, 6, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_2, ['@'], down, 5, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_5, ['@'],   up, 2, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_6, ['@'],   up, 5, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_8, ['@'],   up, 8, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_9, ['@'],   up, 5, [ continuous, bullet_appearance('|', bold, red, default) ]).
norule(_).
norule(_,_,_,_,_).
