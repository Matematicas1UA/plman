%%%
%%% En este mapa aparecen 2 arqueros automáticos arriba y abajo, pero ambos
%%% en una columna aleatoria (de la 1 a la 15). Los arqueros se mueven de
%%% izquierda a derecha y disparan a frecuencias constantes. Sin embargo, 
%%% estas frecuencias de disparo son aleatorias de 4 y 7 movimientos entre
%%% cada 2 disparos. La posición inicial de pl-man también es aleatoria en
%%% el centro del mapa, pudiendo estar entre las columnas 2-14 y las 
%%% filas 3-5.
%%%

map_format_version(1.0).
load_behaviour(combiner).
load_behaviour(automaticArcher).
load_behaviour(enemyBasicMovement).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(17, 9).
num_dots(75).
pacman_start(X, Y) :- randomBetween(2,14,X), randomBetween(3,5,Y).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_0, 'v', object, rnd(1,15), 1, active, combiner([automaticArcher, enemyBasicMovement]), 
			[name(arquero1), solid(false), static(true), use_rule(norule),
			description('Arquero automático del rey'), appearance(attribs(bold, yellow, default))]), 
	createGameEntity(OID_1, '^', object, rnd(1,15), 7, active, combiner([automaticArcher, enemyBasicMovement]),
			[name(arquero2), solid(false), static(true), use_rule(norule),
			description('Arquero automático del rey'), appearance(attribs(bold, yellow, default))]),
	enemyBasicMovement(init, OID_0, right-left, ['#']),
	enemyBasicMovement(init, OID_1, left-right, ['#']),
	randomBetween(4,7,D1), randomBetween(4,7,D2),
   automaticArcher(init, OID_0, ['@'], down, D1, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_1, ['@'],   up, D2, [ continuous, bullet_appearance('!', bold, red, default) ]).
norule(_).
norule(_,_,_,_,_).

