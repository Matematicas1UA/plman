%%%
%%% En este mapa pl-man aparece en posición aleatoria en cualquier parte
%%% de la zona central del mapa (columnas 2-14, filas 2-6). Por su parte, 
%%% el único fantasma que hay en el mapa aparece en una columna aleatoria
%%% cualquiera (1-15) y o bien en la primera fila o bien en la última.
%%% Este peculiar fantasma se mueve en linea recta hasta llegar a una pared
%%% o hasta que vea a pl-man. Si llega a una pared, cambia aleatoriamente
%%% de dirección hacia otra en la que no vea paredes. Si ve a pl-man por
%%% el camino, puede cambiar de dirección para perseguirlo con un 75% 
%%% de probabilidad. Además, aleatoriamente, en 1 de sus 4 direcciones,
%%% si ve a pl-man, le lanza una estrella ninja que también puede
%%% acabar con pl-man.
%%%

map_format_version(1.0).
load_behaviour(combiner).
load_behaviour(spiderGhost).
load_behaviour(automaticArcher).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(17, 9).
num_dots(105).
pacman_start(X, Y) :- randomBetween(2,14,X), randomBetween(2,6,Y).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, 'F', mortal, rnd(1,15), rnd([1,7]), active, combiner([spiderGhost, automaticArcher]), 
		[appearance(attribs(normal, red, default))]),
	randomBetween(1,4,D), nth1(D, [up,down,left,right], DIR),
	spiderGhost(init, EID_0, [ '#' ], [ probFollowPacman(75), maxFollowTimes(1) ]),
	automaticArcher(init, EID_0, ['@'], DIR, 10, [ bullet_appearance('*', bold, red, default) ]).
norule(_).
norule(_,_,_,_,_).
