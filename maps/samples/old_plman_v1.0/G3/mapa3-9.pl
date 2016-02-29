%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from fase0/mapa0-8.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 3
%Puntuaci�n: 150

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '#', 'v', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '.', '#', '^', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(18, 5).
num_dots(3).
pacman_start(6, 2).
initMap:- 
	retractall(d_direccion(_,_)),
	addSolidObject('#'), 
	addSolidObject('v'), 
	addSolidObject('^'),
	% pincho que baja y sube
	createGameEntity('*', object, 10, 1, active, patrullar, 
	                 data(pincho1, solid, static, norule, 'Pincho que baja y sube')), 
	% puerta para dinamitar
	createGameEntity('|', object, 2, 2, active, verCasillaDcha, 
	                 data(puerta, solid, static, norule, 'Puerta para dinamitar')), 
	% pincho que sube y baja
	createGameEntity('*', object, 13, 3, active, patrullar, 
	                 data(pincho2, solid, static, norule, 'Pincho que sube y baja')), 
	% dinamita para dejar en el sitio adecuado, entre los objetos v y ^
	createGameEntity('!', object, 16, 3, inactive, norule, 
	                 data(dinamita, solid, not_static, norule, 'Dinamita que explota cuando se situa entre calentadores ')).

% PREDICADOS DE MOVIMIENTO DE LOS PINCHOS

:- dynamic d_direccion/2.

direccion(up,down).
direccion(down,up).


% CASO INICIAL
patrullar(ID):- 
	not(d_direccion(ID,_)),
	direccion(DIR,_),
	assert(d_direccion(ID,DIR)), !.

% VEO PARED
patrullar(ID):- 
	d_direccion(ID,DIR),
	see(ID, normal, DIR, '#'),
	retract(d_direccion(ID,_)),
	direccion(DIR,NEXTDIR),
	assert(d_doAction(ID,move(NEXTDIR))),
	assert(d_direccion(ID,NEXTDIR)), !.

% NO VEO PARED
patrullar(ID):- 
	d_direccion(ID,DIR),
	assert(d_doAction(ID,move(DIR))), !.


% PREDICADOS DE CONTROL DE LA PUERTESICA
verCasillaDcha(ID):-
	see(ID, normal, right, '@'),
	entityType(PacID, pacman),
	d_object(PacID, _),
	changeEntityAppearance(PacID, '*', _),
	changeEntityAppearance(ID, '*', _),
	msgWindowWriteln('BOOOOOM!!!! La dinamita se ha calentado y te ha explotado en las manos!!!'),
	assert(d_gameEnd), !.
verCasillaDcha(ID):- 
	see(ID, normal, right, '!'),
	entityLocation(ID, X, Y, _),
	PosDcha is X+1,
	entityLocation(ID2, PosDcha, Y, _),
	destroyGameEntity(ID),
	destroyGameEntity(ID2),
	msgWindowWriteln('BOOOOOM !!! Ha explotado la dinamita!!! Puedes pasar (si quieres)'), !.
verCasillaDcha(_).

norule(_).


