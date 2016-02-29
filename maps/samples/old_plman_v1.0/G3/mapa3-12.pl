%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa3-12.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', ' ', '#', '#', '#', '#', '.', '#', '#', ' ', '#', '#', '.', '#', '#', '#', '#', ' ', '#', '.', '#'],
['#', '.', '#', '.', '.', '.', '.', '#', '.', '#', '#', ' ', '#', '#', '.', '#', '.', '.', '.', '.', '#', '.', '#'],
['#', '.', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '.', '#', ' ', '#', '.', '#', '#', '#', '#', '#', '.', '#', ' ', '#', '.', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '.', '#', '#', '#', '#', '.', '#', '#', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', ' ', 'R', '#', '.', '#', ' ', 'I', ' ', '#', '.', '#', 'P', ' ', '#', '.', '#', '.', '#'],
['#', '.', '#', '.', '#', '#', '#', '#', '.', '#', '#', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#', '.', '#'],
['#', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(23, 17).
num_dots(163).
pacman_start(11, 15).

:- dynamic d_totalPills/1.
:- dynamic d_totalGhosts/1.
:- dynamic d_ghostControl/2.
:- dynamic d_magicStatus/1.

initMap:- 
	retractall(d_totalPills(_)),
	retractall(d_magicStatus(_)),
	
	addSolidObject('#'), 
	addSolidObject('R'), 
	addSolidObject('I'), 
	addSolidObject('P'), 

	createGameEntity('', object, 11, 7, active, respawnGhost, 
	                 data(magicStatus, not_solid, static, norule, 'Respawn de ghosts')),

	createGameEntity('', object, 0, 0, active, magicStatusControl, 
	                 data(magicStatus, not_solid, static, norule, 'Control del estado magico')),
	createGameEntity('', object, 0, 0, active, respawnPills, 
	                 data(respawnPills, not_solid, static, norule, 'Respawn de pildoras')).
norule(_).

availableRespawnPlaces(9).
respawnPlace(0,	11,  1).
respawnPlace(1,  6,  3).
respawnPlace(2,	16,  3).
respawnPlace(3,  3,  6).
respawnPlace(4, 19,  6).
respawnPlace(5,  6, 10).
respawnPlace(6,	16, 10).
respawnPlace(7,  5, 15).
respawnPlace(8,	17, 15).
maxPills(3).
respawnPills(_):-
	not(d_totalPills(_)),
	assert(d_totalPills(0)), !.
respawnPills(_):-
	d_totalPills(TW), maxPills(MW), 
	TW < MW, 
	availableRespawnPlaces(Aresp),
	Place is random(Aresp),
	respawnPlace(Place, X, Y),
	not(entityLocation(_, X, Y, _)),
	createGameEntity('o', object, X, Y, active, pillControl, data(magicpill, not_solid, static, norule, 'Pildora magica')), 
        retract(d_totalPills(TW)),
	TW1 is TW + 1,
	assert(d_totalPills(TW1)), !.
respawnPills(_).

magicStatusDuration(20).
pillControl(EID):-
	entityLocation(EID, X, Y, _),
	entityType(PacID, pacman),
	entityLocation(PacID, X, Y, _),
	not(d_magicStatus(_)),
	destroyGameEntity(EID),
	changeEntityAppearance(PacID, '€', _),
	magicStatusDuration(D),
	assert(d_magicStatus(D)),
        retract(d_totalPills(TW)),
	TW1 is TW - 1,
	assert(d_totalPills(TW1)), !.
pillControl(_).

destroydir(1, none).
destroydir(1, up). destroydir(2, up).
destroydir(1, down). destroydir(2, down).
destroydir(1, left). destroydir(2, left).
destroydir(1, right). destroydir(2, right).
destroyCollindantGhosts(X, Y):-
	forall(destroydir(Add, Dir),
	       (
		((Dir=none, X1=X, Y1=Y);
		 (Dir=up, X1=X, Y1 is Y-Add);
		 (Dir=down, X1=X, Y1 is Y+Add);
		 (Dir=left, X1 is X-Add, Y1=Y);
		 (Dir=right, X1 is X+Add, Y1=Y)
		),
		(   
		 forall(entityLocation(EID, X1, Y1, 'F'), 
		       (destroyGameEntity(EID), msgWindowWriteln('JAJA! Muere sucio!')))
		 ; true
		)
		; true
	       )
	      ), !.

magicStatusControl(_):-
	retract(d_magicStatus(0)), 
	entityType(PacID, pacman),
	changeEntityAppearance(PacID, '@', _), !.
magicStatusControl(_):-
	retract(d_magicStatus(Ticks)),
	Ticks1 is Ticks	- 1,
	entityType(PacID, pacman),
	entityLocation(PacID, X, Y, _),
	destroyCollindantGhosts(X, Y),
	assert(d_magicStatus(Ticks1)), !.
magicStatusControl(_).

% Creador automático de fantasmas
maxGhosts(20).
respawnGhost(_):-
	not(d_totalGhosts(_)),
	assert(d_totalGhosts(0)), !.
respawnGhost(EID):-
	maxGhosts(MG), d_totalGhosts(TG),
	TG < MG, random(100) > 85,
	entityLocation(EID, X, Y, _),
	createGameEntity('F', mortal, X, Y, active, spiderGhost, 0), 
	retract(d_totalGhosts(_)), TG1 is TG+1,
	assert(d_totalGhosts(TG1)), !.
respawnGhost(_).

% Control de los fantasmas
spiderdir(0, left, up, down).
spiderdir(1, right, down, up).
spiderdir(2, down, left, right).
spiderdir(3, up, right, left).
spidervalid(' ').
spidervalid('@').
spidervalid('.').
spiderGhost(_):-
	random(100) > 95, !.
spiderGhost(EID):-
	not(d_ghostControl(EID, _)),
	spiderdir(_,_,_,_), % 4 intentos
	X is random(4),
	spiderdir(X, Dir, _, _),
	see(EID, normal, Dir, V),
	spidervalid(V),
	assert(d_ghostControl(EID, Dir)), !.
spiderGhost(EID):-
	d_ghostControl(EID, DMove),
	see(EID, normal, DMove, V),
	not(spidervalid(V)),
	retract(d_ghostControl(EID, _)), !.
spiderGhost(EID):-
	d_ghostControl(EID, DMove),
	(   
	 (spiderdir(_, DMove, D, _); spiderdir(_, DMove, _, D)),
         see(EID, normal, D, V),
	 spidervalid(V), random(100) > 85,
	 retract(d_ghostControl(EID, _)),
	 assert(d_ghostControl(EID, D))
	 ;
	 assert(d_doAction(EID, move(DMove)))
	), !.
