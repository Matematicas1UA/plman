%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa3-4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#'],
['#', '.', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '.', '#', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '#', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '.', '#', '#', '#', '#', '#'],
[' ', ' ', ' ', ' ', '#', '.', '#', '#', '#', '#', '.', '.', '.', '#', '#', '#', '#', '.', '#', ' ', ' ', ' ', ' '],
['#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '#', '#', '#', '#'],
[' ', '.', '.', '.', '.', '.', '#', '.', '#', '#', '#', ' ', '#', '#', '#', '.', '#', '.', '.', '.', '.', '.', ' '],
['#', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', ' ', '#', '#', '#', '.', '#', '.', '#', '#', '#', '#', '#'],
[' ', ' ', ' ', ' ', '#', '.', '.', '.', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', ' ', ' ', ' ', ' '],
['#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '#', '.', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '#', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '#', '.', '#', '#', '#', '#', '.', '#', '.', '#', '#', '#', '#', '.', '#', '.', '.', '.', '#'],
['#', '#', '#', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '#', '.', '#', '#', '#', '#', '#', '#', '#', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', '.', '#', '#', '#', '#', '#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#', '#', '#', '#', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(23, 22).
num_dots(229).
pacman_start(11, 13).

:- dynamic d_totalGhosts/1.
:- dynamic d_ghostControl/2.

initMap:- 
	retractall(d_totalGhosts(_)),
	retractall(d_ghostControl(_,_)),
	
	addSolidObject('#'), 
	createGameEntity('', object, 0, 10, active, tele(21,10), 
	                 data(teletransportador, not_solid, not_static, norule, 'Teletransportador')), 
	createGameEntity('', object, 22, 10, active, tele(1, 10), 
	                 data(teletransportador, not_solid, not_static, norule, 'Teletransportador')), 
	createGameEntity('', mortal, 11, 11, active, respawnGhost, 0).
norule(_).


% teletransporte
tele(X, Y, PortalID):-
	entityLocation(PortalID, PortalX, PortalY, _),
	entityLocation(EID, PortalX, PortalY, _), EID \= PortalID,
	getDMap(Map),
	moveEntity(EID, Map, X, Y), !.
tele(_, _, _).

% Creador automático de fantasmas
maxGhosts(6).
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

