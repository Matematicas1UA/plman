% Dificultad: 6
% Puntuacion: 250

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-1.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic d_mwallList/1.
:- dynamic d_mwallsActivated/0.
:- dynamic d_bouncingBallDir/3.
:- dynamic d_spiderGhostDir/2.
:- dynamic d_squareGhostNumMove/2.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', '#', '.', '.', '.', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', ' ', ' ', '#', ' ', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', ' ', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', '#', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', ' ', '#', ' ', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', '#', '#', '#', '#', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '.', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '#', ' ', ' ', ' ', '#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', ' ', '.', '.', '#'],
['#', '.', '.', '#', ' ', ' ', ' ', '#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '#', ' ', ' ', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(28, 11).
num_dots(118).
pacman_start(1, 1).
initMap:- 
	retractall(d_mwallList(_)),
	retractall(d_spiderGhostDir(_, _)),
	retractall(d_mwallsActivated),
	retractall(d_bouncingBallDir(_,_,_)),
	retractall(d_squareGhostNumMove(_,_)),
	
	addSolidObject('#'), 
        % Pared que se mueve hacia el jugador
	addNextIdToMobileWallList,
	createGameEntity('#', object, 20, 2, inactive, mobileWall, 
	                 data(mwall, solid, static, norule, 'MobileWall1')), 
	addNextIdToMobileWallList,
	createGameEntity('#', object, 20, 3, inactive, mobileWall, 
	                 data(mwall, solid, static, norule, 'MobileWall2')), 
	addNextIdToMobileWallList,
	createGameEntity('#', object, 20, 4, inactive, mobileWall, 
	                 data(mwall, solid, static, norule, 'MobileWall3')), 
	addNextIdToMobileWallList,
	createGameEntity('#', object, 20, 5, inactive, mobileWall, 
	                 data(mwall, solid, static, norule, 'MobileWall4')), 
	% Llave
	createGameEntity('+', object, 26, 5, inactive, norule, 
	                 data(magicKey, not_solid, not_static, keyUseRule, 'Llave magica')), 

        % Lanzadores de ondas
	createGameEntity('>', object, 8, 7, active, waveShooter, 
	                 data(waveShooter, solid, static, norule, 'Lanzador de ondas de choque')), 
	createGameEntity('>', object, 8, 8, active, waveShooter, 
	                 data(waveShooter, solid, static, norule, 'Lanzador de ondas de choque')), 

        % Puerta
	createGameEntity('|', object, 20, 9, inactive, norule, 
	                 data(magicDoor, solid, static, norule, 'Puerta magica')), 

	% Pared Mortal
	createGameEntity('#', mortal, 26, 1, active, mortalWall, 0), 

	% Cuadrado
	createGameEntity('F', mortal, 4, 2, active, squareGhost, 0), 

	% Pinchos Rebotones
	createGameEntity('*', mortal, 14, 2, active, bouncingBall, 0), 
	createGameEntity('*', mortal, 18, 3, active, bouncingBall, 0), 

	% Perseguidor con aleatoriedad
	createGameEntity('F', mortal, 13, 5, active, followerGhost, 0), 

	% Seguidor de la pared con cambios de sentido aleatorios
	createGameEntity('F', mortal, 24, 7, active, spiderGhost, 0).
norule(_).

% Usando la llave
keyUseRule(_, _, X, Y, _):-
	entityLocation(EID2, X, Y, '|'),
	changeEntityAppearance(EID2, ']', _),
	makeObjectNotSolid(EID2),
	msgWindowWriteln('Usas la llave y...abres la magica!'), !.
keyUseRule(_, _, _, _, _):-
	msgWindowWriteln('No parece que puedas usar la llave ahi!').

% Disparador de ondas de choque
waveShooter(EID):-
	see(EID, list, right, SEELIST),
	member(')', SEELIST), !.
waveShooter(EID):-
	P is random(100),
	P > 15,
	entityLocation(EID, X, Y, _),
	X1 is X + 1,
	createGameEntity(')', mortal, X1, Y, active, waveShot, 0), !.
waveShooter(_).

waveShot(EID):-
	see(EID, normal, right, '#'),
	destroyGameEntity(EID), !.
waveShot(EID):-
	assert(d_doAction(EID, move(right))).
	

% Fantasma que hace un cuadrado (o cualquier otro movimiento predeterminado genérico)
squareGhostMoves([up, right, right, down, down, down, left, left, up, up]).
squareGhost(EID):-
	not(d_squareGhostNumMove(EID, _)),
	assert(d_squareGhostNumMove(EID, 0)), !.
squareGhost(EID):-
	retract(d_squareGhostNumMove(EID, N)),
	squareGhostMoves(LMOVES),
	length(LMOVES, TotMoves),
	(N >= TotMoves -> THIS = 0 ; THIS = N),
	nth0(THIS, LMOVES, DIR),
	NEXT is THIS + 1,
	assert(d_squareGhostNumMove(EID, NEXT)),
	assert(d_doAction(EID, move(DIR))).

% Fantasma que va de una pared a otra en plan araña
spiderdir(0, left).
spiderdir(1, right).
spiderdir(2, down).
spiderdir(3, up).
spiderGhost(EID):-
	not(d_spiderGhostDir(EID, _)),
	D is random(4),
	spiderdir(D, DIR),
	assert(d_spiderGhostDir(EID, DIR)), !.
spiderGhost(EID):-
	d_spiderGhostDir(EID, DIR),
	see(EID, normal, DIR, S),
	(S = '#'; S = '|'; S = ']'),
	retractall(d_spiderGhostDir(EID, _)), !.
spiderGhost(EID):-
	d_spiderGhostDir(EID, DIR),
	assert(d_doAction(EID, move(DIR))).

% Bolas de pinchos que rebotan
bouncingBall(EID):-
	not(d_bouncingBallDir(EID, _, _)),
	assert(d_bouncingBallDir(EID, left, right)), !.
bouncingBall(EID):-
	d_bouncingBallDir(EID, DIR, NEXTD),
	see(EID, normal, DIR, '#'),
	retractall(d_bouncingBallDir(EID, _, _)),
	assert(d_bouncingBallDir(EID, NEXTD, DIR)), !.
bouncingBall(EID):-
	d_bouncingBallDir(EID, DIR, _),
	assert(d_doAction(EID, move(DIR))).


% Fantasma que te persigue... casi siempre :)
followersee(left, right).
followersee(right, left).

followerGhost(EID):- 
	followersee(DIR, OTHER),
	see(EID, list, DIR, SEELIST),
	member('@', SEELIST),
	P is random(100), 
	(P > 30 -> FINALDIR = DIR ; FINALDIR = OTHER),
	assert(d_doAction(EID, move(FINALDIR))).

% Grupo de paredes que te encierran
mortalWall(EID):-
	not(d_mwallsActivated), !, 
	see(EID, list, down, SEELIST),
	%nth0(2, SEELIST, '@'),
	member('@', SEELIST),
	activateMobileWalls,
	assert(d_mwallsActivated), !.
mortalWall(EID):-
	d_mwallsActivated,
	see(EID, normal, down, '#'),
	deactivateEntity(EID), !.
mortalWall(EID):-
	d_mwallsActivated,
	assert(d_doAction(EID, move(down))).

mobileWall(EID):-
	see(EID, list, right, [_, '#']),
	deactivateEntity(EID), !.
mobileWall(EID):-
	assert(d_doAction(EID, move(right))).

addNextIdToMobileWallList:-
	(d_mwallList(LIST), !; LIST = []),
	d_entityId(EID),
	append([EID], LIST, NLIST),
	retractall(d_mwallList(_)),
	assert(d_mwallList(NLIST)).
activateMobileWalls:-
	d_mwallList(WLIST),
	maplist(activateEntity,WLIST).






