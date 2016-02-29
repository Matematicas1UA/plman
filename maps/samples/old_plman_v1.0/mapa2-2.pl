% dificultad: 10
% Puntuacion: 1000
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-2.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic d_haveSeenPacman/1.
:- dynamic d_automaticDoor/1.
:- dynamic d_gasLauncher/1.
:- dynamic d_gasParticles/1.
:- dynamic d_gasLife/3.
:- dynamic d_ballsLaunched/1.
:- dynamic d_ballControl/3.
:- dynamic d_laserStatus/3.
:- dynamic d_laserBeamEntity/2.
:- dynamic d_gasdir/1.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', ' ', ' ', ' ', ' ', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', '#', '#', '#', ' ', '#', '#', '#', '#', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', '#', '.', ' ', ' ', ' ', ' ', '.', '#', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', '#', '.', ' ', ' ', ' ', ' ', '.', '#', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(31, 15).
num_dots(204).
pacman_start(X, Y):-
	X is random(4) + 13,
	Y is random(3) + 6.
initMap:- 
	retractall(d_haveSeenPacman(_)),
	retractall(d_automaticDoor(_)),
	retractall(d_gasParticles(_)),
	retractall(d_gasLife(_,_,_)),
	retractall(d_gasLauncher(_)),
	retractall(d_ballsLaunched(_)),
	retractall(d_ballControl(_,_,_)),
	retractall(d_laserStatus(_,_,_)),
	retractall(d_laserBeamEntity(_, _)),
	retractall(d_gasdir(_)),
	
	addSolidObject('#'), 
	
	% Pistolas de una única bala
	createGameEntity('L', object, 29, 13, inactive, norule, 
	                 data(oneShotGun, not_solid, not_static, gunShot, 'Pistola con cargador para una unica bala (cargada).')), 
	createGameEntity('L', object, 1, 13, inactive, norule, 
	                 data(oneShotGun, not_solid, not_static, gunShot, 'Pistola con cargador para una unica bala (cargada).')), 

	% Lanzador de bolas de pinchos
	createGameEntity('}', object, 7, 3, active, ballLauncher, 
	                 data(ballLauncher, solid, static, norule, 'Lanzador de bolas de pinchos destructoras')), 
	
	% Puertas de la zona central (se abren y cierran)
	createGameEntity('#', object, 14, 5, active, randomOpenDoor, 
	                 data(randomOpenDoor, solid, static, norule, 'Puerta que se abre y se cierra sola aleatoriamente')), 
	createGameEntity('#', object, 11, 7, active, randomOpenDoor, 
	                 data(randomOpenDoor, solid, static, norule, 'Puerta que se abre y se cierra sola aleatoriamente')), 
	createGameEntity('#', object, 18, 7, active, randomOpenDoor, 
	                 data(randomOpenDoor, solid, static, norule, 'Puerta que se abre y se cierra sola aleatoriamente')), 

	% Puertas que se cierran al pasar y activan la cámara de gas
	d_entityId(IDDoor1),
	assert(d_automaticDoor(IDDoor1)),
	createGameEntity('', object, 9, 9, active, autoCloseDoor, 
	                 data(autoCloseDoor, not_solid, static, norule, 'Puerta que se cierra automaticamente y activa la camara de gas')), 
	d_entityId(IDDoor2),
	assert(d_automaticDoor(IDDoor2)),
	createGameEntity('', object, 20, 9, active, autoCloseDoor, 
	                 data(autoCloseDoor, not_solid, static, norule, 'Puerta que se cierra automaticamente y activa la camara de gas')), 

	% Cartucho de dinamita
	createRandomDynamite,

	% Surtidor de Gas de la cámara de Gas
	d_entityId(GasID),
	assert(d_gasLauncher(GasID)),
	createGameEntity('', object, 14, 10, inactive, expandGas, 
	                 data(gasExpander, not_solid, static, norule, 'Surtidor que lanza gas por la cámara')), 
	
	% Laseres moviles
	createGameEntity('v', mortal, 1, 1, active, mobileLaser, 0), 
	createGameEntity('v', mortal, 25, 1, active, mobileLaser, 0),

	% Enemigos a destruir porque no se apartan del camino
	createGameEntity('E', mortal, 14, 1, active, staticGhost, 0), 
	createGameEntity('E', mortal, 14, 13, active, staticGhost, 0),
	createRandomKey. 

norule(_).

% Posición aleatoria dinamita
dynamitePos(0, 8).
dynamitePos(1, 19).
createRandomDynamite:-
	Add is random(3),
	Pos is random(2),
	dynamitePos(Pos, XIni),
	X is XIni + Add,
	createGameEntity('¡', object, X, 11, inactive, norule, 
	                 data(dynamite, not_solid, not_static, dynamiteUse, 'Cartucho de dinamita')).

% Llave en Posición Aleatoria
createRandomKey:-
	getDMap(M),
	repeat,
	X is random(14) + 8,
	Y is random(5) + 4,
        getCellContent(X, Y, M, ' '),
	not(entityLocation(_, X, Y, _)),
	createGameEntity('+', object, X, Y, inactive, norule, 
			 data(gasCamKey, not_solid, not_static, keyUseRule, 'Llave de la camara de gas')).

% Puertecitas que se abren y cierran aleatoriamente en el centro
randomOpenDoor(EID):-
	solidEntity(EID),
        random(100) > 90,
	changeEntityAppearance(EID, '', _),
	makeObjectNotSolid(EID), !.
randomOpenDoor(EID):-
	not(solidEntity(EID)),
	entityLocation(EID, X, Y, _),
	forall(entityLocation(EID2, X, Y, _), EID2=EID),
	random(100) > 90,
	changeEntityAppearance(EID, '#', _),
	makeObjectSolid(EID), !.
randomOpenDoor(_).

% Puertas que se autocierran al pasar por delante
closeAutomaticDoors:-
	forall(d_automaticDoor(EID), (makeObjectSolid(EID),changeEntityAppearance(EID, '_', _))),
	msgWindowWriteln('Camara de gas sellada. Comenzando llenado.'),
	d_gasLauncher(GasID),
	activateEntity(GasID).
openAutomaticDoors:-
	forall(d_automaticDoor(EID), (makeObjectNotSolid(EID),changeEntityAppearance(EID, '', _))),
	msgWindowWriteln('Camara de gas abierta. '),
	d_gasLauncher(GasID),
	deactivateEntity(GasID).
	
autoCloseDoor(EID):-
	not(solidEntity(EID)),
	entityType(PacID, pacman),
	entityLocation(EID, X, Y, _),
	(
	 (   
	  entityLocation(PacID, X, Y, _),
	  not(d_haveSeenPacman(EID)),
	  assert(d_haveSeenPacman(EID))
	 );(
	  entityLocation(PacID, PacX, PacY, _),
	  (PacX \= X ; PacY \= Y),
	  d_haveSeenPacman(EID),
	  retractall(d_haveSeenPacman(EID)),
	  closeAutomaticDoors
	 )
	), !.
autoCloseDoor(_).

% La llave de la cámara de gas
keyUseRule(ObjId, _, X, Y, _):-
	entityLocation(EID, X, Y, _),
	d_automaticDoor(EID),
	((   
	  solidEntity(EID),
	  destroyGameEntity(ObjId),
	  createRandomKey,
	  openAutomaticDoors
	 );(
	  not(solidEntity(EID)),
	  msgWindowWriteln('La puerta ya esta abierta. No es necesario usar la llave.')
	)), !.
keyUseRule(_, _, _, _, _):-
	msgWindowWriteln('La llave no tienen ningun uso ahi').
	
% La dinamita no se puede usar, sólo dejar en su sitio apropiado
dynamiteUse(_,_,_,_,_):-
	msgWindowWriteln('No puedes usar este objeto directamente. Es demasiado peligroso.').

% Surtidor de Gas expandiendo partículas
expandGas(_):-
	not(d_gasParticles(_)),
	assert(d_gasParticles(0)), !.
expandGas(EID):-
	d_gasParticles(NGP), NGP < 25,
	retract(d_gasParticles(NGP)), 
	NGP1 is NGP + 1,
	assert(d_gasParticles(NGP1)),
	entityLocation(EID, X, Y, _),
	X1 is X + random(3) - 1,
	Y1 is Y + random(2),
	V is random(4),
	nth0(V, [up, down, left, right], DIR),
	d_entityId(GasId),
	createGameEntity('G', mortal, X1, Y1, active, gasMove, 0),
	assert(d_gasLife(GasId, 25, DIR)), !.
expandGas(_).

% Control de una partícula de gas
gasMove(_):-
	not(d_gasdir(_)),
	assert(d_gasdir(up)),
	assert(d_gasdir(down)),
	assert(d_gasdir(left)),
	assert(d_gasdir(right)), !.
gasMove(EID):-
	retract(d_gasLife(EID, 0, _)),
	destroyGameEntity(EID), 
	retract(d_gasParticles(NGP)),
	NGP1 is NGP - 1,
	assert(d_gasParticles(NGP1)), !.
gasMove(EID):- 
	d_gasLife(EID, Life, DIR),
	see(EID, normal, DIR, V), V\='#', V\='G',
	retract(d_gasLife(EID, _, _)),
	Life1 is Life - 1,
	assert(d_gasLife(EID, Life1, DIR)),
	assert(d_doAction(EID, move(DIR))), !.
gasMove(EID):-
	d_gasdir(MiDir),
	see(EID, normal, MiDir, V), V\='#', V\='G',
	retract(d_gasdir(MiDir)),
	assertz(d_gasdir(MiDir)),
	retract(d_gasLife(EID, Life, _)),
	Life1 is Life-1,
	assert(d_doAction(EID, move(MiDir))), 
	assert(d_gasLife(EID, Life1, MiDir)), !.
gasMove(_).


% Lanzador de bolas locas de pinchos
launchBall(_):-
	not(d_ballsLaunched(_)),
	assert(d_ballsLaunched(0)), !.
launchBall(EID):-
	d_ballsLaunched(NBalls), NBalls < 20,
	NBalls1 is NBalls + 1,
	entityLocation(EID, X, Y, _),
	X1 is X + 1,
	createGameEntity('*', mortal, X1, Y, active, ballMove, 0),
	retract(d_ballsLaunched(NBalls)),
	assert(d_ballsLaunched(NBalls1)), !.

ballLauncher(EID):-
	see(EID, normal, right, '¡'),
	entityLocation(EID, X, Y, _),
	X1 is X + 1,
	entityLocation(EID2, X1, Y, '¡'),
	destroyGameEntity(EID),
	destroyGameEntity(EID2), 
	msgWindowWrite('Una bola ha chocado con la dinamita y la ha hecho explotar!'), !.
ballLauncher(EID):-
	see(EID, list, right, SEELIST),
	nth0(Pos, SEELIST, '@'), Pos > 1,
	launchBall(EID), !.
ballLauncher(_).

% Control de una bola de pinchos
nummove(1, right, down).
nummove(-1, left, up).
ballMove(EID):-
	not(d_ballControl(EID,_,_)),
	assert(d_ballControl(EID,1,1)).
ballMove(EID):-
	d_ballControl(EID, AddX, AddY),
	entityLocation(EID, X, Y, _),
	X1 is X + AddX, Y1 is Y + AddY,
	getDMap(Map),
	(
	 collision(X1, Y1, Map),
	 (0 is random(2) -> VX=1 ; VX= -1),
	 (0 is random(2) -> VY=1 ; VY= -1),
	 retract(d_ballControl(EID, _, _)),
	 assert(d_ballControl(EID, VX, VY))
	 ;  
	 nummove(AddX, MX,  _),
	 nummove(AddY,  _, MY),
	 assert(d_doAction(EID, move(MX))),
	 assert(d_doAction(EID, move(MY)))
	), !.

% Láseres móviles
lasermoves([right,right,right,left,left,right,right,right,left,left,left,left]).
createLaserBeam(_, _, _, 0):- !.
createLaserBeam(EID, X, Y, N):-
	d_entityId(BeamID),
	assert(d_laserBeamEntity(EID, BeamID)),
	createGameEntity('|', mortal, X, Y, active, norule, 0),
	Y1 is Y + 1, N1 is N - 1,
	createLaserBeam(EID, X, Y1, N1).
destroyLaserBeam(EID):-
	forall(d_laserBeamEntity(EID, BeamID), destroyGameEntity(BeamID)), !.

mobileLaser(EID):-
	not(d_laserStatus(EID,_,_)),
	assert(d_laserStatus(EID, seek, 0)), !.
mobileLaser(EID):-
	d_laserStatus(EID, seek, N),
	see(EID, list, down, SEELIST),
	member('@', SEELIST),
	retract(d_laserStatus(EID, _, _)),
	assert(d_laserStatus(EID, found, N)), !.
mobileLaser(EID):-
	retract(d_laserStatus(EID, seek, N)),
	lasermoves(Moves),
	length(Moves, Max), 
	(N < Max -> THIS=N ; THIS=0),
	nth0(THIS, Moves, DIR),
	assert(d_doAction(EID, move(DIR))),
	NEXT is THIS+1,
	assert(d_laserStatus(EID, seek, NEXT)), !.
mobileLaser(EID):-
	retract(d_laserStatus(EID, found, N)),
	assert(d_laserStatus(EID, fire, N)), !.
mobileLaser(EID):-
	retract(d_laserStatus(EID, fire, N)),
	entityLocation(EID, X, Y, _),
	Y1 is Y + 1,
	createLaserBeam(EID, X, Y1, 12),
	assert(d_laserStatus(EID, fired, N)), !.
mobileLaser(EID):-
	retract(d_laserStatus(EID, fired, N)),
	destroyLaserBeam(EID),
	assert(d_laserStatus(EID, seek, N)), !.
	
% Disparo de pistola de una bala
gunShot(ObjID,EID,X,Y,DIR):-
	msgWindowWrite('PAM! '),
	see(EID, list, DIR, SEELIST),
	nth0(Pos, SEELIST, 'E'),
	(DIR=up, Y1 is Y - Pos, X1 = X;
	 DIR=down, Y1 is Y + Pos, X1 = X;
	 DIR=left, X1 is X - Pos, Y1 = Y;
	 DIR=right, X1 is X + Pos, Y1 = Y),
	entityLocation(EID2, X1, Y1, 'E'),
	destroyGameEntity(EID2),
	destroyGameEntity(ObjID),
	msgWindowWriteln('Acabas de matar un enemigo! :o'), !.
gunShot(ObjID,_,_,_,_):-
	destroyGameEntity(ObjID),
	msgWindowWriteln('Tu bala se pierde en la nada :/'), !.

% El fantasma estático no hace nada
staticGhost(_).

