% dificultad: 9
% Puntuacion: 850

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic d_mwallStatus/2.
:- dynamic d_ghostControl/3.
:- dynamic d_laserBeamEntity/2.
:- dynamic d_laserStatus/3.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', ' ', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '#', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '#'],
['#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(28, 11).
num_dots(48).
pacman_start(X, Y):-
	X is random(8) + 7,
	Y is random(5) + 3.
initMap:- 
	retractall(d_mwallStatus(_,_)),
	retractall(d_ghostControl(_,_,_)),
	
	addSolidObject('#'), 

	% Puertas de entrada
	createGameEntity('|', object, 17, 2, inactive, norule, 
	                 data(puerta(1), solid, static, norule, 'Puerta num 1')), 
	createGameEntity('_', object, 20, 4, inactive, norule, 
	                 data(puerta(2), solid, static, norule, 'Puerta num 2')),  
	createGameEntity('_', object, 25, 6, inactive, norule, 
	                 data(puerta(3), solid, static, norule, 'Puerta num 3')), 
	createGameEntity('|', object, 17, 8, inactive, norule, 
	                 data(puerta(4), solid, static, norule, 'Puerta num 4')),  
	
	% Paredes móviles
	createGameEntity('#', object, 5, 3, active, mobileWall, 
	                 data(mobileWall, solid, static, norule, 'Pared Movil')), 
	createGameEntity('#', object, 5, 4, active, mobileWall, 
	                 data(mobileWall, solid, static, norule, 'Pared Movil')), 
	createGameEntity('#', object, 5, 5, active, mobileWall, 
	                 data(mobileWall, solid, static, norule, 'Pared Movil')), 
	createGameEntity('#', object, 5, 6, active, mobileWall, 
	                 data(mobileWall, solid, static, norule, 'Pared Movil')), 
	createGameEntity('#', object, 5, 7, active, mobileWall, 
	                 data(mobileWall, solid, static, norule, 'Pared Movil')), 

	% Pistola de 1 bala
	createGameEntity('L', object, 1, 9, inactive, norule, 
	                 data(oneShotGun, not_solid, not_static, gunShot, 'Pistola con una unica bala (cargada)')), 

	% Laser 
	createGameEntity('v', mortal, 6, 1, active, mobileLaser, 0), 

	% Fantasmas
	createGameEntity('F', mortal, 6, 2, active, spiderGhost, 0), 
	createGameEntity('F', mortal, 22, 2, active, spiderGhost, 0), 
	createGameEntity('F', mortal, 22, 8, active, spiderGhost, 0), 
	createGameEntity('F', mortal, 16, 9, active, spiderGhost, 0),

	% Llaves de las puertas
	createKeys(4).

createKeys(0):-!.
createKeys(N):-
	repeat,
	 X is random(4) + 1,
	 Y is random(9) + 1,
	 not(entityLocation(_, X, Y, _)),
	createGameEntity('+', object, X, Y, inactive, norule, 
	                 data(doorKey, not_solid, not_static, keyUseRule(N), 'Llave de una de las puertas')),  
	N1 is N-1,
	createKeys(N1).

% Regla de uso de las llaves
keyUseRule(NKey, ObjID, _, X, Y, _):-
	entityLocation(DoorID, X, Y, _),
	(objectName(DoorID, puerta(NPuerta))
	 -> 
	  (NPuerta = NKey
	   -> 
	    changeEntityAppearance(DoorID, '', _),
	    makeObjectNotSolid(DoorID),
	    destroyGameEntity(ObjID),
            msgWindowWriteln('La llave hace clic y la puerta se abre!')
	   ;  
	   msgWindowWriteln('Esta llave no parece entrar en la cerradura')
	  )
	 ;
	 msgWindowWriteln('No se como usar la llave ahi!')
	), !.
keyUseRule(_, _, _, _, _, _):-
	msgWindowWriteln('No puedo usar la llave ahi!').
	
norule(_).

% Control de los fantasmas
spiderdir(left).
spiderdir(right).
spiderdir(down).
spiderdir(up).
spidervalid(' ').
spidervalid('@').
spidervalid('.').
spiderGhost(_):-
	random(100) > 95, !.
spiderGhost(EID):-
	not(d_ghostControl(EID, _, _)),
	abolish(d_options/1), assert(d_options([])),
	forall((spiderdir(Dir), see(EID, normal, Dir, V), spidervalid(V)),
	       (retract(d_options(L)), append(L, [Dir], NL), assert(d_options(NL)))
	      ),
        d_options(L), length(L, Max), Max > 0, 
	D1 is random(Max), D2 is random(Max),
	nth0(D1, L, DMove), nth0(D2, L, DSee),
	assert(d_ghostControl(EID, DMove, DSee)), !.
spiderGhost(EID):-
	d_ghostControl(EID, DMove, _),
	see(EID, normal, DMove, V),
	not(spidervalid(V)),
	retract(d_ghostControl(EID, _, _)), !.
spiderGhost(EID):-
	d_ghostControl(EID, DMove, DSee),
	DMove \= DSee,
	see(EID, list, DSee, SEELIST),
	member('@', SEELIST),
	retract(d_ghostControl(EID, _, _)),
	assert(d_ghostControl(EID, DSee, DSee)),
	assert(d_doAction(EID, move(DMove))), !.
spiderGhost(EID):-
	d_ghostControl(EID, DMove, _),
	assert(d_doAction(EID, move(DMove))).

% Disparo de pistola de una bala
whatdestroysgun('F').
whatdestroysgun('v').
gunShot(ObjID,EID,X,Y,DIR):-
	msgWindowWrite('PAM! '),
	see(EID, list, DIR, SEELIST),
	whatdestroysgun(DEST),
	nth0(Pos, SEELIST, DEST),
	(DIR=up, Y1 is Y - Pos, X1 = X;
	 DIR=down, Y1 is Y + Pos, X1 = X;
	 DIR=left, X1 is X - Pos, Y1 = Y;
	 DIR=right, X1 is X + Pos, Y1 = Y),
	entityLocation(EID2, X1, Y1, DEST),
	destroyGameEntity(EID2),
	destroyGameEntity(ObjID),
	msgWindowWriteln('Acabas de matar un enemigo! :o'), !.
gunShot(ObjID,_,_,_,_):-
	destroyGameEntity(ObjID),
	msgWindowWriteln('Tu bala se pierde en la nada :/'), !.


% Control de paredes móbiles
wmove(left, right, 4).
wmove(right, left, 6).
mobileWall(EID):-
	not(d_mwallStatus(EID, _)),
	(random(2)<1 -> St=left ; St=right),
	assert(d_mwallStatus(EID, St)), !.
mobileWall(EID):-
	entityLocation(EID, X, _, _),
	d_mwallStatus(EID, DIR),
	wmove(DIR, NDIR, Max),
	(Max \= X,
	 assert(d_doAction(EID, move(DIR)))
	 ;
	 retract(d_mwallStatus(EID, _)),
	 assert(d_mwallStatus(EID, NDIR))
	), !.

% Control del laser
% Láseres móviles
lasermoves([right,right,right,left,left,right,right,right,right,left,left,right,right,right,right,right,left,left,right,right,right,right,left,left,left,left,left,left,left,left,left,left]).
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
	createLaserBeam(EID, X, Y1, 8),
	assert(d_laserStatus(EID, fired, N)), !.
mobileLaser(EID):-
	retract(d_laserStatus(EID, fired, N)),
	destroyLaserBeam(EID),
	assert(d_laserStatus(EID, seek, N)), !.




