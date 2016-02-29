% dificultad: 8
% Puntuacion: 600
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-5.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic d_totalWeapons/1.
:- dynamic d_totalGhosts/1.
:- dynamic d_weaponAmmo/2.
:- dynamic d_ghostControl/3.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'K', 'I', 'L', 'L', '\'', 'e', 'M', ' ', 'A', 'L', 'L', '!', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', 'T', 'h', 'e', 'y', '\'', 'v', 'e', ' ', 's', 't', 'o', 'l', 'e', 'n', ' ', 'y', 'o', 'u', 'r', ' ', 'd', 'o', 't', 's', '!', ' ', '#'],
['#', '.', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(29, 14).
num_dots(20).
pacman_start(13, 3).
initMap:- 
	retractall(d_totalGhosts(_)),
	retractall(d_totalWeapons(_)),
	retractall(d_weaponAmmo(_,_)),
	
	addSolidObject('#'), addSolidObject('K'), 
	addSolidObject('I'), addSolidObject('L'), 
	addSolidObject('\''),addSolidObject('e'), 
	addSolidObject('M'), addSolidObject('A'), 
	addSolidObject('!'), addSolidObject('T'), 
	addSolidObject('h'), addSolidObject('y'), 
	addSolidObject('v'), addSolidObject('s'), 
	addSolidObject('t'), addSolidObject('o'), 
	addSolidObject('l'), addSolidObject('n'), 
	addSolidObject('u'), addSolidObject('r'), 
	addSolidObject('d'), 
	
	% Respawns para fantasmas
	createGameEntity('', object, 0, 0, active, respawnGhost, 
			data(respawnGhos, solid, static, norule,'Respawneador de fantamas')),
	% Respawns para las armas
	createGameEntity('', object, 0, 0, active, respawnWeapon, 
			data(weaponRespawn, solid, static, norule, 'Respawneador de armas')). 
% Respawn de las armas
availableRespawnPlaces(6).
respawnPlace(0,  3,  1).
respawnPlace(1, 25,  1).
respawnPlace(2, 6,  6).
respawnPlace(3, 13,  6).
respawnPlace(4, 20,  6).
respawnPlace(5, 13, 11).
availableWeapons(1).
weapon(0, gun, gunShot, 3, 'L', 'Revolver con 3 balas (cargada)').
maxWeapons(2).
respawnWeapon(_):-
	not(d_totalWeapons(_)),
	assert(d_totalWeapons(0)), !.
respawnWeapon(_):-
	d_totalWeapons(TW), maxWeapons(MW), 
	TW < MW, random(100) > 95,
	availableWeapons(AvW),
	availableRespawnPlaces(Aresp),
	NumW is random(AvW),
	Place is random(Aresp),
	weapon(NumW, WName, WUse, WAmmo, WAp, WDesc),
	respawnPlace(Place, X, Y),
	not(entityLocation(_, X, Y, _)),
	d_entityId(WeapId),
	createGameEntity(WAp, object, X, Y, inactive, norule, data(WName, solid, not_static, WUse, WDesc)), 
	assert(d_weaponAmmo(WeapId, WAmmo)),
        retract(d_totalWeapons(TW)),
	TW1 is TW + 1,
	assert(d_totalWeapons(TW1)), !.
respawnWeapon(_).
	
% Creador automático de fantasmas
maxGhosts(10).
respawnGhost(_):-
	not(d_totalGhosts(_)),
	assert(d_totalGhosts(0)), !.
respawnGhost(_):-
	maxGhosts(MG), d_totalGhosts(TG),
	TG < MG, random(100) > 91,
	X is random(27) + 1,
	Y is random(11) + 1,
	getDMap(Map),
	not(collision(X, Y, Map)),
	not(entityLocation(_, X, Y, _)),
	createGameEntity('F', mortal, X, Y, active, spiderGhost, 0), !.
respawnGhost(_).

% Regla para disparar
whatdestroysgun('F').
checkIfEnemyDestroyed(EID, X, Y, DIR):-
	see(EID, list, DIR, SEELIST),
	whatdestroysgun(DEST),
	nth0(Pos, SEELIST, DEST),
	(DIR=up, Y1 is Y - Pos, X1 = X;
	 DIR=down, Y1 is Y + Pos, X1 = X;
	 DIR=left, X1 is X - Pos, Y1 = Y;
	 DIR=right, X1 is X + Pos, Y1 = Y),
	entityLocation(GhostID, X1, Y1, DEST),
	destroyGameEntity(GhostID),
	setDCellContent(X1, Y1, '.'),
	retract(d_totalGhosts(TG)),
	TG1 is TG - 1, 
	assert(d_totalGhosts(TG1)),
	msgWindowWriteln('Buen disparo! Has alcanzado a un enemigo! :o'), !.
checkIfEnemyDestroyed(_, _, _, _):-
	msgWindowWriteln('Tus disparos se pierden en la nada :/').
gunShot(ObjID,EID,X,Y,DIR):-
	msgWindowWrite('PAM! '),
	retract(d_weaponAmmo(ObjID, Ammo)),
	Ammo1 is Ammo - 1,
	(Ammo1 > 0 
	 -> assert(d_weaponAmmo(ObjID, Ammo1)),
	    msgWindowWrite('Balas restantes: ' ),
	    msgWindowWriteln(Ammo1)
	 ;  destroyGameEntity(ObjID),
	    msgWindowWriteln('No te quedan balas! Tu arma se ha autodestruido!'),
	    retract(d_totalWeapons(TW)),
	    TW1 is TW - 1,
	    assert(d_totalWeapons(TW1))
	),
	checkIfEnemyDestroyed(EID, X, Y, DIR).

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


norule(_).
