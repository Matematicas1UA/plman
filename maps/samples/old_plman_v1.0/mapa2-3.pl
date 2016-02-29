% dificultad: 7
% Puntuacion: 500

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa2-3.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic d_ghostControl/3.

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(55, 11).
num_dots(422).
pacman_start(X, Y):-
	X is random(45) + 5,
	Y is random(6) + 3.
initMap:- 
	retractall(d_ghostControl(_,_)),
	
	addSolidObject('#'), 
	createGhosts(10).

% Crear spiderGhosts
createGhosts(0):-!.
createGhosts(N):-
	A is random(2),
	(A=0 -> X=1 ; X=53),
	Y is random(8) + 1,
	createGameEntity('F', mortal, X, Y, active, spiderGhost2, 0),
	N1 is N - 1,
	createGhosts(N1).

% Fantasma Araña Modificado
spiderdir(left).
spiderdir(right).
spiderdir(down).
spiderdir(up).
spiderGhost2(_):-
	random(100) > 95, !.
spiderGhost2(EID):-
	not(d_ghostControl(EID, _, _)),
	abolish(d_options/1), assert(d_options([])),
	forall((spiderdir(Dir), see(EID, normal, Dir, V), V\='#'),
	       (retract(d_options(L)), append(L, [Dir], NL), assert(d_options(NL)))
	      ),
        d_options(L), length(L, Max), Max > 0, 
	D1 is random(Max), D2 is random(Max),
	nth0(D1, L, DMove), nth0(D2, L, DSee),
	assert(d_ghostControl(EID, DMove, DSee)), !.
spiderGhost2(EID):-
	d_ghostControl(EID, DMove, _),
	see(EID, normal, DMove, '#'),
	retract(d_ghostControl(EID, _, _)), !.
spiderGhost2(EID):-
	d_ghostControl(EID, DMove, DSee),
	DMove \= DSee,
	see(EID, list, DSee, SEELIST),
	member('@', SEELIST),
	retract(d_ghostControl(EID, _, _)),
	assert(d_ghostControl(EID, DSee, DSee)),
	assert(d_doAction(EID, move(DMove))), !.
spiderGhost2(EID):-
	d_ghostControl(EID, DMove, _),
	assert(d_doAction(EID, move(DMove))).




