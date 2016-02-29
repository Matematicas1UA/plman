%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa0-4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dificultad: 1
% puntuacion: 65

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', ' ', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '.', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(25, 5).
num_dots(47).
pacman_start(6, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('|', object, 5, 1, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('+', object, 1, 3, inactive, norule, 
	                 data(llave, not_solid, not_static, keyUseRule, 'Llave de la puerta')).
keyUseRule(ObjID, _, X, Y, _):-
        entityLocation(PuertaID, X, Y, '|'),
        changeEntityAppearance(PuertaID, '[', _),
        makeObjectNotSolid(PuertaID),
        destroyGameEntity(ObjID),
        msgWindowWriteln('Has abierto la puerta! Ahora puedes pasar!'), !.
keyUseRule(_, _, _, _, _):-
        msgWindowWriteln('No parece que puedas usar la llave ahi!').

norule(_).
