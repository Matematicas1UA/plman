%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mimapa4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 1
%Puntuación: 100

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '#', '.', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', ' ', '#', '#', '.', '.', '.', '#', '#', '#', '.', '.', '.', '#', '#', '#', '.', '.', '.', '#'],
['#', '.', '.', '#', '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
[]]).
map_size(20, 9).
num_dots(84).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('{', object, 18, 1, inactive, norule, 
	                 data(llave_buena, not_solid, not_static, abrirPuerta, 'Llave que permite abrir la puerta')), 
	createGameEntity('-', object, 1, 5, inactive, norule, 
	                 data(puerta, solid, static, norule, 'Puerta')), 
	createGameEntity('}', object, 18, 6, inactive, norule, 
	                 data(llave_mala, not_solid, not_static, ahCapullin, 'Llave que NO permite abrir la puerta')).

% PREDICADO QUE IMPLEMENTA EL USO USO DE LA LLAVE ADECUADA
abrirPuerta(LlaveID, _, X, Y, _):-
	entityLocation(EID2, X, Y, '-'),
	changeEntityAppearance(EID2, '\\', _),
	makeObjectNotSolid(EID2),
	destroyGameEntity(LlaveID),
	msgWindowWriteln('Usas la llave y...abres la puertesica!'), !.
abrirPuerta(_, _, _, _, _):-
	msgWindowWriteln('No parece que puedas usar la llave ahi!').

% PREDICADO QUE IMPLEMENTA EL USO DE LA LLAVE NO ADECUADA
ahCapullin(_,_,_,_,_):- msgWindowWriteln('Espabila porque NO es esta llave!').

norule(_).
