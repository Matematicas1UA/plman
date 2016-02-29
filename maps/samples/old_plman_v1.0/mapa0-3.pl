%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa0-3.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dificultad: 1
% Puntuación: 60

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', ' ', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#'],
['#', ' ', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(11, 7).
num_dots(26).
pacman_start(1, 5).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('&', object, 2, 2, inactive, norule, 
	                 data(busto, solid, not_static, norule, 'Busto que impide el paso')), 
	createGameEntity('$', object, 8, 4, inactive, norule, 
	                 data(billete, not_solid, static, norule, 'Billete de 1 dolar pegado al suelo')).
norule(_).
