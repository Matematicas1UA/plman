%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapa0.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', ' ', '#', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#'],
['#', '.', '#', '.', '#', '.', '#', ' ', '#', '.', '#', ' ', '#', '.', '#', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', ' ', ' ', '.', '#', ' ', ' ', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(17, 5).
num_dots(25).
pacman_start(1, 1).
initMap:- 
	addSolidObject('#').
norule(_).
norule(_,_,_,_,_).
