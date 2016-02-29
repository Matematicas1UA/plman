%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej4.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(14, 3).
num_dots(11).
pacman_start(12, 1).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_0, 'V', object, 4, 1, inactive, norule, 
			[name(maceta), solid(true), static(false), use_rule(norule),
			description('Gran maceta que impide el paso'), appearance(attribs(normal, magenta, default))]), 
	createGameEntity(OID_1, 'U', object, 8, 1, inactive, norule, 
			[name(maceta), solid(true), static(false), use_rule(norule),
			description('Gran maceta que impide el paso'), appearance(attribs(normal, magenta, default))]).
norule(_).
norule(_,_,_,_,_).
