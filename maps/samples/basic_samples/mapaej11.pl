%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej11.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(basicTeletransport).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', ' ', '.', '.', '#', '.', '.', '.', '.', '.', '#'],
['#', ' ', '.', '.', '.', '.', '#', ' ', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(13, 5).
num_dots(27).
pacman_start(3, 2).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(OID_TT, '?', object, 1, 3, active, basicTeletransport, 
			[name(teletransporte), solid(false), static(true), use_rule(norule),
			description('Runa antigua de teletransporte'), appearance(attribs(bold, white, magenta))]), 
	basicTeletransport(init, OID_TT, from(1, 3), to(7, 3), ['@'] , []).
	
norule(_).
norule(_,_,_,_,_).
