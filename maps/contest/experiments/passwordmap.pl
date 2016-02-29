%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapaej5.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).
load_behaviour(entitySequentialMovement).
load_behaviour(password).
map([['#', '#', '#', '#', '#'],
['#', '.', '.', '.', '#'],
['#', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', '#'],
['#', '#', '#', '#', '#']]).
map_size(14, 4).
num_dots(3).
pacman_start(1, 4).
initMap:- 
	addSolidObject('#'), 
	createGameEntity(EID_0, '#', object, 0, 0, active, passwordChecker, []),
	createGameEntity(EID_1, 'P', object, 1, 2, active, passwordDigit, 
                [name(digito1), solid(true), static(true), appearance(attribs(normal, black, white)), 
                 description('Digito modificable 1')]),
	createGameEntity(EID_2, 'P', object, 2, 2, active, passwordDigit, 
                [name(digito2), solid(true), static(true), appearance(attribs(normal, black, white)),
                 description('Digito modificable 2')]),
	createGameEntity(EID_3, '#', object, 3, 2, inactive, norule, 
                [name(pared_bloqueada), solid(true), static(true), description('Pared bloqueada con contrasenya')]),
        passwordDigit(init, EID_1, [ digits([a,b,c,d,1,2,3,4]), switchPos(1,3) ]),
        passwordDigit(init, EID_2, [ digits([0,1,2,3,4]), switchPos(2,3) ]),
        passwordChecker(init, EID_0, [ passwordDigits([EID_1, EID_2]), 
                                       password('916982abc45b40a23c1a0e213e525cb096ae3259'),
                                       action('pl-man':destroyGameEntity(EID_3)) ] ).
