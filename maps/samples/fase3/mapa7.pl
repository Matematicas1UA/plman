%
% La puerta aritmética sólo se abre si usas la llave cuya apariencia se corresponde con el
% resultado de la operación aritmética que está indicada en la puerta. La operación siempre
% cambia, pero siempre es una suma. Las posibles llaves aritméticas están todas en posiciones
% aleatorias.
%
% La pistola esta siempre en una posición aleatoria, pero de la zona media del mapa.
% El enemigo patrulla siempre de la misma forma: no es aleatorio.
%

map_format_version(1.0).
load_behaviour(arithmeticDoor).
load_behaviour(entitySequentialMovement).
load_behaviour(gunBasic).
map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', ' ', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
['#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(15, 12).
num_dots(51).
pacman_start(12, 3).
initMap:- 
	addSolidObject('#'), 

	% Arithmetic Door
	createGameEntity(OID_O1,   0, object, 11, 4, inactive, norule, 
			[name(operando_1), solid(true), static(true), use_rule(norule),
			description('Primer operando de la puerta aritmetica'), appearance(attribs(bold, yellow, green))]), 
	createGameEntity(OID_OP, '+', object, 12, 4, inactive, norule, 
			[name(operador), solid(true), static(true), use_rule(norule),
			description('Operador de la puerta aritmetica'), appearance(attribs(bold, yellow, green))]), 
	createGameEntity(OID_O2,   0, object, 13, 4, inactive, norule, 
			[name(operando_2), solid(true), static(true), use_rule(norule),
			description('Segundo operando de la puerta aritmetica'), appearance(attribs(bold, yellow, green))]), 
	arithmeticDoor(init, OID_OP, OID_O1, OID_O2, RES, [operators(['+'])]),
	createGameEntity(  _, RES, object, rnd(1,13), rnd(1,3), inactive, norule,
			[name(llave_aritmetica), solid(false), static(false), use_rule(arithmeticDoorKey),
			description('Objeto llave para puertas aritmeticas'), appearance(attribs(bold, cyan, default)) ]),
	arithmeticDoor(createRandomKeys, OID_OP, 3, location(rnd(1,13), rnd(1, 3)), _,
      [ distributed, add_properties([appearance(attribs(bold, cyan, default))]) ]),

	% Gun
	createGameEntity(OID_G, 'l', object, rnd(1,13), rnd(5,7), inactive, norule, 
			[name(derringer), solid(false), static(false), use_rule(gunBasic),
			description('Pistola derringer de 1 sola bala'), appearance(attribs(bold, cyan, default))]), 
	gunBasic(init, OID_G, 1, ['E'], keep),
	createGameEntity(EID_0, 'E', mortal, 13, 9, active, entitySequentialMovement, [appearance(attribs(normal, red, default))]),
	entitySequentialMovement(init, EID_0, [ l,l,l,l,l,l,l,l,r,r,r,r,r,r,r,r ], [ no_repeat_moves ]).
norule(_).
norule(_,_,_,_,_).
