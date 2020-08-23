map_format_version(1.0).
load_behaviour(arithmeticDoor).

map([['#', '#', '#', '#', '#', '#', '#']
    ,['#', ' ', ' ', ' ', ' ', '.', '#']
    ,['#', ' ', ' ', '.', ' ', '.', '#']
    ,['#', ' ', ' ', ' ', ' ', '.', '#']
    ,['#', '#', '#', '#', '#', '#', '#']]).

map_size(7, 5).
num_dots(4).
pacman_start(1, 2).
initMap:- 
      addSolidObject('#')
   ,  createGameEntity(OID_OP, '+', object, 4, 2, inactive, norule, [ name(operator), solid(true), static(true), use_rule(norule), description('Operand door'),   appearance(attribs(normal, black, yellow)) ])
   ,  createGameEntity(OID_N1,   1, object, 4, 1, inactive, norule, [ name(number1),  solid(true), static(true), use_rule(norule), description('First Operand'),  appearance(attribs(normal, black, green)) ])
   ,  createGameEntity(OID_N2,   2, object, 4, 3, inactive, norule, [ name(number2),  solid(true), static(true), use_rule(norule), description('Second Operand'), appearance(attribs(normal, black, green)) ])
   ,  arithmeticDoor(init, OID_OP, OID_N1, OID_N2, RES, [operators(['+', '-', '/', '*'])])
   ,  arithmeticDoor(createRandomKeys, OID_OP, 2, location(0, 0), LKEYS, [ add_properties([appearance(attribs(bold, cyan, default))]) ])
   ,  createGameEntity(OID_KEY, RES, object, 0, 0, inactive, norule,[ name(llave_aritmetica), solid(false), static(false), use_rule(arithmeticDoorKey), description('Objeto llave para puertas aritmeticas'), appearance(attribs(bold, cyan, default)) ])
   ,  randomPermutation([ l(3,1), l(2,2), l(3,3) ], [ l(L1X,L1Y), l(L2X,L2Y), l(L3X,L3Y) ] )
   ,  LKEYS = [ KEY1, KEY2 ]
   ,  changeEntityLocation(   KEY1, L1X, L1Y, 0, 0)
   ,  changeEntityLocation(   KEY2, L2X, L2Y, 0, 0)
   ,  changeEntityLocation(OID_KEY, L3X, L3Y, 0, 0)
   .
