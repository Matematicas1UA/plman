map_format_version(1.0).

map([['#', '#', '#', '#', '#', '#', '#', '#']
    ,['#', ' ', ' ', ' ', '.', ' ', '.', '#']
    ,['#', ' ', ' ', ' ', '#', '#', '.', '#']
    ,['#', '#', '#', '#', '#', '#', '#', '#']]).

map_size(8, 4).
num_dots(3).
pacman_start(1, 1).
initMap:- 
      addSolidObject('#')
   ,  createGameEntity(OID_P, '|', object, 5, 1, inactive, norule, [ name(ordereddoor), solid(true), static(true), use_rule(norule), description('Ordered door'),   appearance(attribs(normal, black, yellow)) ])
   ,  randomPermutation([ 1, 2, 3 ], [ N1, N2, N3 ] )
   ,  createGameEntity(OID_N1, N1, object, 1, 2, inactive, norule, [ name(number1),  solid(false), static(false), use_rule(norule), description('First Number'),  appearance(attribs(bold, cyan, default)) ])
   ,  createGameEntity(OID_N2, N2, object, 2, 2, inactive, norule, [ name(number2),  solid(false), static(false), use_rule(norule), description('Second Number'),  appearance(attribs(bold, cyan, default)) ])
   ,  createGameEntity(OID_N3, N3, object, 3, 2, inactive, norule, [ name(number3),  solid(false), static(false), use_rule(norule), description('Thierd Number'),  appearance(attribs(bold, cyan, default)) ])
   ,  createGameEntity(     _, '', object, 9, 6,   active, orderchecker, [ name(orderchecker),  solid(false), static(true), use_rule(norule), description('Order Checker') ])
   ,  orderchecker(init, OID_P, [ n(OID_N1, N1, N1, 2), n(OID_N2, N2, N2, 2), n(OID_N3, N3, N3, 2) ])
   .

% Checks for the numbers to be in order
orderchecker(init, OID_DOOR, LIST_KEYS) :-
      assert(d_orderDoor(OID_DOOR))
   ,  orderchecker(init, LIST_KEYS).

orderchecker(init, []) :- !.
orderchecker(init, [ n(OID, AP, X, Y) | OTHERS ]) :-
      assert(d_order(OID, AP, X, Y))
   ,  orderchecker(init, OTHERS).

% Control Rule
orderchecker(EID):-
      forall(  d_order(OID, AP, X, Y)
            ,  ( entityLocation(OID, X, Y, AP), not(isObjectGot(OID))) 
            )
   ,  d_orderDoor(OID_DOOR)
   ,  destroyGameEntity(OID_DOOR)
   ,  destroyGameEntity(EID).
orderchecker(  _).