%%
%% Experimental map to test cauldrons, in map format 1.1.
%%
map_format_version(1.1).
load_behaviour(cauldron).
load_behaviour(enemyBasicMovement).
load_behaviour(basicDoorKey).
map(['#######',
     '# ....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#######',
     '#.....#',
     '#######']).
num_dots(29).
pacman_start(1, 5).

initMap:- 
	addSolidObject('#'),
	createGameEntity(_, 'a', object, 1, 4, inactive, norule, 
                [name(ing1), solid(true), static(false), appearance(attribs(normal, cyan, default)),
                description('Ingrediente 1')]),
	createGameEntity(_, 'b', object, 3, 1, inactive, norule, 
                [name(ing2), solid(true), static(false), appearance(attribs(normal, cyan, default)),
                description('Ingrediente 2')]),
        cauldron(create(OID_C, 1, 1, [])),
        dynamicProperties(set(OID_C, static(false))),
        cauldron(newRecipe(OID_C,  979303559,  1, [ 'pl-man':cauldronFinish(OID_C, 1) ])),
        cauldron(newRecipe(OID_C, 1498744391, 10, [ 'pl-man':cauldronFinish(OID_C, 2) ])).
        
% One combination of ingredients
cauldronFinish(OID, 1) :-
    cauldron(destroy(OID)), 
    destroyGameEntity(OID),
    setDCellContent(3, 6, ' '),
    createGameEntity(OID_D, '-', object, 3, 6, inactive, norule, 
                [name(ing2), solid(true), static(true), appearance(attribs(normal, cyan, default)),
                description('Puerta secreta')]),
    createGameEntity(OID_K, 'k', object, 1, 1, inactive, norule, 
                [name(magicKey), solid(true), static(false), appearance(attribs(normal, cyan, default)),
                use_rule(basicDoorKey), description('Llave magica secreta')]),
    basicDoorKey(init, OID_D, 
            ['pl-man':destroyGameEntity(OID_D), 'pl-man':destroyGameEntity(OID_K)], [OID_K]).

% Another combination of ingredients
cauldronFinish(_, 2) :-
    createGameEntity(EID, 'E', mortal, rnd(1,4), rnd(2,5), active, enemyBasicMovement, 
                [name(enemigo), solid(false), static(true), appearance(attribs(normal, red, default)),
                description('Perseguidor del mal')]),
    enemyBasicMovement(init, EID, left-right, ['#']).
