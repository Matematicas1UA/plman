%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DEL VIAJE EN EL ENCANTO FLIPENDO
%% HISTORIA:
%% 
%%--------------------------------------------------------------------------------------------

/*
map(['###########################',
     '###    ENCHANT CLASS    ###',
     '###########################',
     '#E ..................D  #.#',
     '# E..................u  #.#',
     'h  E.................m  #.#',
     'e  .E................b  #.#',
     'a  ..E...............l  #.#',
     'r  ...E..............e  #.#',
     '#  ....E.............d  #.#',
     '#  .....E............o  #.#',
     '#  ......E...........r  #.#',
     '#O....................  P.#',
     '###########################']).
*/
%% MAPA 
map_format_version(1.1).
load_behaviour(enemyBasicMovement).
load_behaviour(entitySequentialMovement).
load_behaviour(basicDoorKey).
load_behaviour(magicWand).

map(['###########################',
     '###    ENCHANT CLASS    ###',
     '###########################',
     '#  ..................   #.#',
     '#  ..................   #.#',
     'H  ..................   #.#',
     'E  ..................   #.#',
     'A  ..................   #.#',
     'R  ..................   #.#',
     '#  ..................   #.#',
     '#  ..................   #.#',
     '#  ..................   #.#',
     '#O....................  P.#',
     '###########################']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Contenido modificable %%%%%%%%%%%%%%%
pacman_start(12, 12).     % PLMan puede empezar en cualquier parte que no sea sólida
num_dots(108).
%% Regla de inicialización del mapa (Se permiten hasta 4 Puertas y 4 Profesores)
initMap:- 
	addSolidObject('#'),
	addSolidObject('H'),
	addSolidObject('E'),
	addSolidObject('A'),
	addSolidObject('R'),
        %crearPuerta(OID_P1,  5, 5, '-'),
        %crearLlave(OID_P1, 5, 3, llave_a, 'a'),
        
        %crearPuerta(OID_P2, 13, 5, '-'),
        %crearLlave(OID_P2, 1, 8, llave_gris, 'g'),

        crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D']),
	crearEnemigo(EID_2,  12, 4, enemyBasicMovement, enemigo2, 'M'),
        enemyBasicMovement(init, EID_2, right-left, ['#','u']),
        /*crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D'])
	crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D'])
	crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D'])
	crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D'])
	crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D'])*/
	
	crearObjetoFlipendo(EID_LD, 20,3, 'D'),

	magicWand(create(EID_V, 22, 12, [ setStandardSpells(true) ])).

        %crearProfesor(EID_PROF2, 13, 6, entitySequentialMovement, flitwick, 'F'),
        %entitySequentialMovement(init, EID_PROF2, 
         %       [r,r,r,r,d,d,l,l,l,l,u,u], [ no_repeat_moves ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

crearObjetoFlipendo(OID_P, X, Y, CARACTER):-
	createGameEntity(OID_P, CARACTER, object, X, Y, inactive, norule, 
                [name(molesto2), solid(true), static(true), 
                appearance(attribs(normal, cyan, default)),
                description('Letra')]).

%%
%% Crea una puerta en una posición determinada del mapa, y le pone un
%% carácter dado como apariencia.
%%
crearPuerta(OID_P, X, Y, CARACTER):-
    createGameEntity(OID_P, CARACTER, object, X, Y, inactive, norule, 
                   [name(puerta), solid(true), static(true), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Puerta corredera interna del tren')]).

%%
%% Crea una llave en una posición del mapa e inicializa el comportamiento 
%% como llave para abrir una puerta dada.
%%
crearLlave(OID_PUERTA, X, Y, NOMBRE, CARACTER):-
    createGameEntity(OID_LLAVE, CARACTER, object, X, Y, inactive, norule, 
                   [name(NOMBRE), solid(true), static(false), use_rule(basicDoorKey), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Llave para abrir una puerta del tren')]),
    basicDoorKey(init, OID_PUERTA, 
                ['pl-man':destroyGameEntity(OID_PUERTA) ], [ OID_LLAVE ]).

%%
%% Crear un nuevo Enemigo, dada una posición y un nombre de comportamiento
%%
crearEnemigo(EID, X, Y, BEHAVIOUR, NOMBRE, CARACTER):-
    createGameEntity(EID, CARACTER, mortal, X, Y, active, BEHAVIOUR, 
                   [name(NOMBRE), solid(false), static(true), 
                    appearance(attribs(normal, red, default)), 
                    description('Enemigo')]).
