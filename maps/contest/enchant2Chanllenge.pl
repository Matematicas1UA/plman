%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DEL VIAJE EN EL ENCANTO FLIPENDO
%% HISTORIA:
%% 
%%--------------------------------------------------------------------------------------------

%% MAPA 
map_format_version(1.1).
load_behaviour(enemyBasicMovement).
load_behaviour(entitySequentialMovement).
load_behaviour(basicDoorKey).
load_behaviour(magicWand).

map(['########################',
     '###  ENCHANT  CLASS  ###',
     '########################',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '#  ..................  #',
     '# .................... #',
     '########################',
     '#  Use flIpEnDO Spell  #',
     '########################']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Contenido modificable %%%%%%%%%%%%%%%
pacman_start(12, 12).     % PLMan puede empezar en cualquier parte que no sea sólida
num_dots(182).
%% Regla de inicialización del mapa (Se permiten hasta 4 Puertas y 4 Profesores)
initMap:- 
	addSolidObject('#'),
	addSolidObject('H'),
	addSolidObject('E'),
	addSolidObject('A'),
	addSolidObject('R'),

    crearEnemigo(EID_1,  1, 3, enemyBasicMovement, enemigo1, 'M'),
        enemyBasicMovement(init, EID_1, right-left, ['#','D']),
	crearEnemigo(EID_2,  2, 4, enemyBasicMovement, enemigo2, 'M'),
        enemyBasicMovement(init, EID_2, right-left, ['#','u']),
    crearEnemigo(EID_3,  3, 5, enemyBasicMovement, enemigo3, 'M'),
        enemyBasicMovement(init, EID_3, right-left, ['#','m']),
	crearEnemigo(EID_4,  4, 6, enemyBasicMovement, enemigo4, 'M'),
        enemyBasicMovement(init, EID_4, right-left, ['#','b']),
	crearEnemigo(EID_5,  5, 7, enemyBasicMovement, enemigo5, 'M'),
        enemyBasicMovement(init, EID_5, right-left, ['#','l']),
	crearEnemigo(EID_6,  6, 8, enemyBasicMovement, enemigo6, 'M'),
        enemyBasicMovement(init, EID_6, right-left, ['#','e']),
	crearEnemigo(EID_7,  7, 9, enemyBasicMovement, enemigo7, 'M'),
        enemyBasicMovement(init, EID_7, right-left, ['#','d']),
    crearEnemigo(EID_8,  8,10, enemyBasicMovement, enemigo8, 'M'),
        enemyBasicMovement(init, EID_8, right-left, ['#','o']),
    crearEnemigo(EID_9,  9,11, enemyBasicMovement, enemigo9, 'M'),
        enemyBasicMovement(init, EID_9, right-left, ['#','r']),
	
	crearObjetoFlipendo(_, 20,3, 'D'),
    crearObjetoFlipendo(_, 20,4, 'u'),
    crearObjetoFlipendo(_, 20,5, 'm'),
    crearObjetoFlipendo(_, 20,6, 'b'),
    crearObjetoFlipendo(_, 20,7, 'l'),
    crearObjetoFlipendo(_, 20,8, 'e'),
    crearObjetoFlipendo(_, 20,9, 'd'),
    crearObjetoFlipendo(_, 20,10, 'o'),
    crearObjetoFlipendo(_, 20,11, 'r'),

	magicWand(create(_, 22, 12, [ setStandardSpells(true) ])).

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
