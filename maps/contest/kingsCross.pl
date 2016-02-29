%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% LA ESTACIÓN DE KING'S CROSS Y EL ANDÉN 9 3/4 
%%
%% HISTORIA:
%%     Tras pasar por Gringotts y recoger algo de dinero, Tom Marvolo Riddle camina por el
%% callejón Diagón, donde se encuentran todas las tiendas de material del mundo de la magia.
%% En este mapa, Tom debe llegar a Ollivander's a conseguir su varita mágica, con lo que ya
%% tendrá todo su material y podrá volver al Pub El Caldero Correante.
%%
%%--------------------------------------------------------------------------------------------

map_format_version(1.1).
load_behaviour(enemyBasicMovement).
load_behaviour(basicTeletransport).
load_behaviour(basicDoorKey).
map(['##############################',
     '#............................#',
     '#..#1#....#1#....#1#....#1#..#',
     '#..#0#....#0#....#0#....#0#..#',
     '#..###....###....###....###..#',
     '#..###....###....###....###..#',
     '#..#9#....#9#....#9#....#9#..#',
     '#............................#',
     '##############################',
     '######## KING\'S CROSS ########',
     '##############################',
     '#............................#',
     '#...............#TTTT-TTTT-G>#',
     '##############################']).
num_dots(179).
pacman_start(28, 4).

initMap:- 
	addSolidObject('#'),
        
        crearBillete(OID_B, 1, 3),
        crearSupervisor(_OID_S, OID_B, crearPortal(19,3,1,12)),
        crearPuerta(_OID_P, OID_B, 16, 11, '['),
        
        ENEMIGOS=[ e('P',1,7,right-left), e('P',12,1,left-right), e('P',14, 5,   down-up),
                   e('E',7,3,left-right), e('E',27,2,   up-down), e('S', 1,11,right-left) ],
        forall(member(e(AP, X,Y,DIR), ENEMIGOS), crearEnemigo(_OID_E, AP, X, Y, DIR)).

%%
%% Billete necesario para poder coger el tren
%%
crearBillete(OID_B, X, Y):-
    createGameEntity(OID_B, '=', object, X, Y, inactive, norule,
                   [name(billeteHS), solid(true), static(false), use_rule(basicDoorKey), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Billete de acceso al Hogwarts Express. Anden 9 y 3/4.')]).

%%
%% Mecanismo supervisor para crear el portal de acceso
%%
crearSupervisor(OID_S, OID_CHECKED, ACTION):-
    entityLocation(OID_CHECKED, X, Y, _),
    createGameEntity(OID_S, '', object, 0, 0, active, supervisar,
                   [name(supervisor), solid(false), static(false),  checkAction([ACTION]),
                    appearance(attribs(bold, cyan, default)), check([OID_CHECKED, X, Y]),
                    description('Supervision de billete')]).

% Regla de supervisión: cuando el billete deje de estar en su sitio, se dispara
supervisar(EID):-
    dynamicProperties(get(EID, check([OID_CHECKED, X, Y]))),
    not(entityLocation(OID_CHECKED, X, Y, _)),
    dynamicProperties(get(EID, checkAction([ACTION]))),
    call(ACTION).

%%
%% Crear y abrir el portal 
%%
crearPortal(X, Y, XD, YD):-
    setDCellContent(X, Y, ' '),
    createGameEntity(OID_P, '#', object, X, Y, active, basicTeletransport,
                   [name(portal), solid(false), static(true),
                    appearance(attribs(bold, green, default)),
                    description('Portal hacia el anden 9 y 3/4')]),
    basicTeletransport(init, OID_P, from(X, Y), to(XD, YD), ['@'] ,[ ]).

%%
%% Puerta de acceso al tren
%%
crearPuerta(OID_P, OID_B, X, Y, CARACTER):-
    createGameEntity(OID_P, CARACTER, object, X, Y, inactive, norule, 
                   [name(puertaHS), solid(true), static(true), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Puerta de acceso al Hogwarts\' Express')]),
    basicDoorKey(init, OID_P, [ 'pl-man':destroyGameEntity(OID_P) ], [ OID_B ]).

%%
%% Crea un enemigo simple
%%
crearEnemigo(EID, CARACTER, X, Y, DIR) :-
    createGameEntity(EID, CARACTER, mortal, X, Y, active, enemyBasicMovement, 
                   [name(enemigo), solid(false), static(true), 
                    appearance(attribs(normal, red, default)), 
                    description('Paseante enfadado que te hace perder el tren')]),
    enemyBasicMovement(init, EID, DIR, ['#', '0', '9', '1', 'P', '[', 'E']).

