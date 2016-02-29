%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DE GRINGOTTS
%% HISTORIA:
%%     Eres Tom Marvolo Riddle, y acabas de recibir tu carta de admisión en Hogwarts, escuela
%%  de Magia y Hechicería. Tu madre, que murío al darte a luz, te dejó en herencia suficiente
%%  dinero del mundo de los magos para que pudieras pagarte los estudios (Galleons de oro, 
%%  Sickles de plata y Knuts de bronce). Debes ir al banco de los magos, Gringgots, situado
%%  en el callejón Diagón, y recuperar una parte de ese dinero para poder adquirir tu material
%%  escolar antes de ir a Hogwarts.
%%
%% INFORMACIÓN IMPORTANTE:
%%     Este mapa es una plantilla para crear tus propias versiones. Sólo te está permitido 
%%  modificar la zona marcada como "Contenido Modificable". El resto del mapa debe permanecer
%%  inalterado, aunque te recomendamos que estudies y entiendas el funcionamiento para 
%%  sacarle el máximo partido.
%%
%%--------------------------------------------------------------------------------------------

%% MAPA 
map_format_version(1.1).
load_behaviour(enemyBasicMovement).
load_behaviour(entitySequentialMovement).
load_behaviour(basicDoorKey).
num_dots(62).
map(['###########',
     '#GRINGOTTS#',
     '###########',
     '#...#.#...#',
     '#.........#',
     '#..#...#..#',
     '#..#...#..#',
     '#..#...#..#',
     '#..#...#..#',
     '#.........#',
     '###########',
     '#.........#',
     '###########']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Contenido modificable %%%%%%%%%%%%%%%

pacman_start(1, 9).  % PLMan sólo puede aparecer en filas 3-9
posicionXPuerta(5).  % De 1 a 9
posicionBolsa(5, 3). % Posiciones válidas(X: 1-9, Y: 3-9)

%% Regla de inicialización del mapa (Se permiten hasta 4 Goblins)
initMap:- 
	addSolidObject('#'),

        crearPuertaYBolsa,
        
        crearGoblin(EID_G1, 1, 5, enemyBasicMovement),
        enemyBasicMovement(init, EID_G1, up-down, ['#']),
        
        crearGoblin(EID_G2, 8, 8, entitySequentialMovement),
        entitySequentialMovement(init, EID_G2, 
                [d,l,l,u,u,u,u,u,r,r,d,d,d,d], [ no_repeat_moves ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

%%
%% Crea la puerta en su zona del mapa, destruyendo la pared de debajo
%% Coloca la bolsa en una posición del mapa (ojo que no haya un sólido)
%% La bolsa se usa como llave para salir
%%
crearPuertaYBolsa :-
    % Puerta
    posicionXPuerta(PX),
    setDCellContent(PX, 10, ' '),
    createGameEntity(OID_P, '-', object, PX, 10, inactive, norule, 
                   [name(puerta), solid(true), static(true), 
                    appearance(attribs(normal, cyan, default)), 
                    description('Puerta para salir de Gringgots')]),
    % Bolsa
    posicionBolsa(BX, BY),
    createGameEntity(OID_B, '&', object, BX, BY, inactive, norule, 
                   [name(bolsa), solid(true), static(false), use_rule(basicDoorKey),
                    appearance(attribs(normal, cyan, default)), 
                    description('Bolsa llena de Galleons de oro')]),
    basicDoorKey(init, OID_P, [ 'pl-man':destroyGameEntity(OID_P) ], [ OID_B ]).

%%
%% Crear un nuevo Goblin, dada una posición y un nombre de comportamiento
%%
crearGoblin(EID, X, Y, BEHAVIOUR):-
    createGameEntity(EID, 'G', mortal, X, Y, active, BEHAVIOUR, 
                   [name(goblin), solid(false), static(true), 
                    appearance(attribs(normal, red, default)), 
                    description('Goblin guardian de Gringgots')]).
