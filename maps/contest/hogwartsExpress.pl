%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DEL VIAJE EN EL HOGWARTS EXPRESS
%% HISTORIA:
%%    Tras adquirir todo el material necesario, en el callejón Diagón y entrar al andén 9 3/4
%% a través del portal situado en una de las columnas de la estación de King's Cross, Tom 
%% Marvolo Riddle se dirige a Hogwarts en el tren Express. En el viaje, Tom decide Explorar los
%% vagones de otros estudiantes en busca de objetos mágicos de los que pueda hacer uso, pero
%% debe tener cuidado con los profesores, ya que patrullan en busca de alumnos que se estén
%% saltando las normas.
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
num_dots(108).
map(['#########################',
     '### HOGWARTS\' EXPRESS ###',
     '#########################',
     '#.....#...........#.....#',
     '#.......................#',
     '#####.#####.#.#####.#####',
     '#.....#.....#.....#.....#',
     '#.....#.....#.....#.....#',
     '#.....#.....#.....#.....#',
     '#########################']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Contenido modificable %%%%%%%%%%%%%%%
pacman_start(1, 3).     % PLMan puede empezar en cualquier parte que no sea sólida

%% Regla de inicialización del mapa (Se permiten hasta 4 Puertas y 4 Profesores)
initMap:- 
	addSolidObject('#'),

        crearPuerta(OID_P1,  5, 5, '-'),
        crearLlave(OID_P1, 5, 3, llave_a, 'a'),
        
        crearPuerta(OID_P2, 13, 5, '-'),
        crearLlave(OID_P2, 1, 8, llave_gris, 'g'),

        crearProfesor(EID_PROF1,  7, 3, enemyBasicMovement, mcgonagall, 'M'),
        enemyBasicMovement(init, EID_PROF1, right-left, ['#']),
        
        crearProfesor(EID_PROF2, 13, 6, entitySequentialMovement, flitwick, 'F'),
        entitySequentialMovement(init, EID_PROF2, 
                [r,r,r,r,d,d,l,l,l,l,u,u], [ no_repeat_moves ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

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
%% Crear un nuevo Profesor, dada una posición y un nombre de comportamiento
%%
crearProfesor(EID, X, Y, BEHAVIOUR, NOMBRE, CARACTER):-
    createGameEntity(EID, CARACTER, mortal, X, Y, active, BEHAVIOUR, 
                   [name(NOMBRE), solid(false), static(true), 
                    appearance(attribs(normal, red, default)), 
                    description('Profesor de Hogwarts vigilando a los estudiantes discolos')]).
