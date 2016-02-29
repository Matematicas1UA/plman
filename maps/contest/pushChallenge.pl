%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DEL VIAJE EN EL HOGWARTS EXPRESS
%% HISTORIA:
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
load_behaviour(magicWand).
num_dots(159).
map(['#########################',
     '#### PUSH  CHALLENGE ####',
     '#########################',
     '#.... ... ..............#',
     '#..# ##..# ###......#...#',
     '#... ...... ....#####...#',
     '#..## #..... .......#...#',
     '#.... ..... ....#.......#',
     '#..# ##... ....######...#',
     '#... ....## #0..#.......#',
     '#.... ...... ...........#',
     '###   #        #     ####',
     '#.......................#',
     '#########################']).
pacman_start(1, 12).

%% Regla de inicialización del mapa (Se permiten hasta 4 Puertas y 4 Profesores)
initMap:- 
	addSolidObject('#'),
	addSolidObject('0'),

	mensajeUsarSpell,

	OBJS = [o(5,3,f), o(9,3,f), o(4,4,l), o(10,4,l), o(4, 5, 'I'), o(11, 5, 'I'),
                o(5,6,p), o(12,6,p),o(5,7,'E'),o(11,7,'E'),o(4,8,n), o(10,8,n),
                o(4,9,'D'),o(11,9,'D'),o(5,10,'O'),o(12,10,'O'),o(3,11,'U'),o(4,11,'S'),
                o(5,11,'E'),o(7,11,'f'),o(8,11,'l'),o(9,11,'I'),o(10,11,'p'),o(11,11,'E'),
                o(12,11,'n'),o(13,11,'D'),o(14,11,'O'), o(16,11,'S'),o(17,11,'P'),
                o(18,11,'E'),o(19,11,'L'),o(20,11,'L')],
	forall(member(o(X,Y,AP), OBJS), crearObjeto(_OID_O, X, Y, AP)),
	magicWand(create(_OID, 23, 12, [ setStandardSpells(true) ])),

	crearProfesor(EID_P1, 23, 3, entitySequentialMovement, 'slughorn', 's'),
	entitySequentialMovement(init, EID_P1, [ d,d,d,d,d,d,d,d,l,l,l,l,l,l,u,r,r,r,r,u,u,u,u,u,u,u,r,r ], [ no_repeat_moves ]),

	crearProfesor(EID_P2,  8, 3, enemyBasicMovement, 'bins', 'b'),
        enemyBasicMovement(init, EID_P2, down-up, ['n', '#', 'I', 'D', p, f, 'E', l, 'O']).
 
%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

%%
%% Crear un objeto que moleste
%%
crearObjeto(OID_P, X, Y, CARACTER):-
    createGameEntity(OID_P, CARACTER, object, X, Y, inactive, norule, 
                   [name(objetoMolesto), solid(true), static(true), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Objeto para molestar')]).

%%
%% Crear un nuevo Profesor, dada una posición y un nombre de comportamiento
%%
crearProfesor(EID, X, Y, BEHAVIOUR, NOMBRE, CARACTER):-
    createGameEntity(EID, CARACTER, mortal, X, Y, active, BEHAVIOUR, 
                   [name(NOMBRE), solid(false), static(true), 
                    appearance(attribs(normal, red, default)), 
                    description('Profesor de Hogwarts vigilando a los estudiantes discolos')]).

% Mensaje de uso de un Spell
mensajeUsarSpell:-
    user:writeln('Para usar un Spell, coge la varita y utiliza doAction(use(SPELL, DIR))').
