%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DE LA BUSQUEDA DEL LIBRO DE POCIONES AVANZADAS
%% HISTORIA:
%%     Ya dentro de Hogwarts, Tom Riddle enseguida toma conciencia de sus excepcionales poderes.
%% Al descubrir sus grandes capacidades, empieza a querer saber más y más sobre su historia pero,
%% sobre todo, acerca de cómo hacer hechizos y pociones más poderosos. En este camino, Tom 
%% descubre el libro Moste Potente Potions pero no puede conseguirlo fácilmente porque está en
%% la sección restringida de la biblioteca. En este mapa, Tom deberá esquivar a Filch, a la señora
%% Norris, a Peeves y a Irma Pince (la bibliotecaria) para conseguir el libro y llevarlo consigo
%% a la sala común de Slytherin.
%%
%% INFORMACIÓN IMPORTANTE:
%%     Este mapa es una plantilla para crear tus propias versiones. Puedes modificar todo excepto
%% los comportamientos disponibles y la estructura del mapa (las 4 paredes principales, el tamaño
%% de la biblioteca, la sala común Slytherin y el pasillo) Todo lo demás es modificable a tu criterio.
%% Ojo: debes seguir cumpliendo las restricciones de la categoría para poder entregar el mapa.
%% Tenlo en cuenta a la hora de construir tu solución.
%% 
%%--------------------------------------------------------------------------------------------

%% MAPA 
map_format_version(1.1).
load_behaviour(entitySequentialMovement).
load_behaviour(enemyBasicMovement).
load_behaviour(automaticArcher).
load_behaviour(basicDoorKey).
load_behaviour(magicWand).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Contenido modificable %%%%%%%%%%%%%%%

map(['######################',
     '#SLYTHERIN#  LIBRARY #',
     '######################',
     '#.........#..........#',
     '#.........#.|===|....#',
     '#.........#..........#',
     '#.........#.|======|.#',
     '###.##.####..........#',
     '#.........#....|===|.#',
     '#.##.##.###..........#',
     '#.........#..........#',
     '##.##..#############.#',
     '#....................#',
     '#....................#',
     '######################']).
num_dots(165).
pacman_start(1, 12).

%% Regla de inicialización del mapa (Se permiten hasta 15 entidades como máximo)
initMap:- 
        defineSolidObjects(['#', '|', '=']),
        crearLibro(14, 5, 'B').

%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

%%
%% Define todos los objetos sólidos estáticos del mapa, a partir de una lista 
%%
defineSolidObjects(LIST):-
    forall(member(M, LIST), addSolidObject(M)).

%%
%% Crea el libro de pociones avanzadas
%%
crearLibro(X, Y, CARACTER):-
    createGameEntity(_OID, CARACTER, object, X, Y, inactive, norule, 
                   [name(libroMPP), solid(false), static(false), 
                    appearance(attribs(bold, cyan, default)), 
                    description('Moste Potente Potions, por Phineas Bourne')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
