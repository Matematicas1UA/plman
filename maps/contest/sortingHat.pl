%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% PLANTILLA DE MAPA DEL GRAN SALÓN Y LA CEREMONIA DE ASIGNACIÓN
%% HISTORIA:
%%     Nada más llegar a Hogwarts, Tom Riddle entra en el Gran Salón donde va a dar comienzo
%%  el gran banquete de bienvenida. Pero, antes del gran banquete, los nuevos alumnos deben 
%%  ser asignados a una de las 4 casas de Hogwarts. Este trabajo lo realiza el sombrero seleccionador.
%%  En este mapa, Tom debe decirle algo al sombrero para que éste entienda que debe ponerle en
%%  la casa Slytherin. En este mapa puedes crear personajes que le den pistas a Tom sobre qué
%%  debe decirle exactamente al sombrero. Cuando Tom se acerce al sombrero y le diga lo que debe,
%%  el sombrero aceptará ponerle en Slytherin y hará alguna acción que permita a Tom terminar el mapa.
%%
%% INFORMACIÓN IMPORTANTE:
%%     Este mapa es una plantilla para crear tus propias versiones. Puedes modificar todo excepto
%% los comportamientos disponibles y las partes sólidas de la definición del mapa (las 5 mesas,
%% las paredes principales y el tamaño del mapa). Todo lo demás es modificable a tu criterio.
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

map(['###############################',
     '####### THE GREAT HALL ########',
     '###############################',
     '#............................##',
     '#..                     ......#',
     '#..==S=L=Y=T=H=E=R=I=N==...[..#',
     '#..........................[..#',
     '#..==R=A=V=E=N=C=L=A=W==...[..#',
     '#..........................[..#',
     '#..=G=R=I=F=F=I=N=D=O=R=...[..#',
     '#..........................[..#',
     '#..=H=U=F=F=L=E=P=U=F=F=...[..#',
     '#.............................#',
     '#............................##',
     '###############################']).
num_dots(226).
pacman_start(1, 3).

%% Regla de inicialización del mapa (Se permiten hasta 20 entidades en total)
initMap:- 
        defineSolidObjects(['#', '[', '=']),
        crearSombreroSeleccionador(26, 8, 'ç', controlSombrero).

%%%------------------------------------------------------------------------
%%% Reglas auxiliares para simplificar la creación
%%%------------------------------------------------------------------------

%%
%% Define todos los objetos sólidos estáticos del mapa, a partir de una lista 
%%
defineSolidObjects(LIST):-
    forall(member(M, LIST), addSolidObject(M)).

%%
%% Crea el sombrero seleccionador, que debe respondernos cuando le hablemos
%% desde cerca, para decirnos en qué casa nos toca estar. Para eso, tendrá una
%% regla de control que comprobará si nos oye decir algo en concreto
%%
crearSombreroSeleccionador(X, Y, CARACTER, REGLA_CONTROL):-
    createGameEntity(_OID, CARACTER, object, X, Y, active, REGLA_CONTROL, 
                   [name(sombreroSel), solid(false), static(true), 
                    appearance(attribs(bold, magenta, default)), 
                    description('Sombrero seleccionador de Hogwarts')]).

%%
%% Regla de control: será llamada una vez por turno, y servirá para que el
%% sombrero pueda ver si se cumplen condiciones para hacer algo. EID es el
%% identificador de la entidad sombrero, para la que es llamada esta regla.
%%
controlSombrero(EID):-
    % Oir mensajes a nuestro alrededor y guardarnos nombre(NAME) y tipo(TYPE)
    % de quien produce el mensaje(MSG), y mensaje que dice.
    hear(EID, normal, msg(NAME, TYPE, MSG)), 
    
    % Imprimir lo que OIGO, para comprobar que oigo cosas. Eliminar esta
    % llamada cuando todo funcione correctamente
    % maplist(user:write, ['OIGO: (', NAME, ')(', TYPE, ')(', MSG, ')\n']),

    % Llamar a una regla que procesa lo que oigo y actua.
    escucharYActuar(EID, MSG).
   
%%
%% Para cada posible mensaje que queramos procesar, escribimos una cláusula
%% de esta regla.
%%
escucharYActuar(EID, 'hola'):-
    decir(EID, 'Hola Tom').
escucharYActuar(EID, 'Slytherin'):-
    % Crear 21 nuevos cocos encima de la mesa de Slytherin
    forall(between(3,23,X), setDCellContent(X, 4, '.')),
    decir(EID, 'Muy bien, te pondre en Slytherin!').
escucharYActuar(EID, _OTRACOSA):-
    decir(EID, 'Perdona, te entiendo.').
decir(EID, RESPUESTA):-
    doAction(EID, say(RESPUESTA)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
