%%
%%--------------------------------------------------------------------------------------------
%%
%% IX EDICION CONCURSO DE PROGRAMACIÓN LÓGICA
%% EL CALLEJON DIAGON 
%%
%% HISTORIA:
%%     Tras pasar por Gringotts y recoger algo de dinero, Tom Marvolo Riddle camina por el
%% callejón Diagón, donde se encuentran todas las tiendas de material del mundo de la magia.
%% En este mapa, Tom debe llegar a Ollivander's a conseguir su varita mágica, con lo que ya
%% tendrá todo su material y podrá volver al Pub El Caldero Correante.
%%
%%--------------------------------------------------------------------------------------------

%% Formato del mapa
%% #######################
%% #E.##v..##.m.##..p##..#
%% #..##...##...##...##..#
%% #..###-####.####.###.E#
%% #..|.................@#
%% #..###.####.####.###E.#
%% #..##...##...##...##..#
%% #.E##..b##.l.##h..##..#
%% #######################

map_format_version(1.1).
load_behaviour(entitySequentialMovement).
load_behaviour(basicDoorKey).
map(['#######################',
     '#..##...##...##...##..#',
     '#..##...##...##...##..#',
     '#..###.####.####.###..#',
     '#.....................#',
     '#..###.####.####.###..#',
     '#..##...##...##...##..#',
     '#..##...##...##...##..#',
     '#######################',
     '#### DIAGON  ALLEY ####',
     '#######################']).
num_dots(87).
pacman_start(21, 4).

initMap:- 
	addSolidObject('#'),
        %% ENEMIGOS
	createGameEntity(EID_E1, 'E', mortal, 1, 1, active, entitySequentialMovement, [ appearance(attribs(normal, red, default)) ]),
        entitySequentialMovement(init, EID_E1, [r,d,d,l,u,u], [no_repeat_moves]),
        createGameEntity(EID_E2, 'E', mortal, 2, 7, active, entitySequentialMovement, [ appearance(attribs(normal, red, default)) ]),
        entitySequentialMovement(init, EID_E2, [l,u,u,r,d,d], [no_repeat_moves]),
        createGameEntity(EID_E3, 'E', mortal,21, 3, active, entitySequentialMovement, [ appearance(attribs(normal, red, default)) ]),
        entitySequentialMovement(init, EID_E3, [u,u,l,d,d,r], [no_repeat_moves]),
        createGameEntity(EID_E4, 'E', mortal,20, 5, active, entitySequentialMovement, [ appearance(attribs(normal, red, default)) ]),
        entitySequentialMovement(init, EID_E4, [d,d,r,u,u,l], [no_repeat_moves]),
        %% OBJETOS
        createGameEntity(_, 'v', object, 5, 1, inactive, norule, 
			[ name(varita), solid(false), static(false), description('Varita mágica Ollivander''s'), appearance(attribs(bold, cyan, default))]),
        createGameEntity(OID_G, 'g', object,11, 1, inactive, norule, 
			[ name(moneda), solid(false), static(false), use_rule(basicDoorKey), description('Galleon de oro de Banco de Magos Gringotts'), appearance(attribs(bold, magenta, default))]),
        createGameEntity(_, 'p', object,17, 1, inactive, norule, 
			[ name(pocion), solid(false), static(false), description('Poción Botica Slug && Jigger'), appearance(attribs(bold, green, default))]),
        createGameEntity(_, 'b', object, 7, 7, inactive, norule, 
			[ name(baratija), solid(false), static(false), description('Baratija de Sortilegios Weasley'), appearance(attribs(bold, yellow, default))]),
        createGameEntity(_, 'l', object,11, 7, inactive, norule, 
			[ name(libro), solid(false), static(false), description('Libro de Librería Obscurus'), appearance(attribs(bold, white, default))]),
        createGameEntity(_, 'h', object,15, 7, inactive, norule, 
			[ name(helado), solid(false), static(false), description('Helado de Heladería Florean Fortescue'), appearance(attribs(bold, cyan, default))]),
        %% PUERTAS
        createGameEntity(OID_P1, '-', object, 6, 3, inactive, norule, 
			[ name(puerta_ollivanders), solid(true), static(true), description('Puerta Ollivander''s'), appearance(attribs(normal, cyan, default))]),
        createGameEntity(_, '#', object, 3, 4, active, pasoCaldero, 
			[ name(puerta_banco), solid(false), static(false), description('Puerta Caldero Chorreante'), appearance(attribs(normal, cyan, default))]),
        basicDoorKey(init, OID_P1, ['pl-man':destroyGameEntity(OID_P1)], [OID_G]).

pasoCaldero(OID) :- not(entityLocation(_,5,1,'v')), destroyGameEntity(OID), 
                    msgWindowWriteln('Puerta Caldero Chorreante abierta!!!').
	
