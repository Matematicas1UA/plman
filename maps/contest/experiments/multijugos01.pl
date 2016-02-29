%%
%% Experimental map to multijugos cathegory, in map format 1.1.
%%

%% #######################################
%% #         v           #               #
%% #S....................#.|===|........L#
%% #.| c g |.....|  p  |.#.|~|...........#
%% #.|=====|.....|=====|.#.|~|...........#
%% #.|=====|.....|=====|.#.|~|...........#
%% #.|  o  |.....|  b  |.#.|===|L........#
%% #@.........U..........P...............#
%% #.|  d  |.....|  s  |.#L......|=|.|=|.#
%% #.|=====|.....|=====|.#.......|~~~~~|.#
%% #.|=====|.....|=====|.#...............#
%% #.| l h |.....| a e |.#################
%% #....................H#A..............#
%% #           ^         #...............#
%% #######################################

map_format_version(1.1).
load_behaviour(cauldron).
load_behaviour(combiner).
load_behaviour(automaticArcher).
load_behaviour(enemyBasicMovement).
load_behaviour(entitySequentialMovement).
map(['#######################################',
     '#                     #               #',
     '#.....................#.|===|.........#',
     '#.|     |.....|     |.#.|~|...........#',
     '#.|=====|.....|=====|.#.|~|...........#',
     '#.|=====|.....|=====|.#.|~|...........#',
     '#.|     |.....|     |.#.|===|.........#',
     '# ......... .......... ...............#',
     '#.|     |.....|     |.#.......|=|.|=|.#',
     '#.|=====|.....|=====|.#.......|~~~~~|.#',
     '#.|=====|.....|=====|.#...............#',
     '#.|     |.....|     |.#################',
     '#.....................#...............#',
     '#                     #...............#',
     '#######################################']).
num_dots(250).
pacman_start(1, 7).
%% [i12_crisopos, onza_antimonio, i4_sanguijuelas, i16_escrupulos, i3_dracmas, i2_hojas, cuerno_Bicornio,escofinas_Salitre, piel_serpiente, gusarajo, pelo_slughorn ]
%% [ c, o, s, l, d, h, b, e, p, g, a ]
initMap:- 
        msgWindowWriteln('Solo pasan a la derecha los profesores de Slytherin.'),
        msgWindowWriteln('Transformate en uno de ellos utilizando una pocion!'),
	addSolidObject('#'),
	addSolidObject('|'),
	addSolidObject('='),
	addSolidObject('~'),
        %% Ingredientes
	createGameEntity(_, 'a', object,16,11, inactive, norule, 
          [name(pelo_slughorn), solid(true), static(false), appearance(attribs(normal, cyan, default)),
          description('Un pelo de Horace Slughorn')]),
	createGameEntity(_, 'c', object, 4, 3, inactive, norule, 
          [name(i12_crisopos), solid(true), static(false), appearance(attribs(normal, cyan, default)),
           description('12 crisopos guiados durante 21 dias')]),
        createGameEntity(_, 'p', object,17, 3, inactive, norule, 
          [name(piel_serpiente), solid(true), static(false), appearance(attribs(normal,cyan,default)),
           description('Piel seca desmenuzada de serpiente arbórea africana')]),
        createGameEntity(_, 's', object,17, 8, inactive, norule, 
          [name(i4_sanguijuelas),solid(true),static(false),appearance(attribs(normal,cyan,default)),
           description('4 sanguijuelas')]),
        createGameEntity(_, 'b', object,17, 6, inactive, norule, 
          [name(cuerno_Bicornio), solid(true), static(false), appearance(attribs(normal, cyan, default)),
           description('1 poco de cuerno pulverizado de Bicornio')]),
        createGameEntity(_, 'd', object, 5, 8, inactive, norule, 
          [name(i3_dracmas), solid(true), static(false), appearance(attribs(normal, cyan, default)),
           description('3 dracmas de Sal Amoniac')]),
        createGameEntity(_, 'h', object, 6,11, inactive, norule, 
          [name(i2_hojas), solid(true), static(false), appearance(attribs(normal, cyan, default)),
           description('2 hojas pulverizadas de centunaida')]),
        createGameEntity(_, 'l', object, 4,11, inactive, norule, 
          [name(i16_escrupulos), solid(true), static(false), appearance(attribs(normal, cyan, default)),
           description('16 escrupulos de Descurainia sophia recogida con luna llena')]),
        createGameEntity(_, 'e', object,18,11, inactive, norule, 
           [name(escofinas_Salitre), solid(true), static(false), appearance(attribs(normal, cyan, default)),
            description('Escofinas de Salitre, Mercurio y Marte')]),
        createGameEntity(_, 'o', object, 5, 6, inactive, norule, 
           [name(onza_antimonio), solid(true), static(false), appearance(attribs(normal, cyan, default)),
            description('1 onza de Antinomio crudo')]),
        createGameEntity(_, 'g', object, 6, 3, inactive, norule, 
           [name(gusarajo), solid(true), static(false), appearance(attribs(normal, cyan, default)),
            description('Gusarajo')]),
        %% Arqueros automáticos
        createGameEntity(OID_0, 'v', object, 10, 1, active, combiner([automaticArcher, enemyBasicMovement]), [name(arquero1), solid(false), static(true), use_rule(norule),
	   description('Arquero protector de ingredientes'), appearance(attribs(bold, yellow, default))]), 
	   createGameEntity(OID_1, '^', object, 12,13, active, combiner([automaticArcher, enemyBasicMovement]), [name(arquero2), solid(false), static(true), use_rule(norule),
	   description('Arquero protector de ingredientes'), appearance(attribs(bold, yellow, default))]),
	enemyBasicMovement(init, OID_0, right-left, ['#']),
	enemyBasicMovement(init, OID_1, left-right, ['#']),
	randomBetween(4,7,D1), randomBetween(4,7,D2),
   automaticArcher(init, OID_0, ['@'], down, D1, [ continuous, bullet_appearance('|', bold, red, default) ]),
   automaticArcher(init, OID_1, ['@'],   up, D2, [ continuous, bullet_appearance('|', bold, red, default) ]),
	%% Personajes Zona Izquierda: Slughorn y el Bloody Baron
	createGameEntity(EID_0, 'S', mortal, 1, 2, active, entitySequentialMovement, [appearance(attribs(bold, red, default))]), 
	createGameEntity(EID_1, 'B', active,21,12, active, entitySequentialMovement, [appearance(attribs(bold, magenta, default))]),
	entitySequentialMovement(init, EID_0, [d,d,d,d,d,d,d,d,d,d,r,r,r,r,r,r,r,r,u,u,u,u,u,u,u,u,u,u,l,l,l,l,l,l,l,l], []),
	entitySequentialMovement(init, EID_1, [u,u,u,u,u,u,u,u,u,u,l,l,l,l,l,l,l,l,d,d,d,d,d,d,d,d,d,d,r,r,r,r,r,r,r,r], []),
	%% Personajes Zona derecha: Persona que no deja pasar y enemigos Slytherins (L)
	createGameEntity(_, 'P', object,22, 7, inactive, norule, 
           [name(persona), solid(true), static(true), appearance(attribs(bold, cyan, default)),
           description('Persona que cuida el paso a zona derecha')]), 
	createGameEntity(EID_3, 'L', mortal,37, 2, active, entitySequentialMovement, [appearance(attribs(bold, red, default))]), 
	entitySequentialMovement(init, EID_3, [l,l,l,l,d,d,d,d,r,r,r,r,u,u,u,u], []),
	createGameEntity(EID_4, 'L', mortal,29, 6, active, entitySequentialMovement, [appearance(attribs(bold, red, default))]), 
	entitySequentialMovement(init, EID_4, [r,r,r,u,u,u,u,l,l,l,d,d,d,d], []),
	createGameEntity(EID_5, 'L', mortal,23, 8, active, entitySequentialMovement, [appearance(attribs(bold, red, default))]),
	entitySequentialMovement(init, EID_5, [d,d,r,r,r,r,r,r,u,u,l,l,l,l,l,l], []),
        %% Cauldron
        cauldron(create(OID_C,11, 7, [])),
        dynamicProperties(set(OID_C, static(false))),
        cauldron(newRecipe(OID_C, 1712566998, 1, [ 'pl-man':cauldronFinish(OID_C, 1) ])).
        
norule(_).
norule(_,_,_,_,_).        

