%%
%% Experimental map to test magic Wands, with Standard Spells
%%

map_format_version(1.1).
load_behaviour(magicWand).
map(['#######',
     '# ....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '### ###',
     '#.....#',
     '#######']).
num_dots(29).
pacman_start(1, 5).

initMap:- 
	addSolidObject('#'),
	createGameEntity(_, 'P', object, 1, 4, inactive, norule, 
                [name(molesto1), solid(true), static(true), 
                appearance(attribs(normal, cyan, default)),
                description('Objeto molesto 1')]),
	createGameEntity(_, 'P', object, 3, 1, inactive, norule, 
                [name(molesto2), solid(true), static(true), 
                appearance(attribs(normal, cyan, default)),
                description('Objeto molesto 2')]),
	createGameEntity(_, '#', object, 3, 6, inactive, norule, 
                [name(magicDoor), solid(true), static(true), magic_locked(true),
                appearance(attribs(normal, cyan, default)), pushable(false),
                description('Puerta inempujable magicamente')]),
        magicWand(create(_, 1, 1, [ setStandardSpells(true) ])).
