%%
%% Experimental map to test cauldrons, in map format 1.1.
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
	createGameEntity(_, 'a', object, 1, 4, inactive, norule, 
                [name(ing1), solid(true), static(true), 
                appearance(attribs(normal, cyan, default)),
                description('Objeto molesto 1')]),
	createGameEntity(_, 'b', object, 3, 1, inactive, norule, 
                [name(ing2), solid(true), static(true), 
                appearance(attribs(normal, cyan, default)),
                description('Objeto molesto 2')]),
        magicWand(create(OID_W, 1, 1, [])),
        magicWand(newSpell(OID_W, 739538581, 1, 'pl-man':push)).

push(_, _, X, Y, [ DIR ]):-
    seeEntities(first, X, Y, DIR, L_ENT), !,
    push(moveEntities(L_ENT, DIR)).
push(moveEntities(           [],   _)) :- !.
push(moveEntities([EID | L_ENT], DIR)) :- 
    'pl-man':getDMap(M),
    'pl-man':doEntityAction(EID, M, move(DIR)), !,
    push(moveEntities(L_ENT, DIR)).
    
