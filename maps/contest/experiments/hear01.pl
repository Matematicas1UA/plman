%%
%% Experimental map to test hear/2, in map format 1.1.
%%

map_format_version(1.1).
map(['#######',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#.....#',
     '#######']).
num_dots(35).
pacman_start(1, 4).

initMap:- 
	addSolidObject('#'),
	createGameEntity(_, 'P', object, 1, 2, active, listenAndReply, 
                [name(persona), solid(false), static(true), appearance(attribs(normal, white, default)), 
                 description('Persona que te habla')]).
                 
% Checking to hear anything, printing it and replaying
listenAndReply(EID) :-
	'pl-man':hear(EID, normal, msg(NAME, TYPE, STR)), !,
	maplist(user:write, ['OIGO: (', NAME, ')(', TYPE, ')(', STR, ')\n']),
	listenAndReply(reply(EID, TYPE, STR)).

% Replaying to different messages
listenAndReply(reply(EID, NAME, hello)) :-
	'pl-man':stringListConcat(MSG, [ 'hello ', NAME, '! Nice to hear you! :)' ]),
	'pl-man':doAction(EID, say(MSG)).
listenAndReply(reply(EID, NAME, die)) :-
	'pl-man':stringListConcat(MSG, [ 'Oh My God! ', NAME, ' is killing me! :(' ]),
	'pl-man':doAction(EID, say(MSG)),
	'pl-man':destroyGameEntity(EID).

% Allways working
listenAndReply(_).
	
	 
     
