%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2014 Francisco Gallego <ronaldo@cheesetea.com>
% Departamento de Ciencia de la Computación e Inteligencia Artificial
% Universidad de Alicante
% Campus de San Vicente
% Ap.Correos, 99 
% 03080, San Vicente del Raspeig (Alicante)
%
%   This program is free software: you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation, either version 3 of the License, or
%   (at your option) any later version.
%
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%
%   You should have received a copy of the GNU General Public License
%   along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

%%
%%   MODULE: pl-man
%%  
%%  Prolog-pacman user programmed game.
%%  
:- module('pl-man', [ havingObject/0, havingObject/1, see/3, hear/2, play/2, play/3, play/4, replay/2]).
:- use_module('modules/cheeseEngine').
:- use_module('modules/cheeseText').
:- use_module('modules/cheeseTools').
:- use_module(mapManager).

%% Load Config File
%%  
:- absolute_file_name('config/envConfig.pl', [], Conf),
   access_file(Conf, read)
   -> load_files(Conf, [silent(true)])
   ;
   cls,
   system:writeln('\nUNRECOVERABLE ERROR while loading config file.'),
   system:write(Conf),
   system:writeln(' could be loaded. Please check config file and pl-man settings.\n'),
   break.

%% Load language file
%%
:- language(selected, UserLangName),
   language(default, DefLangName),
   absolute_file_name(UserLangName, 
                      [file_type(prolog), relative_to('pl-man-game/lang/')], 
                      UserLang),
   absolute_file_name(DefLangName, 
                      [file_type(prolog), relative_to('pl-man-game/lang/')], 
                      DefLang),
   ( access_file(UserLang, read) 
     -> load_files(UserLang, [silent(true)])
     ; ( access_file(DefLang, read) 
         -> load_files(DefLang, [silent(true)])
         ;
         cls,
     system:writeln('\nUNRECOVERABLE ERROR while loading language file.'),
     system:write('Neither '), system:write(UserLang),
     system:write(' nor '), system:write(DefLang),
     system:writeln(' could be loaded. Please check language settings.\n'),
     break
       )
   ), !
   ;
   cls,
   system:writeln('\nUNRECOVERABLE ERROR while searching for language files.'),
   system:writeln('There was an error accessing the facts that state the language files to load'),
   system:write('Please check that language(selected, \'YourLang.pl\'). '),
   system:writeln('and language(default, \'defLang.pl\'). are correctly loaded into the database.'),
   break.

%% Dynamic Predicates
%% 
:- dynamic d_object/2.         % Object an entity has got
:- dynamic d_doAction/1.       % Next action to be done by pacman
:- dynamic d_doAction/2.       % Next action to be done by a generic entity
:- dynamic d_playerStatsPLID/1.% Parameter List ID for player stats
:- dynamic d_messageCounter/1. % Counter of system messages printed out
:- dynamic d_logging/1.        % Flag to set on when logging is activated (and holding the LOG_ID)
:- dynamic d_replay/2.         % Parameters for replay (STEP_SIZE, MAX_CYCLE) 
:- dynamic d_drawing/0.        % Flag to set on when visualization is activated
:- dynamic d_EIDConverter/2.   % Table to associate Old EIDs of a log for real EIDs of the replay being played

% Sounds and words produced/pronounced by objects or entities
%  d_sound(EID, DPSID, location(X,Y))  EID = producer Entity ID, DPSID = Dinamic Properties Sound ID.
%  Dinamic Properties: string(STR), strength(S)
:- dynamic d_sound/3.

% Dynamic entities of the game
%   d_entity(Eid, Type, Status, ControlRule, Location)
%  
:- dynamic d_entity/5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PLAYER STATS CONTROL
%
% playerStats(OPERATION)
%   Realizes an operation to the statistics of the player
%
% OPERATIONS:
% * add(PROP_NAME, C))
%      - PROP_NAME: Name of the property to modify
%      - ADD: Quantity to be added to the present value of the property
%    Adds C to the given property
%
% * get(PROP) // get(PROP_NAME, V)
% * set(PROP) // set(PROP_NAME, V)
%      - PROP: Property to get/set (i.e. a functor of arity 1)
%      - PROP_NAME: Name of the property to get/set
%      - V: Value of the property
%    Gets/Sets the value of the property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
playerStats(reset):-
    retractall(d_playerStatsPLID(_)),
    dynamicProperties(create(PLID, 
        [movements(0), collisions(0), bad_attempts(0), fails(0), max_movements(-1),
        unknown_actions(0), remaining_dots(0), game_status(running)])),
    assert(d_playerStatsPLID(PLID)).
playerStats(add(PROP_NAME, C)):-
    d_playerStatsPLID(PLID),
    dynamicProperties(get(PLID, PROP_NAME, VAL)),
    NewVAL is VAL + C,
    dynamicProperties(set(PLID, PROP_NAME, NewVAL)), !.
playerStats(get(PROP)):-
    d_playerStatsPLID(PLID),
    dynamicProperties(get(PLID, PROP)).
playerStats(get(PROP_NAME, V)):-
    d_playerStatsPLID(PLID),
    dynamicProperties(get(PLID, PROP_NAME, V)).
playerStats(set(PROP)):-
    d_playerStatsPLID(PLID),
    dynamicProperties(set(PLID, PROP)).
playerStats(set(PROP_NAME, V)):-
    d_playerStatsPLID(PLID),
    dynamicProperties(set(PLID, PROP_NAME, V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN LOG CONTROL
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mainLog(create(FILE)):-
    retractall(d_logging(_)),
    logToFile(reset), 
    logToFile(create(ID, FILE)), 
    assert(d_logging(ID)), !.
mainLog(_):-
    not(d_logging(_)), !.
mainLog(append_raw(STR)):-
    d_logging(ID),
    logToFile(append(ID, STR)), !.
mainLog(append_c(STR)):-
    d_logging(ID),
    playerStats(get(movements(MO))),
    with_output_to(string(S), maplist(system:write, [ 'c(',MO,', ', STR ,').\n' ] )),
    logToFile(append(ID, S)), !.
mainLog(flush):-
    d_logging(ID),
    logToFile(flush(ID)), !.
mainLog(close):-
    d_logging(ID),
    logToFile(close(ID)), !.

% Logging entity movements
mainLog(moveEntity(EID, X, Y)):-
    mainLog(append_c( me(EID, X, Y) )), !.
% Logging objects that are got by entities
mainLog(objectGot(EID, OID)):-
    swritef(SS, '%q', [ go(EID, OID) ]),
    mainLog(append_c( SS )), !.
% Logging objects that are dropped by entities
mainLog(objectDropped(EID, OID, X, Y)):-
    swritef(SS, '%q', [ do(EID, OID, X, Y) ]),
    mainLog(append_c( SS )), !.
% Logging creation of game entities
mainLog(createGameEntity(EID, Type, location(X,Y,AP), Data)):-
    ( member(appearance(attribs(Atr, FC, BC)), Data) ; Atr=normal, FC=default, BC=default ),
    swritef(SAP, '%q', [ AP ]),
    mainLog(append_c( cge(EID, Type, l(X, Y), ap(SAP, Atr, FC, BC)) )), !.
% Logging changing entity complex appearances
mainLog(changeEntityAppearance(EID, appearance(NewAP, NewAtr, NewTC, NewBC))):-
    swritef(SAP, '%q', [ NewAP  ]),
    swritef(SAT, '%q', [ NewAtr ]),
    swritef(STC, '%q', [ NewTC  ]),
    swritef(SBC, '%q', [ NewBC  ]),
    mainLog(append_c( cea(EID, SAP, SAT, STC, SBC) )), !.
% Logging changing entity simple appearances
mainLog(changeEntityAppearance(EID, NewAp)):-
    swritef(SAP, '%q', [ NewAp ]),
    mainLog(append_c( cea(EID, SAP) )), !.
% Logging initial state
mainLog(initialState):-
    getDMap(M),
    swritef(SS, '%q', [ map(M) ]),
    mainLog(append_c( SS )), !,
    forall( d_entity(EID, Type, _, _, location(X, Y, AP)), (
         ( dynamicProperties(get(EID, appearance(APP))) 
         ; APP=attribs(normal, default, default)),
         mainLog(createGameEntity(EID, Type, location(X,Y,AP), [ appearance(APP) ]))
        )), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% havingObject
%   Suceeds if pacman has got any object
% 
% havingObject(OID)
%   Succeeds if pacman has got an object OID
%
% havingObject(appearance(AP))
%   Succeeds if pacman has got an object with appearance AP
%
% havingObject(name(NA))
%   Succeeds if pacman has got an object whose name is NA
%
% havingObject(EID, OID)
%   Succeeds if EID has got object OID
%
% havingObject(EID, appearance(AP))
%   Succeeds if EID has got an object with appearance AP
%
% havingObject(EID, name(NA))
%   Succeeds if EID has got an object whose name is NA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
havingObject:-
    entityType(EID, pacman),
    havingObject(EID, _).
havingObject(COMMAND):-
    entityType(EID, pacman),
    havingObject(EID, COMMAND).
havingObject(EID, appearance(AP)):-
    d_object(EID, d_entity(_, _, _, _, location(_,_,AP))), !.
havingObject(EID, name(NA)):-
    d_object(EID, d_entity(OID, _, _, _, _)), 
    dynamicProperties(get(OID, name, NA)), !.
havingObject(EID, OID):-
    d_object(EID, d_entity(OID, _, _, _, _)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doAction(Act)
%  A: Action to do
%  
%  Changes the action that pacman will perform in the next
%  timestep. This rule is intended for users.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
doAction(say(STR)):- !, 
    entityType(PacID, pacman), 
    manageSounds(removeAll(PacID)),
    doAction(PacID, say(STR)).

doAction(Act):-
    retractall(d_doAction(_)),
    assert(d_doAction(Act)).

doAction(EID, say(STR)):- !,
    manageSounds(create(EID, STR, 3)),
    entityName(EID, NAME),
    stringListConcat(S, [NAME, ': ', STR]),
    showSystemMsg(user, normal, sound, S, []).

doAction(EID, Act):-
    assert(d_doAction(EID, Act)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loadNewMap(File)
%   File: Map definition prolog file
% loadNewMap(version, V)
%   V: Map definition file format version
% 
%  Loads a prolog file containing a map definition in prolog
%  and obtains all the data related to the map, acording to  
%  its version number.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private rule to load behaviour libraries
:- multifile lang_message/3.
p_loadBehaviours_loadLangFile(PATH, FILENAME) :- 
        absolute_file_name(FILENAME, 
                           [file_type(prolog), relative_to(PATH)],
                           FILE),
        access_file(FILE, read),
        load_files(FILE, [silent(true)]).
p_loadBehaviours_loadLanguage(BLIBPATH) :-
        language(selected, SELLANG),
        language(default, DEFLANG),
        atom_concat(BLIBPATH, '/lang/', LANGPATH),
        (
            p_loadBehaviours_loadLangFile(LANGPATH, SELLANG), !
        ;
            p_loadBehaviours_loadLangFile(LANGPATH, DEFLANG)
        ), !.
p_loadBehaviours_loadLanguage(BLIBPATH) :-
    showSystemMsg(system, error, error_loading_behaviour_language_file, BLIBPATH, [halt(1)]).

p_loadBehaviours:-
    not(clause(load_behaviour(_), true)), !.
p_loadBehaviours:-
    forall( load_behaviour(BLIB), loadNewBehaviour(BLIB)), !.
p_loadBehaviours:-
    showSystemMsg(system, error, error_loading_behaviours, '', [halt(1)]).
loadNewBehaviour(BLIB):-
        atom_concat('pl-man-game/blib/', BLIB, BLIBPATH),
        p_loadBehaviours_loadLanguage(BLIBPATH), 
        absolute_file_name('main',
        [file_type(prolog), relative_to(BLIBPATH)],
        FILE),
    catch(use_module(FILE), _, fail).

loadNewMap(version, 1.0):-
    map(M), 
    loadNewMap(initialize, M).
loadNewMap(version, 1.1):-
    map(M), 
    atomMap2charMap(M, MCHAR),
    loadNewMap(initialize, MCHAR).  
loadNewMap(initialize, MAP):-
    startNewMap(MAP),
    num_dots(Dots),
    playerStats(set(remaining_dots(Dots))),
    pacman_start(PX, PY),
    entityType(PacID, pacman),
    moveEntity(PacID, MAP, PX, PY),
    p_loadBehaviours,
    initMap, !.
    
loadNewMap(File):-
    catch(load_files(File, [silent(true)]),_, fail),
        clause(map(_), true),       % If file loads, map/1 should then exist
    (not(clause(map_format_version(_), true)) 
     -> V = 1.0
     ;  map_format_version(V)
    ),
    loadNewMap(version, V), !.
loadNewMap(File):-
    showSystemMsg(system, error, map_file_error, File, [halt(1)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% showSystemMsg(SCOPE, Type, msgID, ADDEDTXT, L_ACTIONS)
%    SCOPE: Scope for the execution of write command (system, user, 'pl-man'...)
%    TYPE:  Message type (warning, error...)
%    MSGID: Identifier of the message (normally an english atom)
%    ADDEDTXT: Text to append at the end of the message
%    L_ACTIONS: List of callable actions to do
%
%  Shows a message from the system to the user. This messages
% are kept on lang databases for appropiate translation. 
% Once the message is shown, it calls one by one all the actions
% given on the L_ACTIONS list, so you are able to abort the 
% program or start a trace section if you like.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
showSystemMsg(SCOPE, TYPE, MSGID, ADDEDTXT, L_ACTIONS):-
    retract(d_messageCounter(N)),
    N1 is N+1,
    assert(d_messageCounter(N1)),
    lang_message(TYPE, TypeMSG),
    lang_message(MSGID, TextMSG),
    maplist(SCOPE:write, [N, ' ', TypeMSG, ': ', TextMSG]),
    ((ADDEDTXT \= '')
    -> maplist(SCOPE:write, [' [', ADDEDTXT, ']\n'])
     ; SCOPE:nl
    ),
    maplist(call, L_ACTIONS), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eatDot(EID, Map, X, Y)   
% moveEntity(EID, Map, X, Y)
%   EID: Entity identifier
%   Map: Actual 2D Map of the world 
%   X, Y: New place where to put the entity
%
%   Moves an entity to a new place in the world, regardless
%   the things that could be in that place.
%   
%   If the entity moving is pacman, it can eat a new dot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pacman has eaten a dot, and we have to take it into account
dotEaten:-
    playerStats(add(remaining_dots, -1)),
    playerStats(get(remaining_dots(RD))),
    (RD =< 0 
    -> playerStats(set(game_status(finished)))
    ; true), !.

% If the entity moving is pacman, it can eat dots
eatDot(EID, Map, X, Y):-
    entityType(EID, pacman),
    getCellContent(X, Y, Map, '.'),
    updateCellContent(X, Y, ' ', Map, NewMap),
    updateDMap(NewMap), 
    mainLog(append_c( um(X, Y, '\' \'') )),
    dotEaten, !.
eatDot(_, _, _, _).

moveEntity(EID, Map, X, Y):-
    eatDot(EID, Map, X, Y),
    retract(d_entity(EID, Type, Status, ControlRule, location(_,_,Ap))),
    assert(d_entity(EID, Type, Status, ControlRule, location(X,Y,Ap))),
    mainLog(moveEntity(EID, X, Y)).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getSeeList(TYPE, X, Y, CONTENT, ADD, WHAT)
%   TYPE:    column/row -> Trying to see in the same column or row
%   X,Y:     Actual location in the map where to see
%   CONTENT: Map/Row where to see
%   ADD:     +1/-1 to set direction to continue seeing
%   WHAT:    List containing what you see
%
%   Calculates and returns what you are seeing depending on
%   where you are looking (in a row or in a column) and in
%   what direction you look.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getSeeList(_, X, Y, _, _, [ W ]):-
    entityLocation(EID, X, Y, W),
    solidEntity(EID), !.
getSeeList(row, X, _, Row, _, [W]):-
    nth0(X, Row, W),
    isSolid(W), !.
getSeeList(column, X, Y, Map, _, [W]):-
    getCellContent(X, Y, Map, W),
    isSolid(W), !.
getSeeList(row, X, Y, Row, Add, [H|T]):-
    entityLocation(EID, X, Y, H), H \= '',
        not(solidEntity(EID)),
    X1 is X + Add,
    getSeeList(row, X1, Y, Row, Add, T), !.
getSeeList(column, X, Y, Map, Add, [H|T]):-
    entityLocation(EID, X, Y, H), H \= '',
        not(solidEntity(EID)),
    Y1 is Y + Add,
    getSeeList(column, X, Y1, Map, Add, T), !.
getSeeList(row, X, Y, Row, Add, [H|T]):-
        not(entityLocation(_, X, Y, _)),
    nth0(X, Row, H),
        not(isSolid(H)),
    X1 is X + Add,
    getSeeList(row, X1, Y, Row, Add, T).
getSeeList(column, X, Y, Map, Add, [H|T]):-
        not(entityLocation(_, X, Y, _)),
    getCellContent(X, Y, Map, H),
        not(isSolid(H)),
    Y1 is Y + Add,
    getSeeList(column, X, Y1, Map, Add, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% see(EID, MODE, DIR, -WHAT)
% see(MODE, DIR, -WHAT)
%   EID:  Entity identifier.
%   MODE: looking mode (normal, list)
%   DIR:  Direction where to see something
%   WHAT: What you can see in the direction specified
%
%   Looks at pacman's' surroundings and constructs an atom
%   or a list of what pacman is able to see in the specified
%   direction.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add quantities depending on seeing direction
xy_seedir(here, 0, 0).
xy_seedir(up, 0, -1).
xy_seedir(left, -1, 0).
xy_seedir(right, 1, 0).
xy_seedir(down, 0, 1).
xy_seedir(down-left, -1, 1).
xy_seedir(down-right, 1, 1).
xy_seedir(up-left, -1, -1).
xy_seedir(up-right, 1, -1).
xy_seedir(left-down, X, Y):- xy_seedir(down-left, X, Y).
xy_seedir(left-up, X, Y):- xy_seedir(up-left, X, Y).
xy_seedir(right-down, X, Y):- xy_seedir(down-right, X, Y).
xy_seedir(right-up, X, Y):- xy_seedir(up-right, X, Y).

% You can see entities or map things, but not both
entityYouCanSee(EID, X, Y, W):-
    entityLocation(OEID, X, Y, W), 
    EID \= OEID,
    not(isObjectGot(OEID)),
    isInteractive(OEID).
whatYouSee(EID, _, X, Y, W):-
    entityYouCanSee(EID, X, Y, W).
whatYouSee(EID, Map, X, Y, W):-
    not(entityYouCanSee(EID, X, Y, _)),
    getCellContent(X, Y, Map, W).

see(MODE, DIR, WHAT):-
    entityType(EID, pacman),
    see(EID, MODE, DIR, WHAT).
see(EID, MODE, DIR, WHAT):-
    entityLocation(EID, X, Y, _),
    getDMap(Map),
    see(EID, MODE, DIR, Map, X, Y, WHAT).

see(EID, normal, Dir, Map, X, Y, W):-
    xy_seedir(Dir, AddX, AddY),
    X1 is X + AddX, 
    Y1 is Y + AddY,
    whatYouSee(EID, Map, X1, Y1, W).

see(  _, list, DIR, Map, X, Y, W):-
    (DIR = left; DIR = right), 
    nth0(Y, Map, Row),
    xy_seedir(DIR, Add, _),
    X1 is X+Add,
    getSeeList(row, X1, Y, Row, Add, W).
see(  _, list, DIR, Map, X, Y, W):-
    (DIR = up; DIR = down), 
    xy_seedir(DIR, _, Add),
    Y1 is Y+Add,
    getSeeList(column, X, Y1, Map, Add, W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% seeEntities(first, X, Y, DIR, SEELIST)
%
% returns a list of entities that we can see from here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seeEntities(first, X, Y,   _,      []) :-
    not(isInsideMap(X,Y)), !.
seeEntities(first, X, Y,   _, SEELIST) :-
    findall(EID, entityLocation(EID, X, Y, _), SEELIST),
    SEELIST \= [], !.
seeEntities(first, X, Y,   _,      []) :-
    collision(X, Y), !.
seeEntities(first, X, Y, DIR, SEELIST) :-
    member(DIR, [up, down, left, right]), !,
    xy_seedir(DIR, XADD, YADD), 
    X1 is X + XADD, Y1 is Y + YADD, !,
    seeEntities(first, X1, Y1, DIR, SEELIST).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% manageSounds(ACTION)
%   ACTION: Action to do managing sounds. Possible actions:
%       remove(DSID): Removes sound with DSID id
%       removeAll(EID): Removes all sounds produced by a given entity
%       create(EID, STR, S): Creates a new sound STR, produced by EID
%                       and with Strength S.
%
% Manages the sounds produced by entities in the system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manageSounds(create(EID, STR, STRENGTH)):-
    entityLocation(EID, X, Y, _),
    dynamicProperties(create(PLID, [string(STR), strength(STRENGTH)])),
    assert(d_sound(EID, PLID, location(X,Y))).
manageSounds(remove(DSID)):-
    d_sound(_, DSID, _),
    dynamicProperties(destroy(DSID)),
    retract(d_sound(_, DSID, _)).
manageSounds(removeAll(EID)):-
    forall( d_sound(EID, DSID, _), 
            manageSounds(remove(DSID))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hear(normal, MSG)
% hear(list, MSGLIST)
%   MSG:        Individual sound nearby
%   MSGLIST:    List of messages that nearby entities are producing 
%
%   Listens to verbal messages or sounds produced by nearby
%   entities. Sounds are only possible to be heard if we are
%   near the entity that produced them (depending on the 
%   strength of the sound).   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hear(TYPE, MSG):-
    entityType(PacID, pacman),
    hear(PacID, TYPE, MSG).

hear(EID, list, MSGLIST) :-
    integer(EID),
    entityLocation(EID, MX, MY, _), !,
    findall(msg(NAME, TYPE, STR), 
            hear(internal, _, getSurroundingSound(MX, MY, NAME, TYPE, STR)), 
            MSGLIST).

hear(EID, normal, msg(NAME, TYPE, STR)) :-
    entityLocation(EID, X, Y, _),
    hear(internal, _, getSurroundingSound(X, Y, NAME, TYPE, STR)). 

hear(internal, EID, getSurroundingSound(X, Y, NAME, TYPE, STR)):-
    d_sound(EID, DSID, location(SX, SY)),
    dynamicProperties(get(DSID, strength(S))),
    manhattanDistance(p(X, Y), p(SX, SY), D), D =< S,
    entityName(EID, NAME),
    entityType(EID, TYPE),
    dynamicProperties(get(DSID, string(STR))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkPacmanDeath(X, Y)
%   X,Y: Pacman's location
%
%   Checks if pacman has collided with a mortal object, which
%   would represent pacman's death
%   
% checkPacmanDeath(EID)
%   EID: Entity supposely killing pacman
%   
%   Checks if the entity EID is killing pacman (i.e. is a
%   mortal entity and it is in the same place as pacman)
%   Note: If EID is not killing pacman, this rule simply fails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkPacmanDeath(X, Y):-
    entityType(EID2, mortal), 
    entityLocation(EID2, X, Y, _),
    playerStats(set(game_status(finished))), !.
checkPacmanDeath(_, _).
checkPacmanDeath(_):- playerStats(get(game_status(finished))), !.
checkPacmanDeath(EID):-
    entityType(PacID, pacman),
    entityLocation(PacID, X, Y, _), 
    entityType(EID, mortal),
    entityLocation(EID, X, Y, _),
    playerStats(set(game_status(finished))), !.
checkPacmanDeath(_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCollisionsAndUpdate(EID, Map, X, Y)
%   EID: Entity identifier
%   Map: Actual 2D map of the world
%   X, Y: New coordinates of the Entity
%   
%   Checks if it is possible for the entity to move on to
%   the location on X,Y. If possible, it moves the entity.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCollisionsAndUpdate(EID, Map, X, Y):-
    collision(X, Y, Map),
    p_collisionCountAndReport(EID), !. 
checkCollisionsAndUpdate(EID, _, X, Y):-
    entityLocation(EID2, X, Y, _), 
    EID \= EID2, 
    not(isObjectGot(EID2)),
    (solidEntity(EID2); solidEntity(EID)),
    p_collisionCountAndReport(EID), !.
checkCollisionsAndUpdate(EID, Map, X, Y):-
    moveEntity(EID, Map, X, Y), !.

% Subrule to be used when a collision occurs to 
% count it and report to the user
p_collisionCountAndReport(EID):-
    entityType(EID, pacman),
    playerStats(add(collisions, 1)),
    showSystemMsg(user, warning, movement_collision, '', []), !.
p_collisionCountAndReport(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getObjectFrom(EID, X, Y)
%   EID: Entity which tries to get the object
%   X, Y: Coordinates where entity tries to get it from
%   
%   An entity gets the object which is lying on X, Y if possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getObjectFrom(EID, X, Y):-
    not((entityLocation(ObjID, X, Y, _),
         entityType(ObjID, object))),
    p_errorWithObject(EID, no_object_found), !.
getObjectFrom(EID, _, _):-
    d_object(EID, _),
    p_errorWithObject(EID, you_already_have_object), !.
getObjectFrom(EID, X, Y):-
    entityLocation(ObjID, X, Y, _),
    staticObject(ObjID),
    p_errorWithObject(EID, object_is_static, ObjID), !.
getObjectFrom(EID, X, Y):-
    ENTY = d_entity(OID, object, _, _, location(X,Y,Ap)),
    ENTY, assert(d_object(EID, ENTY)),
    retract(ENTY), 
    (not(entityType(EID, pacman))
    -> true
     ; dynamicProperties(get(OID, name, ObjName)),
       dynamicProperties(get(OID, description, ObjDesc)),
       stringListConcat(Str, [ObjName, '(', Ap, ') ', ObjDesc]),
       string_to_atom(Str, SS),
       showSystemMsg(user, normal, object_got, SS, []),
       mainLog(objectGot(EID, OID))
    ), !.

% Subrule to be used when an error ocurs in trying to get,drop or use 
% an object. This rule counts the bad attempt and reports
% to the user if necessary
p_errorWithObject(EID, ERRORTYPE, OID):-
    entityType(EID, pacman),
    dynamicProperties(get(OID, name(NAME))),    
    dynamicProperties(get(OID, description(DES))),  
    stringListConcat(TXT, [NAME, ': ', DES]),
    showSystemMsg(user, warning, ERRORTYPE, TXT, []),
    playerStats(add(bad_attempts, 1)), !.
p_errorWithObject(EID, ERRORTYPE):-
    entityType(EID, pacman),
    showSystemMsg(user, warning, ERRORTYPE, '', []),
    playerStats(add(bad_attempts, 1)), !.
p_errorWithObject(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dropObjectTo(EID, Map, X, Y)
%   EID: Entity which tries to drop its object
%   X, Y: Coordinates where entity tries to drop it to
%   
%   An entity drops the object that is carrying to a location
%   in the map if possible.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dropObjectTo(EID, Map, X, Y):-
    (collision(X, Y, Map); 
     entityLocation(EID, X, Y, _), isInteractive(EID)),
    p_errorWithObject(EID, impossible_drop_object), !.
dropObjectTo(EID, _, _, _):-
    not(d_object(EID, _)),
    p_errorWithObject(EID, not_carrying_object), !.
dropObjectTo(EID, _, X, Y):-
    d_object(EID, OBJ), 
    OBJ = d_entity(ObjID, object, Status, Rule, location(_,_,Ap)),
    assert(d_entity(ObjID, object, Status, Rule, location(X,Y,Ap))),
    retract(d_object(EID, OBJ)),
    (not(entityType(EID, pacman))
    -> true
     ; showSystemMsg(user, normal, object_dropped, Ap, []),
       mainLog(objectDropped(EID, ObjID, X, Y))
    ), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% useObjectTo(EID, DIR, X, Y)
%   EID: Entity which tries to use the object
%   DIR: Direction of use (left, right, up, down)
%   X, Y: Coordinates where entity tries to use it 
%   
%   An entity tries to use an object in a place where it 
%   could be another object or nothing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
useObjectTo(EID, _, _, _):-
    not(d_object(EID, _)),
    p_errorWithObject(EID, no_object_to_use), !.
useObjectTo(EID, DIR, X, Y):-
    d_object(EID, d_entity(ObjId, object, _, _, _)),
    dynamicProperties(get(ObjId, use_rule, ObjRule)),
    call(ObjRule, ObjId, EID, X, Y, DIR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doEntityAction(EID, Map, Action)
%   EID: Entity Identifier
%   Map: Actual 2D map of the world
%   Action: Action to be done by the entity
%   
%   Makes the necessary calculations to do the action that
%   the entity wants to do.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Offset coordinates depending on direction selected
p_directionList([d(left, -1, 0), d(right, +1, 0), 
        d(down, 0, +1), d(up, 0, -1), d(none, 0, 0)]).
% Calculate new (X,Y) coordinates depending on direction selected
p_calculateNewXY(DIR, X, Y, NewX, NewY):-
    p_directionList(DL),
    member(d(DIR, AddX, AddY), DL),
    NewX is X + AddX, NewY is Y + AddY.
% Print a message for different attempts of actions to do
p_printMessageDoAction(MsgType, DIR):-
    lang_message(DIR, DIRMSG),
    showSystemMsg(user, normal, MsgType, DIRMSG, []).
% doEntityAction Clauses
doEntityAction(EID, Map, unrestricted_move(DIR)):-
    not(entityType(EID, pacman)),
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
    moveEntity(EID, Map, X1, Y1), !.
doEntityAction(EID, Map, move(DIR)):-
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
    checkCollisionsAndUpdate(EID, Map, X1, Y1).
doEntityAction(EID, _, get(DIR)):-
    p_printMessageDoAction(getting_object, DIR),
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
    getObjectFrom(EID, X1, Y1), !.
doEntityAction(EID, Map, drop(DIR)):-
    p_printMessageDoAction(dropping_object, DIR),
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
    dropObjectTo(EID, Map, X1, Y1), !.
doEntityAction(EID, _, use(DIR)):-
    p_printMessageDoAction(using_object, DIR),
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
    useObjectTo(EID, DIR, X1, Y1), !.
doEntityAction(EID, _, use(PRED, DIR)):-
    p_printMessageDoAction(using_object, DIR),
    entityLocation(EID, X, Y, _),
    p_calculateNewXY(DIR, X, Y, X1, Y1),
        functor(FNC, PRED, 1), arg(1, FNC, DIR),
    useObjectTo(EID, FNC, X1, Y1), !.
doEntityAction(_, _, endGame):-
    playerStats(set(game_status(finished))), !.
doEntityAction(_, _, Action):- 
    playerStats(add(unknown_actions, 1)),
    with_output_to(string(StrAct), system:write(Action)),
    showSystemMsg(user, warning, invalid_action, StrAct, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update
%  Actualización del mundo. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updateWorld:- 
    entityType(PacID, pacman),

    %%% Convert pacman actions to other entities format
    forall(d_doAction(Action), (retractall(d_doAction(Action)), asserta(d_doAction(PacID, Action)))),

    %%% Para cada acción que se haya solicitado, hacemos el movimiento 
    forall( d_doAction(EID, Action),
        (
            retractall(d_doAction(EID, Action)),
            checkPacmanDeath(EID),
            (  
                entityLocation(EID, _, _, _),
                not(playerStats(get(game_status(finished)))),
                getDMap(Map),
                doEntityAction(EID, Map, Action), 
                checkPacmanDeath(EID)
                ; 
                true
            ),!
        )
    ),
    % Update movement counter
    playerStats(add(movements, 1)),
    % Check if pacman is still alive
    entityLocation(PacID, X, Y, _),
    checkPacmanDeath(X, Y).

updateEntities:-
    entityType(PacID, pacman),

        % Remove sounds produced by pacman
        manageSounds(removeAll(PacID)),

        %%% Update PacMan
    %d_entity(_,pacman,_,PacRule,_),
    ignore(call_cleanup(do(Action), State, alertState(State, Action))), !,

        %%% Remove sounds not produced by pacman
        forall( ( d_sound(EID, _, _), EID \= PacID ),
                manageSounds(removeAll(EID)) ),

        %%% Update Other Entities
    forall( ( d_entity(EID,Type,active,BotRule,_)
        ; d_object(_, d_entity(EID,Type,active,BotRule,_)) ),
           (   
             Type \= pacman ->
         ignore(call(BotRule, EID)), ! 
             ; true
           )
          ).

drawWorld:-
    d_drawing,
    cls,
    getDMap(M),
    entityType(PacID, pacman),
    entityLocation(PacID, X1, Y1, Ap),
    properties(map,MapW,MapH,MapX,MapY,MapFrColor),
    properties(msgWindow,_,_,MsgX,MsgY,MsgFrColor),
    drawFrame(MapX,MapY,MapW,MapH,MapFrColor), 
    drawMap(MapX,MapY,X1,Y1,MapW,MapH,M,Ap), 
    getMsgWindowSize(MsgW,MsgH), 
    drawFrame(MsgX,MsgY,MsgW,MsgH,MsgFrColor), 
    drawMsgWindow(MsgX,MsgY),
    forall(d_entity(EID,Type,_,_,LOCATION),
           p_drawEntity(EID, Type, MapX,MapY,LOCATION)
          ),
    setCursor(1,1),
    flush_output, !.
drawWorld:-
    not(d_drawing).

% Draw entities taking into account their attribs (and do not draw pacman!)
p_drawEntity(_, pacman, _, _, _):- !.
p_drawEntity(EID, _, MapX, MapY, location(EntX,EntY,EntAp)):-
    dynamicProperties(get(EID, appearance(attribs(Atr, FC, BC)))),
    drawEntity(MapX, MapY, EntX, EntY, appearance(Atr, FC, BC, EntAp)), !. 
p_drawEntity(_, _, MapX, MapY, location(EntX,EntY,EntAp)):-
    drawEntity(MapX, MapY, EntX, EntY, EntAp), !. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gameInit(BotRule)
%   MapFile: Archivo de definición del mapa a jugar
%    
%   Initializes the environment setting everything to its
%   initial state.
%
% p_checkInitStatus
%   Checks initial status conditions to ensure that 
%   everything works as expected
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p_checkInitStatus :-
    not( current_predicate(do/1) )
  , showSystemMsg(system, error, error_not_do_predicate_defined, '', [ halt(1) ]).
p_checkInitStatus.

gameInit(MapFile):-
      dynamicProperties(reset)
    , retractall(d_drawing)
    , retractall(d_logging)
    , retractall(d_object(_, _))
    , retractall(d_entity(_,_,_,_,_))
    , retractall(d_messageCounter(_))
    , retractall(d_doAction(_))
    , retractall(d_doAction(_,_))
    , assert(d_messageCounter(1))
    , properties(msgWindow,W,H,_,_,_)
    , clearMsgWindow
    , setMsgWindowSize(W,H)
    , createGameEntity('@', pacman, 1, 1, active, do, 0)
    , playerStats(reset)
    , p_checkInitStatus
    , loadNewMap(MapFile)
    .
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alertState(St, Act).
%   St: estado (exit, fail, !)
%   Act: Acción que PLMan debe hacer en caso de éxito
%   
%   Escribe un mensaje de alerta en la ventana de mensajes
%   en caso de que la regla del usuario haya terminado en
%   fracaso.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
alertState(!, Act)      :- alertState(exit, Act).
alertState(exit, Act)   :- doAction(Act).
alertState(fail,   _)   :-
    playerStats(add(fails, 1)),
    showSystemMsg(user, warning, botrule_fail, '', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dropObjectIfEntityHas(EID)
%   EID: Entity Identifier
%   
%   If the entity with EID identifier has got an object, 
%   the object is dropped in the same location the entity is.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dropObjectIfEntityHas(EID):-
    not(d_object(EID, _)), !.
dropObjectIfEntityHas(EID):-
    d_entity(EID, _, _, _, location(X, Y, _)),
    retract(d_object(EID, d_entity(ObjID, ObjT, ObjA, ObjCR, location(_,_,ObjAp)))),
    assert(d_entity(ObjID, ObjT, ObjA, ObjCR, location(X,Y,ObjAp))),
    mainLog(objectDropped(EID, ObjID, X, Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createGameEntity(-EID, +Ap, +Type, +X, +Y, +Active, +ControlRule, +Data).
%   EID: Free variable to hold the assigned entity identifier.
%    This parameter is optional.
%   Ap: Appearance
%   Type: type of the entity to be created
%   X, Y: Location 2D coordinates. They could be numbers or generators:
%   rnd(Z, T): Random integer number in [Z, T]
%   rnd(LIST): Random integer picked up from LIST of numbers
%   Active: true if it is in proactive state
%   ControlRule: Rule to be called each timestep
%   Data: Structure containing related data about the entity.
%   This parameter may be a list of properties and 
%   values of the entity.
%   
%   This rule creates entities of the game, assigning them
%   a unique entity-identifier (EID). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createGameEntity(Ap, Type, X, Y, Active, ControlRule, Data):-
    createGameEntity(_, Ap, Type, X, Y, Active, ControlRule, Data), !.
createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, Data):-
    (not(integer(X)) ; not(integer(Y))), !,
    ( X = rnd(FX, TX) -> randomBetween(FX, TX, C_X) 
    ; X = rnd(LX), is_list(LX) -> randomFromList(C_X, LX)
    ; C_X = X ), !,
    ( Y = rnd(FY, TY) -> randomBetween(FY, TY, C_Y) 
    ; Y = rnd(LY), is_list(LY) -> randomFromList(C_Y, LY)
    ; C_Y = Y ), !,
    createGameEntity(EID, Ap, Type, C_X, C_Y, Active, ControlRule, Data), !.
createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, 0):-
    createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, []), !.
createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, data(NAME, SOLID, STATIC, USE, DESCRIP)):-
    (SOLID=solid -> N_SOLID=true; N_SOLID=false),
    (STATIC=static -> N_STATIC=true; N_STATIC=false),
    createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, 
        [name(NAME), solid(N_SOLID), static(N_STATIC), 
        use_rule(USE), description(DESCRIP)]), !.
createGameEntity(EID, Ap, Type, X, Y, Active, ControlRule, Data):-
    is_list(Data), 
    var(EID), 
    number(X), number(Y),
    dynamicProperties(create(EID, Data)),
    assert(d_entity(EID, Type, Active, ControlRule, location(X,Y,Ap))),
    mainLog(createGameEntity(EID, Type, location(X,Y,Ap), Data)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% destroyGameEntity(EID)
%   EID: Entity Identifier
%   
%   The entity with the identifier EID is destroyed, dropping
%   an object if the entity had one.
%
% destroyEverythingAt(X, Y, except(L_EXC))
%   X, Y: Coordinates where to destroy
%   L_EXC: List of atoms appearances of objects that should
%          not be destroyed
%
%   Destroys entities, objects, solids and other things
%  that it founds located at X,Y, except thouse whose 
%  appearance can be found in the L_EXC list.
%  NOTE: It will not destroy anything located at the edge
%  of the map.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroyGameEntity(EID):-
    dropObjectIfEntityHas(EID),
    retractall(d_object(_, d_entity(EID,_,_,_,_))),
    retractall(d_doAction(EID, _)),
    retractall(d_entity(EID, _, _, _, _)),
    mainLog(append_c( de(EID) )), !.

% Destoy everything at a given location in the map
destroyEverythingAt(X, Y, except(L_EXAP)):-
        destroyEverythingAt(X, Y, L_EXAP, []), !.
destroyEverythingAt(X, Y, L_EXAP, L_EXEID):-
        is_list(L_EXAP), is_list(L_EXEID),
        destroyAllGameEntitiesAt(X, Y, L_EXAP, L_EXEID),
        destroyMapItemAt(X, Y, except(L_EXAP)).

% Destroy all game entities at a given location
destroyAllGameEntitiesAt(X, Y, L_EXAP, L_EXEID):-
        is_list(L_EXAP), is_list(L_EXEID),
        entityLocation(EID, X, Y, AP),
        not(member(EID, L_EXEID)),
        not(member(AP, L_EXAP)),
        destroyGameEntity(EID),
        fail.
destroyAllGameEntitiesAt(_, _, _, _).

% Destroy map item at a given location, but not in its limits
destroyMapItemAt(_, 0, except(_)) :- !.
destroyMapItemAt(0, _, except(_)) :- !.
destroyMapItemAt(X, _, except(_)) :- X < 0, !.
destroyMapItemAt(_, Y, except(_)) :- Y < 0, !.
destroyMapItemAt(X, Y, except(_)) :-
    getDMap(Map),
    LX is X + 1, LY is Y + 1,
    ( length(Map, LY) 
    ; nth0(0, Map, Row), length(Row, LX)), !.
destroyMapItemAt(X, Y, except(L_EXC)):-
        getDMap(Map),
    getCellContent(X, Y, Map, WHAT),
    ( not(member(WHAT, L_EXC)),
      updateCellContent(X, Y, ' ', Map, NewMap),
      updateDMap(NewMap),
      mainLog(append_c( um(X, Y, '\' \'') ))
    ; true), !.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% entityLocation(EID, -X, -Y, -Ap)
%   EID: Entity identifier
%   X, Y, Ap: Location and Appearance of the entity
%    
%   Obtains entity location knowing the entity-identifier
%   
% entityName(+EID, -NAME)
%   Returns the name of a entity
%
% entityType(EID, Type)
%   Type: Entity type
%   
%   Relates Entity type to identifiers of the entities of that
%   type.
%   
% solidEntity(EID)
%   
%   Defines entities that are solid
%   
% staticObject(EID)
% 
%   Defines objects that are static (they cannot be taken)
%
% getAllEntitiesFrom(-ENTITYLIST, +X, +Y, +EXCEPTLIST)
%   
%   Obtains all entities from a given (X,Y) place removing 
%   the entities given in the EXCEPTLIST.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aliveEntity(EID):-
    d_entity(EID, _, _, _, _).
entityName(EID, plman):-
    entityType(EID, pacman), !.
entityName(EID, NAME):-
    dynamicProperties(get(EID, name(NAME))) , !.
entityName(_, noname).
entityLocation(EID, X, Y, Ap):-
    d_entity(EID, _, _, _, location(X, Y, Ap)).
entityLocation(EID, X, Y, Ap):-
    d_object(HOLD_EID, d_entity(EID, _, _, _, location(_,_,Ap))),
    d_entity(HOLD_EID, _, _, _, location(X, Y, _)).
entityType(EID, Type):-
    d_entity(EID, Type, _, _, _).
solidEntity(EID):-
    (entityType(EID, solid);
    dynamicProperties(get(EID, solid(true)))), !.
staticObject(EID):-
    entityType(EID, object),
    dynamicProperties(get(EID, static(true))).

% Check that a newly created object is not in the same place of a previously created one
%  If it is, the newly created object gets destroyed
%
checkSamePlaceAsOtherObjectAndDestroy(OID):-
    entityLocation(OID, X, Y, _),
    entityLocation(OID_1, X, Y, _),
    OID \= OID_1,
    destroyGameEntity(OID), !.

% Get the name of an object
objectName(ObjID, Name):-
    dynamicProperties(get(ObjID, name(Name))).
changeEntityAppearance(EID, NewAP, NewAtr, NewTC, NewBC):-
    entityLocation(EID, _, _, OldAP),
    dynamicProperties(get(EID, appearance(attribs(OldAtr, OldTC, OldBC)))),
    (var( NewAP) ->  NewAP= OldAP ; true),
    (var(NewAtr) -> NewAtr=OldAtr ; true),
    (var( NewTC) ->  NewTC= OldTC ; true),
    (var( NewBC) ->  NewBC= OldBC ; true),  
    dynamicProperties(set(EID, appearance(attribs(NewAtr, NewTC, NewBC)))),
    retract(d_entity(EID, T, St, CR, location(X, Y,     _))),
    assert( d_entity(EID, T, St, CR, location(X, Y, NewAP))),
    mainLog(changeEntityAppearance(EID, appearance(NewAP, NewAtr, NewTC, NewBC))).
changeEntityAppearance(EID, NewAp, OldAp):-
    retract(d_entity(EID, T, St, CR, location(X, Y, OldAp))),
    assert( d_entity(EID, T, St, CR, location(X, Y, NewAp))),
    mainLog(changeEntityAppearance(EID, NewAp)).
invertEntityColors(EID):-
    dynamicProperties(get(EID, appearance(attribs(OldAt, OldTC, OldBC)))),
    dynamicProperties(set(EID, appearance(attribs(OldAt, OldBC, OldTC)))).
changeObjectName(ObjID, NewName, OldName):-
    dynamicProperties(get(ObjID, name(OldName))),
    dynamicProperties(set(ObjID, name(NewName))).
changeEntityLocation(EID, NewX, NewY, OldX, OldY):-
    retract(d_entity(EID, T, St, CR, location(OldX, OldY, Ap))),
    assert( d_entity(EID, T, St, CR, location(NewX, NewY, Ap))).
makeObjectStatic(ObjID):-
    dynamicProperties(set(ObjID, static(true))).
makeObjectSolid(ObjID):-
    dynamicProperties(set(ObjID, solid(true))).
makeObjectNotStatic(ObjID):-
    dynamicProperties(set(ObjID, static(false))).
makeObjectNotSolid(ObjID):-
    dynamicProperties(set(ObjID, solid(false))).
deactivateEntity(EID):-
    retract(d_entity(EID, T,        _, CR, Loc)),
    assert( d_entity(EID, T, inactive, CR, Loc)).
activateEntity(EID):-
    retract(d_entity(EID, T,      _, CR, Loc)),
    assert( d_entity(EID, T, active, CR, Loc)).
isObjectGot(OID):-
    d_object(_, d_entity(OID, _, _, _, _)).
isInteractive(EID):-
    d_entity(EID, T, _, _, _),
    T \= non_interactive.
getAllEntitiesFrom(ENTITYLIST, X, Y, EXCEPTLIST) :-
    is_list(EXCEPTLIST), integer(X), integer(Y),
    findall(OID, entityLocation(OID, X, Y, _), L_ENT),
    subtract(L_ENT, EXCEPTLIST , ENTITYLIST), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% waitFor(SleepTime)
%    SleepTime: keypress -> Wait until a key is pressed
%       secs -> Waits some secs 
%    
%  Waits some time or for a key press
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p_actionKeyPress(27):- playerStats(set(game_status(finished))), !.
p_actionKeyPress(_).
waitFor(keypress):- 
    get_single_char(CHAR), 
    p_actionKeyPress(CHAR), !.
waitFor(SleepTime):- 
    number(SleepTime),
    sleep(SleepTime).
waitFor(SOMETHING):-
    showSystemMsg(system, error, sleep_time_error, SOMETHING, [halt]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% endMessages(NumDots)
%    NumDots: Number of lasting dots on the map
%    
%  Shows lastest messages when the game is over, depending
%  on the number of dots lasting on the map.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
endMessages(0):-
    showSystemMsg(user, normal, you_win, '', []).
endMessages(_):-
    showSystemMsg(user, normal, you_lose, '', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% play
%    MapFile: Prolog file containing the description of the map
%    BotRule: Regla que debe ser llamada para cederle el
%         control al bot que controla el Comecocos
%    SleepTime: Tiempo de pausa entre movimientos.
%    
% Da comienzo a la partida
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(MapFile, BotRule):- play(MapFile, BotRule, 0, []).
play(MapFile, BotRule, SleepTime):- play(MapFile, BotRule, SleepTime, []).
play(MapFile,       _, SleepTime, L_PARAMS):- 
    gameInit(MapFile), !,
    play(activateExecutionParameters(L_PARAMS)), !,
    mainLog(initialState), !,
    play(mainLoop(SleepTime)), !,
    play(finalEvaluation(L_PARAMS)),
    mainLog(close).

%
% Rule for evaluation purposes
%   - It measures performance achieved by solution proposed
%
play(finalEvaluation(L_PARAMS)):-
    member(eval(true), L_PARAMS), 
    play(show_endstats), !.
play(finalEvaluation(_)).

%
% Activate pl-man behaviour modifiers
%
play(activateExecutionParameters(L_PARAMS)):-
    (member(log_file(LOG_FILE), L_PARAMS), mainLog(create(LOG_FILE)); true),
    (member(max_moves(MV), L_PARAMS), playerStats(set(max_movements(MV))); true),
    (member(drawing(false), L_PARAMS), !; assert(d_drawing)), !.

%%
%%% Finishing execution
%% 
play(endplay):-
    d_drawing,
    playerStats(get(remaining_dots(D))),
    endMessages(D),
    showSystemMsg(user, normal, press_X, '', []),
    drawWorld,
    repeat,
     get_single_char(C),
     (char_code('X', C); char_code('x', C)), !.
play(endplay):-
    not(d_drawing).

%%
%%% Main Loop
%% 
play(mainLoop(_)) :- 
    ( playerStats(get(game_status(finished)))
    ; playerStats(get(max_movements(MMV))), 
      MMV > 0,  
      playerStats(get(movements(M))), 
      M > MMV
    ),
    play(endplay), !.
play(mainLoop(SleepTime)):-
    drawWorld, !,
    mainLog(flush),
    waitFor(SleepTime),
    updateEntities, !,
    updateWorld, !,  
    play(mainLoop(SleepTime)).
    
%%
%%% Show statistics at the end of the game
%% 
play(show_endstats):-
    cls,
    statistics(inferences, INF),
    statistics(cputime, CT),
    playerStats(get(max_movements(MAXMV))),
    playerStats(get(remaining_dots(D))),
    playerStats(get(movements(MO))),
    playerStats(get(collisions(CO))),
    playerStats(get(bad_attempts(BA))),
    playerStats(get(unknown_actions(IA))),
    playerStats(get(fails(FA))),
        num_dots(TD),
    Comidos is TD - D,
    Percent is (100 * Comidos / TD),
    system:writeln('============================================================='),
    system:writeln('||              ESTADISTICAS DE EJECUCION                    '),
    system:writeln('|____________________________________________________________'),
    system:write('| * Estado final:       '),
    ( (MAXMV >= 0, MAXMV =< MO)
     -> system:writeln('LIMITE DE MOVIMIENTOS SUPERADO :(')
     ;(D > 0)
     -> system:writeln('PACMAN HA MUERTO! :(')
     ;  system:writeln('MAPA SUPERADO! :)')
    ),
    system:writeln('|____________________________________________________________'),
    maplist(system:write, ['| + Cocos comidos:      ', Comidos, ' / ', TD, ' (', Percent,'%)\n']),
    maplist(system:write, ['| + Movimientos:        ', MO]),
    (MAXMV >=0 -> maplist(system:write, ['  (Max: ', MAXMV,')\n']) ; system:nl),
    maplist(system:write, ['| + Inferencias:        ', INF, '\n']),
    maplist(system:write, ['| + Tiempo CPU:         ', CT, '\n']),
    system:writeln('|____________________________________________________________'),
    maplist(system:write, ['| - Colisiones:         ', CO, '\n']),
    maplist(system:write, ['| - Intentos de accion: ', BA, '\n']),
    maplist(system:write, ['| - Acciones erroneas:  ', IA, '\n']),
    maplist(system:write, ['| - Fallos de la regla: ', FA, '\n']),
    system:writeln('=============================================================').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replay
%    LogFile: Log of a previously played game that is asked
%             to be reproduced again
%    L_PARAMS: Reproduction parameters
%    
% Reproduces a previously played match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replay(LogFile, L_PARAMS):-
    is_list(L_PARAMS),
    catch(load_files(LogFile, [silent(true)]),_, fail),
    retractall(d_replay(_,_)),
    assert(d_replay(1,0)),
    replay(calculateMaxCycle),
    replay(reset), 
    replay(init), 
    replay(mainLoop(0)), !.
replay(LogFile, L_PARAMS):-
    maplist(system:write, [ 'ERROR: Bad parameters for replay(', LogFile, ',', L_PARAMS, ').\n' ]).


%%
%% Main replay loop
%%
replay(mainLoop(27)):- 
    cls, !.
replay(mainLoop(_)):- 
    drawWorld, !,
    get_single_char(CO),
    replay(processKeyCommand(CO)), !,
    replay(mainLoop(CO)).

%%
%% Get and change step size
%% Calculate Max cycle and get it 
%%
replay(setStepSize(N)):-
    retract(d_replay(_, MC)),
    assert(d_replay(N, MC)), 
    maplist(user:write, ['Stepsize is no set to ', N, '\n' ]), !.
replay(getStepSize(N)):-
    d_replay(N, _), !.
replay(calculateMaxCycle):-
    setof(A, B^c(A, B), L), 
    maxOfList(MAX, L),
    MAX_CYCLE is MAX + 1,
    retract(d_replay(N, _)),
    assert(d_replay(N, MAX_CYCLE)), !.
replay(getMaxCycle(MC)):-
    d_replay(_, MC), !.

%%
%% Process different commands depending on what key the user has pressed 
%%
replay(processKeyCommand(CH)):-
    (CH = 80 ; CH = 112), % 'P' o 'p'
    replay(getStepSize(STEP)),
    replay(jump_steps(STEP)), !.
replay(processKeyCommand(CH)):-
    (CH = 79 ; CH = 111), % 'O' o 'o'
    replay(getStepSize(STEP)),
    replay(jump_steps(-STEP)), !.
replay(processKeyCommand(CH)):-
    (CH >= 49, CH =< 57), % numbers 1-9
    STEP is CH - 48,
    replay(setStepSize(STEP)), !.
replay(processKeyCommand(27)):-!.
replay(processKeyCommand(K)):-
    char_code(CH, K),
    maplist(user:write, 
        ['WARNING: You have pressed a non-associated key (',CH,')\n',
         'Functional Keys are (1-9, O, P, ESC)\n']).

%%
%% Jump step to a new situation in the replay
%%
replay(jump_steps(0)):-!.
replay(jump_steps(N)):-
    replay(calculateStepRange(N, FROM, TO)),
    playerStats(set(movements, TO)),
    replay(update(FROM, TO)), !.
replay(jump_steps(_)).

replay(calculateStepRange(STEP, FROM, TO)):-
    playerStats(get(movements(FROM))),
    replay(getMaxCycle(MC)),
    TTO is FROM + STEP,
    ( TTO < 0, TO = 0
    ; TTO > MC, TO = MC
    ; TO = TTO), !.

%%
%% Update from one world state to other
%%
replay(update(X, X)):-!.
replay(update(0, TO)):-
    TO > 0,
    ( not(c(0, _)) ;
      forall(c(0, ACT), ( (functor(ACT, cge, _), !) ; replay(change(ACT)) ) )
    ),
    replay(update(1, TO)), !.
replay(update(_, 0)):-
    replay(reset),
    replay(init), !.
replay(update(FROM, TO)):-
    FROM < TO,
    TO1 is TO - 1,
    forall(between(FROM, TO1, ST), (
        forall(c(ST, ACT), replay(change(ACT))) 
        )), !.
replay(update(_, TO)):-
    TO1 is TO - 1,
    replay(reset),
    c(0, map(M)),
    updateDMap(M),
    forall(between(0, TO1, ST), (
        forall(c(ST, ACT), replay(change(ACT))) 
        )),
    playerStats(set(movements(TO))), !.

%%
%% Make an individual change to the world status
%%
replay(change(map(_))):- !.
% update map
replay(change(um(X, Y, ST))):-   
    getDMap(Map),
    updateCellContent(X, Y, ST, Map, NewMap),
    updateDMap(NewMap), !. 
% move entity
replay(change(me(EID, X, Y))):-
    d_EIDConverter(EID, R_EID),
    retract(d_entity(R_EID, Type, Status, ControlRule, location(_,_,Ap))),
    assert(d_entity(R_EID, Type, Status, ControlRule, location(X,Y,Ap))), !.
% destroy entity
replay(change(de(EID))):-
    retract(d_EIDConverter(EID, R_EID)),
    destroyGameEntity(R_EID), !.
% Getting an Object
replay(change(go(EID, OID))):-
    d_EIDConverter(EID, R_EID),
    d_EIDConverter(OID, R_OID),
    ENTY = d_entity(R_OID, object, _, _, location(_,_,_)),
    ENTY, assert(d_object(R_EID, ENTY)),
    retract(ENTY), !.
% Dropping an Object
replay(change(do(EID, OID, X, Y))):-
    d_EIDConverter(EID, R_EID),
    d_EIDConverter(OID, R_OID),
    retract(d_object(R_EID, OBJ)), 
    OBJ = d_entity(R_OID, object, Status, Rule, location(_,_,Ap)),
    assert(d_entity(R_OID, object, Status, Rule, location(X,Y,Ap))), !.
% create game entity
replay(change(cge(EID, Type, l(X, Y), ap(AP, ATR, FC, BC)))):-
    createGameEntity(REAL_EID, AP, Type, X, Y, inactive, norule, [appearance(attribs(ATR, FC, BC))]),
    assert(d_EIDConverter(EID, REAL_EID)), !.
% Changing simple entity appearance
replay(change(cea(EID, NewAp))):- 
    d_EIDConverter(EID, R_EID),
    changeEntityAppearance(R_EID, NewAp, _), !.
% Changing complex entity appearance
replay(change(cea(EID, NewAP, NewAtr, NewTC, NewBC))):- 
    d_EIDConverter(EID, R_EID),
    changeEntityAppearance(R_EID, NewAP, NewAtr, NewTC, NewBC), !.
% Writing to the window buffer
replay(change(w(TEXT))):- 
    with_output_to(string(S), system:write(TEXT)),
    user:write(S), !.
% Other actions logged that this player do not recognize
replay(change(OTHER)):- 
    with_output_to(string(S), system:write(OTHER)),
    maplist(user:write, ['WARNING: Not known log action (',S,')\n']), !.    
%%
%% Resetting and initializing the replay
%%
replay(reset):-
    dynamicProperties(reset),
    playerStats(reset),
    retractall(d_drawing),
    retractall(d_logging),
    retractall(d_EIDConverter(_,_)),
    retractall(d_object(_, _)),
    retractall(d_entity(_,_,_,_,_)),
    retractall(d_messageCounter(_)),
    retractall(d_doAction(_)),
    retractall(d_doAction(_,_)),
    assert(d_messageCounter(1)),
    assert(d_drawing),
    properties(msgWindow,W,H,_,_,_),
    clearMsgWindow,
    setMsgWindowSize(W,H).
replay(init):-
    c(0, map(M)),
    startNewMap(M),
    forall( c(0, cge(EID, Type, l(X, Y), ap(AP, ATR, FC, BC))), 
        replay(change(cge(EID, Type, l(X, Y), ap(AP, ATR, FC, BC)))) ).
