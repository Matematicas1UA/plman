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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% magicWand
%
% Implements a magicWand that lets you cast spells with
% it, and do magic!.
%
% Detailed Initialization and Spell Customization
%--------------------------------------------------
%  %% Creation and initialization all-in-one
%  magicWand(create(OID, X, Y, L_PARAMS)):-    
%    X, Y: Location where to create the new wand.
%    OID, L_PARAMS: Initialization parameters. See init(OID, L_PARAMS).
%
%  %% Initialization of an object as a magicWand
%  magicWand(init(OID, L_PARAMS)):-
%    OID:  Identifier of the magicWand object
%    L_PARAMS: Params to control the way the magicWand behaves.
%       setStandardSpells(true): Initializes the wand with all the
%                           standard Spells working on it.
%
%  %% Creation of new spells for the magicWands
%  magicWand(newSpell(OID, SHA1, NPAR, ACTION)):-
%    OID:   Identifier of the magicWand able to perform the spell.
%           Use a free variable to make all the wands able.
%    SHA1:  Encripted version of the spell. Use magicWand(hashSpellName/2)
%           to encript Spell names.
%    ACTION: Callable predicate that will be called when the spell is
%           performed. Parameters OID, X, Y, L_PARAMS will be added to 
%           the call at the end.
%
%  %% Set all standard spells for a given magicWand
%  magicWand(setStandardSpells(OID))
%    OID:   Identifier of the magicWand to set all standard spells for.
%           Use a free variable to set them for all wands.
%
% Initialization Example
%--------------------------
%    'pl-man':createGameEntity(OID, '!', object, X, Y, inactive, norule, 
%        [name(magicWand), solid(false), static(false), 
%         appearance(attribs(normal, cyan, default)),
%         use_rule(magicWand), description('Magic Wand')]),
%     magicWand(init(OID, [])).
%
% Create an object and initialize it as a magicWand, with no Spells 
% associated to it.
%
% Most Simple Creation Example
%-------------------------------
%   magicWand(create(_, 1, 1, [ setStandardSpells(true) ])).
%
% Create a magicWand at location (1,1) able to perform all the 
% standard spells by default.
%
% Creation example
%--------------------
%   magicWand(create(OID_W, 1, 1, [])),
%   magicWand(newSpell(OID_W, '423ee4e0e03b49e842c2acc1dac7846c26251a68', 
%                       1, 'pl-man':push)).
%
% Create a magicWand at position (1,1) and define a 
% Spell for it, that requires 1 parameter to be casted.
% The spellName, that is requiered for casting the spell
% is SHA1-Encripted. You may use magicWand(hashSpellName(NAME, SHA1))
% to encript your own spellNames. Whenever the Spell
% is casted with doAction(use(SPELLNAME, DIR)), the 
% rule 'pl-man':push(OID, EID, X, Y, [ DIR ]) is called.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(magicWand, [magicWand/1, magicWand/5]).
:- dynamic d_wandStatus/2.
:- dynamic d_spell/4.

%%%
%%% Creation and initialization of new magicWands
%%%
magicWand(create(OID, X, Y, L_PARAMS)):-
    'pl-man':createGameEntity(OID, '!', object, X, Y, inactive, norule, 
        [name(magicWand), solid(false), static(false), 
         appearance(attribs(normal, cyan, default)),
         use_rule(magicWand), description('Magic Wand')]),
    magicWand(init(OID, L_PARAMS)), !.
magicWand(create(OID, X, Y,        _)):-
    'pl-man':showLangMessage(magicWand, bad_creation, [ 'OID: ', OID, ' XY:', X, ',', Y]), !.

%%%
%%% Initialization
%%%
% init a magicWand (if it already exists, it is overwritten)
magicWand(init(OID, L_PARAMS)):-
    integer(OID),
    'pl-man':dynamicProperties(checkPropertiesList(L_PARAMS)),
    %'pl-man':checkMandatoryParameters([digits(is_list), switchPos(integer, integer)], PARAMS, passwordDigit),
    magicWand(destroyAll(OID)),
    'pl-man':dynamicProperties(create(PID, L_PARAMS)),
    assert(d_wandStatus(OID, PID)),  
    forall(magicWand(processParameters(PID)), true).
magicWand(init(OID,        _)):-
    'pl-man':showLangMessage(magicWand, bad_parameters, [ 'OID: ', OID ]), !.

% Processing parameters to make them do their functions
magicWand(processParameters(PLID)) :-
    'pl-man':dynamicProperties(get(PLID, setStandardSpells(true))),
    d_wandStatus(OID, PLID),
    magicWand(setStandardSpells(OID)).

%%%
%%% Destroy 
%%%
% Destroy all magicWands of the same OID
magicWand(destroyAll(OID)):-
    forall( d_wandStatus(OID, _), 
            magicWand(destroy(OID)) ).
magicWand(destroy(OID)):-
    retract(d_wandStatus(OID, PID)),
    'pl-man':dynamicProperties(destroy(PID)).
magicWand(destroy(OID)):-
    'pl-man':showLangMessage(magicWand, bad_destroy, [ 'OID: ', OID ]), !.

%%%
%%% Create a new Spell
%%% 
magicWand(newSpell(OID, HASH, NPAR, ACTION)):-
    % Check parameters
    (integer(OID) ; var(OID)),
    integer(HASH), %atom(HASH), atom_length(HASH, 40),  %% FIX FOR BAD VARIANT_SHA1/2
    integer(NPAR), callable(ACTION), 
    % Create spell
    assert(d_spell(OID, HASH, NPAR, ACTION)), !.
magicWand(newSpell(OID, HASH, NPAR,      _)):-
    'pl-man':showLangMessage(magicWand, bad_spell_params, 
            [ 'OID:', OID, ' HASH:', HASH, ' NPAR:', NPAR ]), !.

%%%
%%% Create all the standard spells for a wand
%%% 
magicWand(setStandardSpells(OID)):-
    forall( magicWand(ss(SPELL), init(_)),
            magicWand(ss(SPELL), init(OID)) ), !.

%%%
%%% Identify a Spell and get its parameters
%%% 
magicWand(identifySpell(SPELL, OID, SNAME, ACT, L_PARAMS)) :-
    functor(SPELL, SNAME, NPAR), !,
    magicWand(hashSpellName(SNAME, SHA1)),
    d_spell(OID, SHA1, NPAR, ACT), !,
    findall(P, arg(_, SPELL, P), L_PARAMS).

%%%
%%% Hash a spell name into SHA1
%%% 
magicWand(hashSpellName(SNAME, SHA1)):-
    term_hash(SNAME, 1, 2147483647, SHA1).
%% FIX FOR BAD VARIANT_SHA1/2
%    term_hash(SNAME, 1, 2147483647, TH),
%    variant_sha1(TH, SHA1), !.

%%%
%%% Main Use Rule (Use magicWand to cast a Spell)
%%% 

% Main Use Rule (Trigger a spell whenever required)
magicWand(OID, EID, X, Y, SPELL) :-
    magicWand(identifySpell(SPELL, OID, SNAME, ACT, L_PARAMS)), !,
    append([ SNAME, '|'], L_PARAMS, PRINT),
    'pl-man':showLangMessage(magicWand, casting_spell, PRINT), 
    ignore(call(ACT, OID, EID, X, Y, L_PARAMS)).
magicWand(OID, EID, X, Y,     _) :-
    'pl-man':showLangMessage(magicWand, bad_spell_cast, 
            [ 'OID:', OID, ' EID:', EID, ' X,Y:(', X, ',', Y, ')\n']), !.

%%%
%%% STANDARD SPELLS: Configuration and working rules
%%%

%%% PUSH SPELL
magicWand(ss(push), init(OID)):-
    magicWand(newSpell(OID, '423ee4e0e03b49e842c2acc1dac7846c26251a68', 
                        1, magicWand(ss(push)) ) ).
magicWand(ss(push), moveEntities(           [],   _)) :- !.
magicWand(ss(push), moveEntities([EID | L_ENT], DIR)) :- 
    magicWand(ss(push), entityIsNotPushable(EID)), !,
    magicWand(ss(push), moveEntities(L_ENT, DIR)).
magicWand(ss(push), moveEntities([EID | L_ENT], DIR)) :- 
    'pl-man':getDMap(M),
    'pl-man':doEntityAction(EID, M, move(DIR)), !,
    magicWand(ss(push), moveEntities(L_ENT, DIR)).
magicWand(ss(push), entityIsNotPushable(EID)) :-
    'pl-man':dynamicProperties(get(EID, pushable(false))) ;
    'pl-man':dynamicProperties(get(EID, avoid_magic(true))).

%%% OPEN SPELL
magicWand(ss(open), init(OID)):-
    magicWand(newSpell(OID, 'c870a01096f99a16413208853c9a7879c166f095', 
                        1, magicWand(ss(open)) ) ).
magicWand(ss(open), openEntities(           [],   _)) :- !.
magicWand(ss(open), openEntities([EID | L_ENT], DIR)) :- 
    magicWand(ss(open), entityIsNotOpenable(EID)), !,
    magicWand(ss(open), moveEntities(L_ENT, DIR)).
magicWand(ss(open), openEntities([EID | L_ENT], DIR)) :- 
    'pl-man':makeObjectNotSolid(EID),
    'pl-man':invertEntityColors(EID),
    'pl-man':dynamicProperties(setOrAdd(EID, magic_locked(false))),
    magicWand(ss(open), moveEntities(L_ENT, DIR)).
magicWand(ss(open), entityIsNotOpenable(EID)) :-
    not('pl-man':solidEntity(EID))                                ;
    not('pl-man':dynamicProperties(get(EID, magic_locked(true)))) ;
    'pl-man':dynamicProperties(get(EID, avoid_magic(true))).

%%%
%%% STANDARD SPELLS: Main Use Rules
%%%

%%% PUSH SPELL: Use Rule
magicWand(ss(push), _, _, X, Y, [ DIR ]):-
    'pl-man':seeEntities(first, X, Y, DIR, L_ENT), !,
    magicWand(ss(push), moveEntities(L_ENT, DIR)).

%%% OPEN SPELL: Use Rule
magicWand(ss(open), _, _, X, Y, [ DIR ]):-
    'pl-man':seeEntities(first, X, Y, DIR, L_ENT), !,
    magicWand(ss(open), openEntities(L_ENT, DIR)).

