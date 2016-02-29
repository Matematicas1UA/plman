%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2008 Francisco Gallego <ronaldo@cheesetea.com>
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
% respawnPoint
%
% Controlls the behaviour of a group of entities that 
% represent respawn points for entities (normally, for enemies)
%
% Initialization
%--------------------
% respawnPoint(init, L_OIDs, MAX_OBJS, L_OBJPROPS, L_INIT, L_MODIFIERS)
%	L_OIDs: List of identifiers of the objects that represents 
%	   	different respawn points that act as a group
%	MAX_OBJS: Max number of objects to respawn. Once this number
%		of objects has been created, the respawner waits for 
%		at least one of them to be destroyed to continue respawning.
%	L_OBJPROPS: List of properties of new objects to be created. 
%		These properties include:
%			- app(AP): Appearance of new entities to be created. DEF: ' '
%			- type(T): Type of new entities to be created (object, mortal, ...). DEF: mortal
%			- active(CR): Parameter of activity for newly created objects. DEF: active
%			- crule(CR): Control rule for new objects to be created. DEF: norule
%			- name(N): Name of the new entities to be created. DEF: noname
%			- solid(S): Solid state of the new entities to be created. DEF: false
%			- static(S): Static state of the new entities to be created. DEF: false
%			- use_rule(R): Use rule of the new entities to be created. DEF: norule
%			- description(D): Description of the new entities to be created. DEF: unknown
%			- appearance(attribs(ATR,TC,BC)): Appearance attributes of the
%				new entities to be created (ATTRIBUTE, TEXTCOLOR, BACKG_COLOR)
%	L_INIT: List of rules to call on the event of initializing every object created.
%		The first element of this list MUST be an uninstantiated variable which will
%		by instantiated with the entity identifier (EID) of each new object to be
%		created.
%	L_MODIFIERS: List of atoms that express modifications to the 
%		standard behaviour.
%			- probRespawn(PR): Probability (1-1000) of respawning a new entity in each 
%				execution cycle. DEF: 350
%
% Example
%--------------------
%  createGameEntity(OID_RS1, '', object, 5, 2, inactive, norule, 
%	            [name(respawn1), solid(false), static(true), use_rule(norule), description('Punto de respawn')]), 
%  createGameEntity(OID_RS2, '', object, 10, 2, inactive, norule, 
%	            [name(respawn2), solid(false), static(true), use_rule(norule), description('Punto de respawn')]), 
%  respawnPoint(init, [ OID_RS1, OID_RS2 ], 5, 
%		[ app('F'), type(mortal), crule(enemyBasicMovement) ], 
%		[ EID, enemyBasicMovement(init, EID, left-right, ['#']) ], []).
%
% Creates two respawn points that may respawn up to 5 
% enemies that will always move from left to right, using
% the enemyBasicMovement behaviour.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(respawnPoint, [ respawnPoint/1, respawnPoint/6 ]).
:- dynamic d_respawnSt/2.

%%%
%%% Initialization
%%%
respawnPoint(init, L_OIDS, MAX_OBJS, L_EPROPS, L_INIT, L_MODIFIERS):-
	% Perform necessary checks
	number(MAX_OBJS), MAX_OBJS > 0,
	is_list(L_OIDS), is_list(L_INIT),
	is_list(L_EPROPS), is_list(L_MODIFIERS),
	L_INIT=[ID|_], var(ID),
	% Create respawner and set properties
	'pl-man':createGameEntity(EID_RPW, '', object, -1, -1, active, respawnPoint, []),
	'pl-man':dynamicProperties(create(GID, 
		[ cge([]), initSequence(L_INIT), respawnPoints(L_OIDS), aliveEntities(0), 
		  maxEntities(MAX_OBJS), entitiesRespawned([]), probRespawn(350) ])),
	'pl-man':dynamicProperties(create(GID_TMP, 
		[ app(' '), type(mortal), active(active), crule(norule), name(noname), solid(false), static(false),
		  use_rule(norule), description(unknown), appearance(attribs(normal, default, default))])),
	forall(member(P, L_EPROPS), 'pl-man':dynamicProperties(set(GID_TMP, P))),
	forall(member(P, L_MODIFIERS), 'pl-man':dynamicProperties(set(GID, P))),
	p_createCGE(GID, GID_TMP),
	assert(d_respawnSt(EID_RPW, GID)).
respawnPoint(init, L_OIDS, _, _):-
        'pl-man':lang_message(respawnPoint, bad_parameters, MSG),
	write(MSG), maplist(write, L_OIDS), write('\n').

%%%
%%% Control
%%%
respawnPoint(EID):-
	d_respawnSt(EID, GID),
	respawnPoint(EID, GID).

% Count still alive entities
respawnPoint(_, GID) :-
	'pl-man':dynamicProperties(get(GID, entitiesRespawned, ER)),
	ER \= [],
	partition('pl-man':aliveEntity, ER, ER_ALIVE, ER_DEAD),
	length(ER_DEAD, N),
	'pl-man':dynamicProperties(get(GID, aliveEntities, AE)),
	NAE is AE - N,
	'pl-man':dynamicProperties(set(GID, aliveEntities, NAE)),
	'pl-man':dynamicProperties(set(GID, entitiesRespawned, ER_ALIVE)),
	fail.

% Spawn new entity
respawnPoint(_, GID) :-
	'pl-man':dynamicProperties(get(GID, aliveEntities, AE)),
	'pl-man':dynamicProperties(get(GID, maxEntities, ME)),
	AE < ME,
	'pl-man':dynamicProperties(get(GID, probRespawn, PRPW)),
	'pl-man':randomBetween(1,1000,P),
	P < PRPW,
	'pl-man':dynamicProperties(get(GID, respawnPoints, L_OIDS)),
	'pl-man':randomFromList(OID, L_OIDS),
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':dynamicProperties(get(GID, cge, L_CGE)),
	L_CGE = [ CGE ],
	arg(1, CGE, EID),
	arg(4, CGE, X),
	arg(5, CGE, Y),
	call('pl-man':CGE),
	'pl-man':dynamicProperties(get(GID, initSequence, L_INIT)),
	L_INIT = [ EID | L_INITCALL ],
	forall(member(M, L_INITCALL), ignore(call(M))),
	'pl-man':dynamicProperties(get(GID, entitiesRespawned, L_ENTITIES)),
	NAE is AE + 1,
	append(L_ENTITIES, [ EID ], NL_ENT),
	'pl-man':dynamicProperties(set(GID, aliveEntities, NAE)),
	'pl-man':dynamicProperties(set(GID, entitiesRespawned, NL_ENT)).
%%%
%%% Subrules
%%%
% Set-up a predifined createGameEntity functor
p_createCGE(GID, GID_TMP) :-
	'pl-man':dynamicProperties(get(GID_TMP, app, APP)),
	'pl-man':dynamicProperties(get(GID_TMP, type, TYPE)),
	'pl-man':dynamicProperties(get(GID_TMP, active, ACTIVE)),
	'pl-man':dynamicProperties(get(GID_TMP, crule, CRULE)),
	'pl-man':dynamicProperties(get(GID_TMP, name, NAME)),
	'pl-man':dynamicProperties(get(GID_TMP, solid, SOLID)),
	'pl-man':dynamicProperties(get(GID_TMP, static, STATIC)),
	'pl-man':dynamicProperties(get(GID_TMP, use_rule, USERULE)),
	'pl-man':dynamicProperties(get(GID_TMP, description, DESC)),
	'pl-man':dynamicProperties(get(GID_TMP, appearance, APPEARANCE)),
	'pl-man':dynamicProperties(set(GID, cge, 
		[ createGameEntity(_, APP, TYPE, _, _, ACTIVE, CRULE, 
			[ name(NAME), solid(SOLID), static(STATIC), use_rule(USERULE), description(DESC), appearance(APPEARANCE) ]) ])).

