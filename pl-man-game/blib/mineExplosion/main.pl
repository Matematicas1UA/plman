%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2011 Francisco Gallego <ronaldo@cheesetea.com>
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
% mine
%
% Implements the behaviour of a programable mine, that 
% while explode some cycles after it is dropped in
% some place.
%
% Includes the sub-behaviours
%	explosion
%
% Initialization
%--------------------
%  mine(init, OID, TIME, WAVE, L_PARAMS):-
%    OID:  Identifier of the mine object
%    TIME: Cycles before the mine explodes since it is dropped
%    WAVE: Size (in chars) of the radious of the expansive wave
%    L_PARAMS: Params to control the way the mine behaves
%        no_destroy(L_EX): objects or entities that will survive
%		to the explosion of the mine
%
% Example
%--------------------
%   createGameEntity(OID_MINE, '+', object, 5, 2, active, mine, 
%	            [name(mine), solid(false), static(false), 
%	             use_rule(norule), description('programmable mine')]), 
%   mine(init, OID_MINE, 2, 1, [ no_destroy(['.']) ]).
%
% Creates a mine that will explode when two cycles of
% execution have passed since the mine has been dropped.
% The explosion will destroy everything except surrounding
% dots ('.') in 1 cell of distance from the mine.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explosion
%
% Creates an object that represents an explosion and
% destrois everything inside the cell where it is.
%
% Initialization
%--------------------
%  explosion(init, OID, TIME, L_PARAMS):-
%    OID:  Identifier of the explosion object
%    TIME: Cycles before the explosion wave disappears
%    L_PARAMS: Params to control the way the mine behaves
%        no_destroy(L_EX): objects or entities that will survive
%		to the explosion
% Example
%--------------------
%  createGameEntity(EID_EX, '*', mortal, 12, 14, active, explosion, 
%                   [appearance(attribs(bold, yellow, red))]),
%  explosion(init, EID_EX, 1, [ no_destroy(['.']) ] )
%
% Creates a mortal object at position (12,14) that is
% automátically converted into a explosion wave. The
% object will be autodestroyed in 1 cycle of execution
% and all objects in (12,14) except thouse with appearance
% '.' will also be destroyed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mine, [mine/1, mine/5, explosion/1, explosion/4]).
:- dynamic d_mineStatus/5.
:- dynamic d_explosionStatus/3.
:- dynamic d_allExplosions/1.

%%%
%%% Initialization
%%%
% init a mine
mine(init, OID, TIME, WAVE, L_PARAMS):-
	integer(OID),
	integer(TIME), TIME >= 1, 
	integer(WAVE), WAVE >= 0,
	is_list(L_PARAMS),
	retractall(d_mineStatus(OID, _,_,_,_)),
	assert(d_mineStatus(OID, standby, TIME, WAVE, L_PARAMS)), !.
mine(init, OID, _, _, _):-
        'pl-man':lang_message(mineExplosion, mine_bad_parameters, MSG),
	maplist(user:write, [MSG, OID, '\n']).

% Add new explosion to all_explosions list
explosion(add_explosion, OID) :-
        d_allExplosions(L_EXPLOSIONS),
        retractall(d_allExplosions(_)),
        append([OID], L_EXPLOSIONS, L_NEWEXP),
        assert(d_allExplosions(L_NEWEXP)), !.
explosion(add_explosion, OID) :-
        assert(d_allExplosions([OID])).

% Remove explosion from list of explosions
explosion(remove_explosion, OID):-
        retract(d_allExplosions(L_ALL)),
        delete(L_ALL, OID, L_ALLMINUS),
        assert(d_allExplosions(L_ALLMINUS)),!.
explosion(remove_explosion, OID):-
        'pl-man':lang_message(mineExplosion, explosion_error_removing, MSG),
	maplist(user:write, [MSG, OID, '\n']).

% Destroy all entities inside explosion (except other explosions)
explosion(destroy_entities, OID):-
        d_explosionStatus(OID, _, L_PARAMS), 
	'pl-man':entityLocation(OID, X, Y, _),
        'pl-man':getParamFromList(L_EXAP, no_destroy, [], L_PARAMS), 
	d_allExplosions(L_ALLEXP),
        'pl-man':destroyEverythingAt(X, Y, L_EXAP, L_ALLEXP), !.
explosion(destroy_entities, _).
        
% init a explosion
explosion(init, OID, TIME, L_PARAMS):-
	integer(OID),
	integer(TIME), TIME >= 1, 
	is_list(L_PARAMS),
	retractall(d_explosionStatus(OID, _, _)),
        explosion(add_explosion, OID),
	assert(d_explosionStatus(OID, TIME, L_PARAMS)), !.
explosion(init, OID, _, _):-
        'pl-man':lang_message(mineExplosion, explosion_bad_parameters, MSG),
	maplist(user:write, [MSG, OID, '\n']).

% Explosion wave
explosion(wave, OID, WAVE, L_PARAMS):-
	'pl-man':entityLocation(OID, X, Y, _), !,
	NWAVE is -WAVE,
	forall( ( between(NWAVE, WAVE, XW), between(NWAVE, WAVE, YW) ),
		( 
		  EX_X is X + XW, EX_Y is Y + YW,
		  'pl-man':createGameEntity(EID_EX, '*', mortal, EX_X, EX_Y, active, explosion, [appearance(attribs(bold, yellow, red))]),
		  explosion(init, EID_EX, 1, L_PARAMS)
		)).

%%%
%%% Control
%%%

%% mine

% Check if the mine has been got by an entity
mine(OID):-
	d_mineStatus(OID, standby, _, _, _), !,
	'pl-man':isObjectGot(OID),
	retract(d_mineStatus(OID, _, T, W, L_P)),
	assert(d_mineStatus(OID, ready, T, W, L_P)), !.
% Check if the mine has been activated
mine(OID):-
	d_mineStatus(OID, ready, _, _, _), !,
	not('pl-man':isObjectGot(OID)),
	retract(d_mineStatus(OID, _, T, W, L_P)),
	assert(d_mineStatus(OID, activated, T, W, L_P)), 
        'pl-man':lang_message(mineExplosion, mine_activated, MSGMINE),
	user:writeln(MSGMINE), !.
% Explosion?
mine(OID):-
	d_mineStatus(OID, activated, TIME, WAVE, L_PARAMS),
	(TIME = 1 ; 'pl-man':isObjectGot(OID)), !,
	p_mineExplosion(OID, WAVE, L_PARAMS), !.
% Countdown for explosion
mine(OID):-
	retract(d_mineStatus(OID, activated, TIME, WAVE, L_PARAMS)), !,
	NEWTIME is TIME - 1,
        'pl-man':lang_message(mineExplosion, cicles_left_for_explosion, MSGMINE),
	maplist(user:write, [MSGMINE, NEWTIME, '\n']),
	assert(d_mineStatus(OID, activated, NEWTIME, WAVE, L_PARAMS)), !.
% mine init error
mine(OID):-
        'pl-man':lang_message(mineExplosion, mine_incorrect_instantiation, MSG),
	maplist(user:write, [MSG, OID, '\n']).


%% explosion

% explosion time finished
explosion(OID):-
        d_explosionStatus(OID, 1, _),
        explosion(destroy_entities, OID),
	retractall(d_explosionStatus(OID, _, _)), 
        explosion(remove_explosion, OID),
	'pl-man':destroyGameEntity(OID), !.

% explosiong still shinning
explosion(OID):-
	retract(d_explosionStatus(OID, TIME, L_PARAMS)), 
	NEWTIME is TIME-1,
	assert(d_explosionStatus(OID, NEWTIME, L_PARAMS)), 
        explosion(destroy_entities, OID), !.

% explosion init error
explosion(OID):-
        'pl-man':lang_message(mineExplosion, explosion_incorrect_instantiation, MSG),
	maplist(user:write, [MSG, OID, '\n']).

%%%
%%% Subrules
%%%

% mine explosion
p_mineExplosion(OID, WAVE, L_PARAMS):-
        'pl-man':lang_message(mineExplosion, explosion_sound, MSGMINE),
	user:writeln(MSGMINE),
        explosion(wave, OID, WAVE, L_PARAMS),
	retractall(d_mineStatus(OID, _, _, _, _)),
	'pl-man':destroyGameEntity(OID).
        
