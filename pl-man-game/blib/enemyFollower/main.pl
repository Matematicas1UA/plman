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
% enemyFollower
%
% Controlls the behaviour of an enemy (or moving entity) 
% that follows pacman or other kind of entity
%
% Initialization
%--------------------
%  enemyFollower(init, EID, L_FOLLOW, L_DIRS, L_PARAMS)
%   EID: Entity ID of the entity to be controlled
%   L_FOLLOW: List of types of entities that will be followed
%   L_DIRS: List of directions in which the enemy will
%           follow other entities
%   L_PARAMS: List of modifiers
%         delay(N): N Cycles of delay between movements
%
% Example
%--------------------
%  createGameEntity(EID, 'E', mortal, 11, 1, active, enemyFollower, []),
%  enemyFollower(init, EID, ['@'], [up, down], [delay(2)]).
%
% Creates an enemy that will follow pacman if the enemy
% sees it on up or down. The enemy will be 2 cycles 
% stopped after every movement it makes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(enemyFollower, [ enemyFollower/1, enemyFollower/5 ]).
:- dynamic d_enemyFollowerSt/5.

%%%
%%% Initialization
%%%
enemyFollower(init, EID, L_FOLLOW, L_DIRS, L_PARAMS):-
	integer(EID),
	is_list(L_PARAMS),
	is_list(L_FOLLOW),
	is_list(L_DIRS),
	forall(member(M, L_DIRS), member(M, [up, down, left, right])),
	p_getDelay(DELAY, L_PARAMS),
	retractall(d_enemyFollowerSt(EID, _, _, _, _)),
	assert(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, ready)), !.
enemyFollower(init, EID, _, _, _):-
        'pl-man':lang_message(enemyFollower, bad_parameters, MSG),
	maplist(user:write, [MSG, EID, '\n']).
	

%%%
%%% Control
%%%
% Look for an objective to pursue
enemyFollower(EID):-
	d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, _, ready), !,
	member(DIR, L_DIRS),
	'pl-man':see(EID, list, DIR, L_SEE),
	list_to_set(L_SEE, SET_SEE),
	member(AIM, L_FOLLOW),
	member(AIM, SET_SEE),
	p_startDelayTime(EID),
	'pl-man':doAction(EID, move(DIR)), !.

% After delay, enemy is ready again to continue
enemyFollower(EID):-
	retract(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, DELAY)),
	assert(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, ready)), !.

% One delay cycle more 
enemyFollower(EID):-
	retract(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, CYCLES)),
	NEWCYCLES is CYCLES + 1,
	assert(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, NEWCYCLES)), !.

% init error
enemyFollower(EID):-
	not(d_enemyFollowerSt(EID,_,_,_,_)),
        'pl-man':lang_message(enemyFollower, instantiation_error, MSG),
	maplist(user:write, ['(EID: ',EID ,'): ', MSG, '\n']).

%%%
%%% Subrules
%%%

% Start delay time of an enemy that has recently moved, if necessary
p_startDelayTime(EID):-
	d_enemyFollowerSt(EID, _, _, 0, _), !.
p_startDelayTime(EID):-
	retract(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, _)),
	assert(d_enemyFollowerSt(EID, L_FOLLOW, L_DIRS, DELAY, 1)).

% get delay value from the params list, if it is correctly specified
p_getDelay(0, []):- !.
p_getDelay(DELAY, L_PARAMS):-
	member(delay(DELAY), L_PARAMS), !,
	integer(DELAY),
	DELAY > 0.
p_getDelay(0, _).
