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
% enemyBasicMovement
%
% Controlls the behaviour of an enemy (or moving entity) 
% that moves from left to right or for up to down
%
% Initialization
%--------------------
%  enemyBasicMovement(init, EID, MOVE_TYPE, LIMITS)
%   EID: Entity ID of the entity to be controlled
%   MOVE_TYPE: up-down, down-up, left-right or right-left
%   LIMITS: List of object that make the entity change
%	its moving direction when it sees one of them.
%
% Example
%--------------------
%  createGameEntity(EID, 'E', mortal, 11, 1, active, enemyBasicMovement, 0),
%  enemyBasicMovement(init, EID, left-right, ['#', '|']).
%
% Creates an enemy that will move from left to right.
% Whenever it sees a '#' or a '|', it will change its
% direction of movement.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(enemyBasicMovement, [ enemyBasicMovement/1, enemyBasicMovement/4 ]).
:- dynamic d_enemyBMStatus/4.

%% Init
enemyBasicMovement(init, EID, MOVE_TYPE, LIMITS):-
	p_enemyNextDir(MOVE_TYPE, INIT_MOVE, _),
	number(EID),
	is_list(LIMITS),
	retractall(d_enemyBMStatus(EID, _, _, _)),
	assert(d_enemyBMStatus(EID, MOVE_TYPE, INIT_MOVE, LIMITS)), !.
enemyBasicMovement(init, EID, _, _):-
        'pl-man':lang_message(enemyBasicMovement, bad_parameters, MSG),
        maplist(user:write, [MSG, EID, '\n']).

%% Control Rule         
enemyBasicMovement(EID):-
	d_enemyBMStatus(EID, _, DIR, LIMITS),
	'pl-man':see(EID, normal, DIR, X),
	not(member(X, LIMITS)),
	'pl-man':doAction(EID, move(DIR)), !.
enemyBasicMovement(EID):-
	retract(d_enemyBMStatus(EID, MOVE_TYPE, DIR, LIMITS)),
	p_enemyNextDir(MOVE_TYPE, DIR, NEXTDIR),
	assert(d_enemyBMStatus(EID, MOVE_TYPE, NEXTDIR, LIMITS)), !.
enemyBasicMovement(EID):-
	not(d_enemyBMStatus(EID, _, _, _)),
        'pl-man':lang_message(enemyBasicMovement, instantiation_error, MSG),
	maplist(user:write, ['(EID: ', EID, '): ', MSG, '\n']).

% Next movement direction when the enemy arrives to a limit
p_enemyNextDir(up-down, up, down).
p_enemyNextDir(down-up, down, up).
p_enemyNextDir(left-right, left, right).
p_enemyNextDir(right-left, right, left).
p_enemyNextDir(T, X, Y) :- clause(p_enemyNextDir(T, Y, X), true).
