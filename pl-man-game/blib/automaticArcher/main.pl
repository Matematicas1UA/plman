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
% automaticArcher
%
% Creates an object that automatically shots arrows
% to kill pacman, like an archer. 
%
% It includes the sub-behaviour
% 	automaticArcherBullet
%
% Initialization
%--------------------
%  automaticArcher(init, OID, L_AIM, DIR, DELAY, L_PARAMS):-
%    OID:   Identifier of the automatic archer object
%    L_AIM: Aiming types of object that make the automatic archer 
%           start shotting proccess when seen. Example: ['@']
%    DIR:   Direction of shot (up, down, left or right)
%    DELAY: No. Cycles between shots
%    L_PARAMS: Params to control the way the automatic Archer behaves
%       continuous: shots continuously every DELAY cycles. By default
%                   it shots only when it sees an objective from L_AIM
%	randomFirstShot: Creates an artificial amount of cycles to wait
%		before the first shot
%       bullet_appearance(AP): AP is the character representing the bullet
%       bullet_appearance(AP, ATR, TC, BC): Extended appearance of the bullet.
%                   AP: Character, ATR: Atribute, TC: Text color, BC, Background color
%
% Example
%--------------------
%   createGameEntity(OID_ARCH, '>', object, 5, 2, active, automaticArcher, 
%	            [name(archer1), solid(false), static(true), norule, description('automatic archer')]), 
%   automaticArcher(init, OID_ARCH, ['@'], right, 2, [ bullet_appearance('-', bold, red, default) ]).
%
% Creates an automatic archer on location 5,2 that will shot
% automatically to every @ that it will see on its right,
% shotting at the objective every 2 cycles of execution.
% Bullets ('-') will be bold, red-colored.
%
% IMPORTANT: automaticArcher must be not_solid entities.
%  If not, bullets get created and automatically destroyed
%  because they first hit archers.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% automaticArcherBullet
%
% Controls a bullet that has been shot by an archer
%
% Initialization
%--------------------
%  automaticArcherBullet(init, OID, DIR)
%    OID:   Identifier of the bullet object
%    DIR:   Direction of movement (up, down, left or right)
%
% Example
%--------------------
%   createGameEntity(EID_BULLET, '>', mortal, 5, 2, active, automaticArcherBullet, []),
%   automaticArcher(init, OID_BULLET, right).
%
% Creates a bullet that will start at 5,2 and move to the
% right until it hits pacman or it hits a solid entity.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(automaticArcher, [automaticArcher/1, automaticArcher/6, automaticArcherBullet/1, automaticArcherBullet/3]).
:- dynamic d_automaticArcherStatus/6.
:- dynamic d_bulletStatus/2.

%%%
%%% Initialization
%%%
% init an archer
automaticArcher(init, OID, L_AIM, DIR, DELAY, L_PARAMS):-
	integer(OID),
	integer(DELAY), DELAY >= 0, 
	is_list(L_AIM),
	is_list(L_PARAMS),
	member(DIR, [up, down, left, right]),
	(member(randomFirstShot, L_PARAMS), CYCLESPASSED is random(DELAY)+1 ; CYCLESPASSED is DELAY),
	retractall(d_automaticArcherStatus(OID, _, _, _, _, _)),
	assert(d_automaticArcherStatus(OID, L_AIM, DIR, DELAY, L_PARAMS, CYCLESPASSED)), !.
automaticArcher(init, OID, _, _, _, _):-
        'pl-man':lang_message(automaticArcher, bad_parameters, MSG),
	maplist(user:write, [MSG, OID, ')\n']).

% init a bullet
automaticArcherBullet(init, OID, DIR):-
	retractall(d_bulletStatus(OID, _)),
	assert(d_bulletStatus(OID, DIR)), !.
automaticArcherBullet(init, OID, _):-
        'pl-man':lang_message(automaticArcher, bad_parameters_bullet, MSG),
	maplist(user:write, [MSG, OID, ')\n']).

%%%
%%% Control
%%%

% Cannot shot while in delay from previous shot
automaticArcher(OID):-
	d_automaticArcherStatus(OID, _, _, MAX_DELAY, _, CYCLESPASSED),
	CYCLESPASSED < MAX_DELAY,
	NEWCYCLES is CYCLESPASSED + 1,
	retract(d_automaticArcherStatus(OID, L_AIM, DIR, DELAY, L_PARAMS, _)),
	assert(d_automaticArcherStatus(OID, L_AIM, DIR, DELAY, L_PARAMS, NEWCYCLES)), !.

% Check if we have to shot
automaticArcher(OID):-
	d_automaticArcherStatus(OID, L_AIM, DIR, _, L_PARAMS, _),
	(
	 member(continuous, L_PARAMS) 
	 ;
	 'pl-man':see(OID, list, DIR, SEELIST),
	 list_to_set(SEELIST, SEESET),
	 member(X, L_AIM),
	 member(X, SEESET)
	),
	p_shot(OID), !.

% init error
automaticArcher(EID):-
	not(d_automaticArcherStatus(EID, _, _, _, _, _)),
        'pl-man':lang_message(automaticArcher, incorrect_instantiation, MSG),
	maplist(user:write, ['(', EID, '): ', MSG, '\n']).

%------------------------------------	
% Check if the bullet has reached a solid entity
automaticArcherBullet(OID):-
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':getDMap(Map),
	('pl-man':collision(X, Y, Map);
	 'pl-man':entityLocation(EID_2, X, Y, _), 
	  EID_2 \= OID, 
	 'pl-man':solidEntity(EID_2)),
	'pl-man':destroyGameEntity(OID), !.

% move the bullet
automaticArcherBullet(OID):-
	d_bulletStatus(OID, DIR),
	'pl-man':doAction(OID, unrestricted_move(DIR)), !.

% init error
automaticArcherBullet(EID):-
        'pl-man':lang_message(automaticArcher, incorrect_instantiation_bullet, MSG),
	maplist(user:write, ['(', EID, '): ', MSG, '\n']).

%%%
%%% Subrules
%%%

% shot a new bullet
p_shot(OID):-
	retract(d_automaticArcherStatus(OID, L_AIM, DIR, DELAY, L_PARAMS, _)),
	assert(d_automaticArcherStatus(OID, L_AIM, DIR, DELAY, L_PARAMS, 0)),
	p_getBulletAppearance(OID, L_PARAMS, AP, ATR, TC, BC),
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':createGameEntity(OID_BULLET, AP, mortal, X, Y, active, automaticArcherBullet, [appearance(attribs(ATR,TC,BC))]),
	automaticArcherBullet(init, OID_BULLET, DIR).

% obtain the appearance of the bullet to create, depending on user parameters
p_getBulletAppearance(_, L_PARAMS, AP, normal, default, default):-
	member(bullet_appearance(AP), L_PARAMS), !.
p_getBulletAppearance(_, L_PARAMS, AP, ATR, TC, BC):-
	member(bullet_appearance(AP, ATR, TC, BC), L_PARAMS), !.
p_getBulletAppearance(OID, _, AP, normal, default, default):-
	'pl-man':entityLocation(OID, _, _, AP), !.
