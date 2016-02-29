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
% bazooka
%
% Implements the behaviour of a bazooka that will be
% able to shot a number of missiles
%
% Uses subbehaviours
%       missile
%
% Direct Bazooka Creation
%-------------------------
%  bazooka(create, X, Y, L_PARAMS).
%  bazooka(create, OID_B, X, Y, L_PARAMS).
%   OID_B:    OID of the Bazooka
%   X, Y:     Starting location coordinates
%   L_PARAMS: List of parameters to modify standard behaviour
%       ammo(A):  Starting ammo of the bazooka (default=1)
%       power(X): Missile power (explosion radius, default=1)
%       speed(S): Missile speed in chars/turn (default=1)
%
%   Directly creates an entity for the bazooka and initiates it,
%   easying the task. 
%
%  Example
% --------------------
%
%   bazooka(create, 1, 3, [ammo(5)]).
%
%   Creates a new bazooka at location 1,3, with 5 missiles of ammunition.
%   Missiles will have an explosion radius of 1 char, and a speed of 1 chars/turn.
%
% Manual Bazooka Creation
%-------------------------
%  
%  If you wanted, you will be able to manually perform the requiered
%  steps to create a bazooka, so having a greater control.
%
%  Initialization
% --------------------
%  bazooka(init, OID_B, L_PARAMS):-
%   OID_B:    OID of the Bazooka
%   L_PARAMS: List of parameters to modify standard behaviour
%       ammo(A):  Starting ammo of the bazooka (default=1)
%       power(X): Missile power (explosion radius, default=1)
%       speed(S): Missile speed in chars/turn (default=1)
%
%  Example
% --------------------
%
%   createGameEntity(OID_B, 'L', object, 1, 3, active, norule, 
% 	[name(bazooka), solid(false), static(false), use_rule(bazooka),
%	description('Bazoka'), appearance(attribs(bold, cyan, default))]),
%   bazooka(init, OID_B, [ammo(5)]).
%
%   Does same as previous example.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(bazooka, [ bazooka/3, bazooka/5, bazooka/6 ]).

% d_bazookaStatus(OID_BAZOOKA, AMMO, POWER, SPEED)
:- dynamic d_bazookaStatus/4.
:- 'pl-man':loadNewBehaviour(missile).

%%
%% Initialization
%%
bazooka(init, OID_B, L_PARAMS):-
	number(OID_B), is_list(L_PARAMS),
        cheeseTools:getParamFromList(AMMO, ammo, 1, L_PARAMS), 
        cheeseTools:getParamFromList(POWER, power, 1, L_PARAMS), 
        cheeseTools:getParamFromList(SPEED, speed, 1, L_PARAMS), 
        AMMO >= 0, POWER >= 1, SPEED >= 1,
	retractall(d_bazookaStatus(OID_B, _, _, _)),
	assert(d_bazookaStatus(OID_B, AMMO, POWER, SPEED)), 
        !.
bazooka(init, OID_B, _):-
        'pl-man':lang_message(bazooka, bad_parameters, MSG),
	maplist(user:write, [MSG, OID_B, '\n']).
	
%%
%% Creation rule
%%
bazooka(create, OID_B, APP, X, Y, L_PARAMS):- 
    'pl-man':createGameEntity(OID_B, APP, object, X, Y, inactive, norule, 
		[name(bazooka), solid(false), static(false), use_rule(bazooka),
		description('Bazoka'), appearance(attribs(bold, cyan, default))]),
    bazooka(init, OID_B, L_PARAMS).
bazooka(create, APP, X, Y, L_PARAMS):- 
    bazooka(create, _, APP, X, Y, L_PARAMS).

%%
%% Control
%%
 
% Use Rule
%
bazooka(OID, _, _, _, _):-
    d_bazookaStatus(OID, 0, _, _),
    'pl-man':lang_message(bazooka, out_of_ammo, MSG),
    maplist(user:write, [MSG, '\n']), !.
bazooka(OID, _, X, Y, DIR):-
    d_bazookaStatus(OID, AMMO, POWER, SPEED),
    NEWAMMO is AMMO-1,
    'pl-man':missile(create, X, Y, DIR, [ power(POWER), speed(SPEED) ]),
    retractall(d_bazookaStatus(OID, _, _, _)),
    assert(d_bazookaStatus(OID, NEWAMMO, POWER, SPEED)),
    'pl-man':lang_message(bazooka, missile_shot, MSG),
    maplist(user:write, [MSG, NEWAMMO, '\n']).
