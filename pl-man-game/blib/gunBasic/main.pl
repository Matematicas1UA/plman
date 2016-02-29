%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2009 Francisco Gallego <ronaldo@cheesetea.com>
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
% gunBasic
%
% Controlls the behaviour of a gun which can be got and
% shot.
%
% Initialization
%--------------------
%  gunBasic(init, OID, AMMO, AIMLIST, AMMO_END)
%   OID: Object ID of the object to be controlled
%   AMMO: Number of shots the gun comes with
%   AIMLIST: List of objects the gun can hit at
%   AMMO_END: What to do when the gun runs out of ammo
%		(destroy, keep).
%
% Example
%--------------------
% createGameEntity(OID, '¬', object, 6, 5, inactive, norule, 
%                 data(pistola, not_solid, not_static, gunBasic, 'Derringer')),
% gunBasic(init, OID, 1, ['E'], destroy).
%
% Creates a gun with 1 shot of ammo, which is only capable
% of killing enemies ('E'). This gun will disappear (i.e.
% it will autodestroy itself) once shot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(gunBasic, [ gunBasic/5 ]).
:- dynamic d_basicGunStatus/4.

% INIT
gunBasic(init, OID, AMMO, AIMLIST, GUN_END):-
	number(AMMO), 
	is_list(AIMLIST),
	member(GUN_END, [destroy, keep]),
	retractall(d_basicGunStatus(OID, _, _, _)),
	assert(d_basicGunStatus(OID, AMMO, AIMLIST, GUN_END)), 
        !.
gunBasic(init, OID, _, _, _):-
        'pl-man':lang_message(gunBasic, bad_parameters, MSG),
	maplist(user:write, [MSG, OID, '\n']).

% USE RULE
gunBasic(OID, _, _, _, _):-
	d_basicGunStatus(OID, AMMO, _, _),
	AMMO =< 0,
        'pl-man':lang_message(gunBasic, out_of_ammo, MSG),
        maplist(user:write, [MSG, '\n']),
        !.
gunBasic(OID, EID, X, Y, DIR):-
	d_basicGunStatus(OID, AMMO, AIMLIST, GUN_END),
        'pl-man':lang_message(gunBasic, bang, MSG),
        user:writeln(MSG),
	p_checkShotSuccess(EID, DIR, X, Y, AIMLIST),
	p_checkGunStatus(OID, AMMO, GUN_END),
        !.
gunBasic(OID, _, _, _, _):-
	not(d_basicGunStatus(OID, _, _, _)),
        'pl-man':lang_message(gunBasic, incorrect_instantiation, MSG),
	maplist(user:write, [MSG, OID, '\n']).

% Calculate X,Y location of the entity we are seeing
p_calculateEntityXY(left, DIST, X, Y, NX, Y):- NX is X-DIST.
p_calculateEntityXY(right, DIST, X, Y, NX, Y):- NX is X+DIST.
p_calculateEntityXY(up, DIST, X, Y, X, NY):- NY is Y-DIST.
p_calculateEntityXY(down, DIST, X, Y, X, NY):- NY is Y+DIST.

% check if shot has collided with an entity from the aimlist
p_checkShotSuccess(EID, DIR, X, Y, AIMLIST):-
	'pl-man':see(EID, list, DIR, SEELIST),
	member(AIM, SEELIST),
	member(AIM, AIMLIST), !, 
	nth0(DIST, SEELIST, AIM),
	p_calculateEntityXY(DIR, DIST, X, Y, EX, EY),
	'pl-man':entityLocation(DEST_EID,EX,EY,AIM), 
	'pl-man':destroyGameEntity(DEST_EID), 
        'pl-man':lang_message(gunBasic, entity_shot, MSG),
	maplist(user:write, [MSG, AIM, '\n']),
        !.
p_checkShotSuccess(_,_,_,_,_):-
        'pl-man':lang_message(gunBasic, futile_shot, MSG),
	user:writeln(MSG).

% check and update the status of the gun depending on ammo
p_checkGunStatus(OID, 1, destroy):-
	retractall(d_basicGunStatus(OID,_,_,_)),
	'pl-man':destroyGameEntity(OID), 
        'pl-man':lang_message(gunBasic, out_of_ammo_vanish, MSG),
	user:writeln(MSG),
        !.
p_checkGunStatus(OID, AMMO, GUN_END):-
	NEWAMMO is AMMO - 1,
	retract(d_basicGunStatus(OID, _, AIMLIST, _)),
	assert(d_basicGunStatus(OID, NEWAMMO, AIMLIST, GUN_END)),
        'pl-man':lang_message(gunBasic, ammo_status, MSG),
	maplist(user:write, [MSG, NEWAMMO, '\n']),
        !.

