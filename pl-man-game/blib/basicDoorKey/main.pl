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
% basicDoorKey
%
% Controlls the behaviour of a door and the group of
% keys that available to open it.
%
% Initialization
%--------------------
%  basicDoorKey(init, OID, ON_OPEN, KEYLIST)
%   OID: Identifier of the door object
%   ON_OPEN: List of actions to do when door is open. They 
%		could be any pl-man actions
%   KEYLIST: List of OIDs of keys that can open the door
%
% Example
%--------------------
% createGameEntity(OID_D, '/', object, 1, 4, inactive, norule, 
%                 data(puerta, solid, static, norule, 'Puerta que se abre con la llave azul')), 
% createGameEntity(OID_A, '-', object, 8, 5, inactive, norule, 
%                 data(llave_azul, not_solid, not_static, basicDoorKey, 'Llave Azul')),
% basicDoorKey(init, OID_D, 
%	['pl-man':destroyGameEntity(OID_D), 'pl-man':destroyGameEntity(OID_A)], [OID_A]).
%
% Creates a door that can only be opened with 'llave_azul'.
% The door and the key simply vanish when the door is opened.
%
% **** THIS BEHAVIOUR IS DEPRECATED ***
% **** ERRORS MAY APPEAR WITH THE ON_OPEN LIST ***
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('basicDoorKey', [ basicDoorKey/4, basicDoorKey/5 ] ).
:- dynamic d_basicDoorKeyST/3.

basicDoorKey(init, OID, ON_OPEN, KEYLIST):-
	number(OID),
	is_list(ON_OPEN),
	is_list(KEYLIST),
	maplist(callable, ON_OPEN),
	retractall(d_basicDoorKeyST(OID, _, _)),
	assert(d_basicDoorKeyST(OID,ON_OPEN,KEYLIST)).
basicDoorKey(OID, _, X, Y, _):-
	'pl-man':entityLocation(EID_DOOR, X, Y,  _),
	d_basicDoorKeyST(EID_DOOR, ON_OPEN, KEYLIST),
	member(OID, KEYLIST),
	p_openDoor(EID_DOOR, ON_OPEN), !.
basicDoorKey(OID, _, _, _, _):-
	'pl-man':objectName(OID, NAME),
        'pl-man':lang_message(basicDoorKey, cannot_be_used_there, MSG),
	maplist(user:write, ['(', NAME, '); ', MSG, '\n']), !.
p_openDoor(EID_DOOR, ON_OPEN):-
	'pl-man':objectName(EID_DOOR, NAME),
	'pl-man':makeObjectNotSolid(EID_DOOR),
	maplist(call, ON_OPEN),
        'pl-man':lang_message(basicDoorKey, door_opened, MSG),
	maplist(user:write, ['(', NAME, '): ', MSG, '!\n']), !.
