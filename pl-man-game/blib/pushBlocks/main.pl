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
% pushBlocks
%
% Controlls the behaviour of a list of blocks that 
% can be pushed using a "Palanca" :)
%
% Initialization
%--------------------
%  pushBlocks(init, OID, BLOCKLIST)
%   OID: Identifier of the object to use for pushing blocks
%   BLOCKLIST: List of OIDs of block objects that can
%		be pushed.
%
% Example
%--------------------
%  createGameEntity(OID_P, '\\', object, 5, 6, inactive, norule, 
%                 data(palanca, not_solid, not_static, pushBlocks, 'Barra grande con la
%			que se pueden empujar grandes bloques.')), 
%  createGameEntity(OID_B1,'%', object, 18, 5, inactive, norule, 
%                 data(bloque1, solid, static, norule, 'Bloque grande de hormigón')), 
%  createGameEntity(OID_B2,'%', object, 18, 6, inactive, norule, 
%                 data(bloque2, solid, static, norule, 'Bloque grande de hormigón')),
%  pushBlocks(init, OID_P, [OID_B1, OID_B2]).
%
% Creates 2 blocks (OID_B1 & OID_B2) that can be pushed
% using the "Palanca" (OID_P)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(pushBlocks, [ pushBlocks/3, pushBlocks/5] ).
:- dynamic d_availablePushBlocks/2.

pushBlocks(init, OID, BLOCKLIST):-
	is_list(BLOCKLIST),
	retractall(d_availablePushBlocks(_,_)),
	assert(d_availablePushBlocks(OID, BLOCKLIST)).
pushBlocks(OID, _, X, Y, DIR):-
	p_moveCollindantEntities(OID, X, Y, DIR),
        'pl-man':lang_message(pushBlocks, pushing, MSG),
        'pl-man':lang_message(DIR, DIRMSG),
	maplist(user:write, [MSG, DIRMSG, '\n']), !.
pushBlocks(OID, _, _, _, _):-
	not(d_availablePushBlocks(OID, _)),
        'pl-man':lang_message(pushBlocks, bad_parameters, MSG),
	maplist(user:write, [MSG, OID, '\n']), !.
pushBlocks(OID, _, _, _, _):-
	'pl-man':objectName(OID, OName),
        'pl-man':lang_message(pushBlocks, cannot_use, MSG),
	maplist(user:write, [MSG, OName, '!\n']).

%
%  Subrules
% 
p_dir(   up,  0, -1).
p_dir( down,  0,  1).
p_dir( left, -1,  0).
p_dir(right,  1,  0).
p_moveCollindantEntities(OID, X, Y, DIR):-
	'pl-man':entityLocation(OID_PUSH, X, Y, _),
	d_availablePushBlocks(OID, BLOCKLIST),
	member(OID_PUSH, BLOCKLIST),
	'pl-man':getDMap(M),
	p_dir(DIR, ADDX, ADDY),
	X1 is X+ADDX, Y1 is Y+ADDY,
	p_moveCollindantEntities(X1, Y1, DIR, M, BLOCKLIST),
	'pl-man':doEntityAction(OID_PUSH, M, move(DIR)), !.
p_moveCollindantEntities(_, _, _, _).
p_moveCollindantEntities(0, _, _, _, _):-!.
p_moveCollindantEntities(_, 0, _, _, _):-!.
p_moveCollindantEntities(X, Y, _, _, _):-
	not('pl-man':entityLocation(_, X, Y, _)), !.
p_moveCollindantEntities(X, Y, DIR, M, BL):-
	p_dir(DIR, ADDX, ADDY),
	X1 is X+ADDX, Y1 is Y+ADDY,
	p_moveCollindantEntities(X1, Y1, DIR, M, BL),
	forall('pl-man':entityLocation(EID, X, Y, _), (
		not(member(EID, BL)),
		'pl-man':doEntityAction(EID, M, move(DIR)))), !.
