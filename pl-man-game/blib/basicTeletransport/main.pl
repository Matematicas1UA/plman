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
% basicTeletransport
%
% Controlls the behaviour of a teletransport object.
%
% Initialization
%--------------------
%  basicTeletrasnport(init, EID, from(X1, Y1), to(X2, Y2), L_PARAMS):-
%    OID: Identifier of the door object
%    X1, Y1: Coordinates from the origin of the telentransport tunnel
%    X2, Y2: Coordiantes from the end of the telentransport tunnel
%    L_TRANS: List of entities that can use the teletransport (by their appearance)
%    L_PARAMS: Params to control the way teletransport behaves
%       viceversa: Enables reverse teletransport from-to and to-from
%
% Example
%--------------------
%  createGameEntity(OID_TT, '?', object, 5, 2, active, basicTeletransport, 
%	            [name(teletransport), solid(false), static(true), use_rule(norule), description('Teletransporte')], 
%  basicTeletransport(init, OID_TT, from(5, 2), to(1, 1), ['@'] ,[ viceversa ]).
%
% Creates a teletranport tunnel from (5,2) to (1,1) 
% which could be used viceversa. The tunnel is only available
% for pacman ('@').
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(basicTeletransport, [basicTeletransport/1, basicTeletransport/6]).
:- dynamic d_transportStatus/6.

% Initialization
%
basicTeletransport(init, EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS):-
	number(EID),
	number(X1), number(Y1),
	number(X2), number(Y2),
	is_list(L_PARAMS),
	is_list(L_TRANS),
	retractall(d_transportStatus(EID, _, _, _, _, _)),
	assert(d_transportStatus(EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS, ready)), !.
basicTeletransport(init, EID, _, _, _, _):-
        'pl-man':lang_message(basicTeletransport, bad_parameters, MSG),
	maplist(user:write, [MSG, EID, '\n']).

% Control
%
basicTeletransport(EID):-
	d_transportStatus(EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS, ready),
	(check_transport(EID, X1, Y1, X2, Y2, L_TRANS, [ viceversa ]) ;
	 check_transport(EID, X2, Y2, X1, Y1, L_TRANS, L_PARAMS) ; true), !.
basicTeletransport(EID):-
	d_transportStatus(EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS, not_ready),
	not(('pl-man':entityLocation(_, X1, Y1, AP1), member(AP1, L_TRANS))),
	not(('pl-man':entityLocation(_, X2, Y2, AP2), member(AP2, L_TRANS))),
	retract(d_transportStatus(EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS, not_ready)),
	assert(d_transportStatus(EID, from(X1, Y1), to(X2, Y2), L_TRANS, L_PARAMS, ready)), !.

% Check and transport if conditions are satisfied for a concrete type of teletransport
check_transport(EID, X1, Y1, X2, Y2, L_TRANS, L_PARAMS):-
	'pl-man':entityLocation(T_EID, X1, Y1, AP),
	member(viceversa, L_PARAMS),
	member(AP, L_TRANS),
	'pl-man':getDMap(Map),
	'pl-man':moveEntity(T_EID, Map, X2, Y2), 
	retract(d_transportStatus(EID, from(A1, A2), to(B1, B2), L_TRANS, L_PARAMS, _)),
	assert(d_transportStatus(EID, from(A1, A2), to(B1, B2), L_TRANS, L_PARAMS, not_ready)), !.
