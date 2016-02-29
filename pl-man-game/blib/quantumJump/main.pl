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
% quantumJump
%
% Implements the behaviour of a portable quantum Jump
% that lets the player move more than 1 cell beyond in
% a single step. 
%
% Initialization
%--------------------
%  quantumJump(init, OID, STEP, L_PARAMS):-
%    OID:  Identifier of the quantum Jump object
%    STEP: Cells you can jump in 1 single step
%    L_PARAMS: Params to control the way the quantum jump behaves
%
% Example
%--------------------
%   createGameEntity(OID_JUMP, 'j', object, 5, 2, inactive, quantumJump,
%	            [name(quatum_jump), solid(false), static(false), use_rule(quantumJump), description('quantum jump'))]), 
%   quantumJump(init, OID_JUMP, 2, [ ]).
%
% Creates a quantum Jump object that will let you jump
% two cells in just one single step.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(quantumJump, [quantumJump/4, quantumJump/5]).
:- dynamic d_quantumStatus/2.

%%%
%%% Initialization
%%%
% init a quantumJump
quantumJump(init, OID, STEP, L_PARAMS):-
	integer(OID),
	integer(STEP), STEP >= 1, 
	is_list(L_PARAMS),
        retractall(d_quantumStatus(OID, STEP)),
	assert(d_quantumStatus(OID, STEP)), !.
quantumJump(init, OID, _, _):-
        'pl-man':lang_message(quantumJump, bad_parameters, MSG),
	maplist(user:write, [MSG, '(', OID, ')\n']).

%%%
%%% Control
%%%

% Calculate destination coordinates and check
quantumJump(OID, EID, X, Y, DIR):-
        d_quantumStatus(OID, STEP),
        p_calcStep(DIR, STEP, X, Y, NX, NY),
        'pl-man':lang_message(quantumJump, qj_started, MSG1),
        'pl-man':lang_message(quantumJump, qj_step, MSG2),
        'pl-man':lang_message(quantumJump, qj_direction, MSG3),
        maplist(user:write, [MSG1, '(', MSG2, STEP, ', ', MSG3, DIR, ').']), nl,
        p_doJump(OID, EID, NX, NY).

%%%
%%% Subrules
%%%
% Trying to jump outside the map
p_doJump(OID, _, NX, NY):-
        not('pl-man':isInsideMap(NX, NY)), !,
        'pl-man':lang_message(quantumJump, jump_outside_map, MSG),
        user:writeln(MSG),
        p_destroyQuantum(OID), !.
% Trying to jump into a solid object
p_doJump(OID, _, NX, NY) :-
        'pl-man':collision(NX, NY), !,
        'pl-man':lang_message(quantumJump, jump_over_solid, MSG),
        user:writeln(MSG),
        p_destroyQuantum(OID), !.
% Do jump
p_doJump(OID, EID, NX, NY) :-
        'pl-man':changeEntityLocation(EID, NX, NY, _, _),
        'pl-man':lang_message(quantumJump, jump_success, MSG),
        user:writeln(MSG),
        p_destroyQuantum(OID), !.

% Destroy the quantum Jump
p_destroyQuantum(OID) :-
        retractall(d_quantumStatus(OID, _)),
        'pl-man':destroyGameEntity(OID),
        'pl-man':lang_message(quantumJump, qj_destroyed, MSG),
        user:writeln(MSG).
       
% Calculate where the quantum jump takes you
p_calcStep(left,  S, X, Y, NX, Y) :- NX is X - S.
p_calcStep(right, S, X, Y, NX, Y) :- NX is X + S.
p_calcStep(up,    S, X, Y, X, NY) :- NY is Y - S.
p_calcStep(down,  S, X, Y, X, NY) :- NY is Y + S.
