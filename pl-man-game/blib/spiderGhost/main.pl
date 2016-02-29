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
% spiderGhost
%
% Controlls the behaviour of a ghost that goes from wall
% to wall and sometimes sees and follows the player
%
% Initialization
%--------------------
%  spiderGhost(init, EID, L_LIMITS, L_PARAMS)
%   EID: Entity ID of the entity to be controlled
%   L_LIMITS: List of delimiter characters that make spiderGhost
%		change its direction of movement
%   L_PARAMS: List of modifiers
%		probFollowPacman: Probability of following pacman 
%			when it	is seen (0-100). Default value=50
%		maxFollowTimes: Maximum number of direction changes
%			the ghost will do when following pacman. Default value=1
%		av_dirs(LM): List of available directions to 
%			see and follow pacman. Default value=[up,down,left,right]
%
% Example
%--------------------
%  createGameEntity(EID, 'F', mortal, 11, 1, active, spiderGhost, []),
%  spiderGhost(init, EID, [ '#' ], [ probFollowPacman(80), maxFollowTimes(2) ]).
%
% Creates an enemy that will behave like a spiderGhost,
% following walls and hunting pacman. This ghost will
% follow pacman up to 2 times each time the ghost 
% lefts the wall, with 80% of probability.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(spiderGhost, [ spiderGhost/1, spiderGhost/4 ]).
:- dynamic d_spiderGhostSt/2.

%%%
%%% Initialization
%%%
spiderGhost(init, EID, L_LIMITS, L_PARAMS):-
	integer(EID),
	is_list(L_LIMITS),
	is_list(L_PARAMS),
	'pl-man':entityLocation(EID, X, Y, _),
	retractall(d_spiderGhostSt(EID, _)),
	'pl-man':dynamicProperties(create(GID, 
		[ dir(none), av_dirs([up,down,left,right]), limits([]), probFollowPacman(50),
		  current_location(p(X,Y)), previous_location(p(-1,-1)), maxFollowTimes(1),
		  followed(0) ])),
	forall(member(P, L_PARAMS), 'pl-man':dynamicProperties(set(GID, P))),
	'pl-man':dynamicProperties(set(GID, limits, L_LIMITS)),
	assert(d_spiderGhostSt(EID, GID)).
spiderGhost(init, EID, _, _):-
        'pl-man':lang_message(spiderGhost, bad_parameters, MSG),
	maplist(user:write, [MSG, EID, '\n']).	

%%%
%%% Control
%%%
% Get the GID and check cases
spiderGhost(EID):-
	d_spiderGhostSt(EID, GID),
	p_saveCurrentLocation(EID, GID),
	spiderGhost_case(EID, GID).
% init error
spiderGhost(EID):-
        'pl-man':lang_message(spiderGhost, bad_initialization, MSG),
	maplist(user:write, [MSG, '(EID: ', EID, ')\n']).

% Set new dir when it is not possible to move (if possible)
spiderGhost_case(EID, GID):-
	'pl-man':dynamicProperties(get(GID, current_location,  p(X,Y))),
	'pl-man':dynamicProperties(get(GID, previous_location, p(X,Y))),
	p_setNewMoveDir(EID, GID).
spiderGhost_case(EID, GID):-
	'pl-man':dynamicProperties(get(GID, dir, DIR)),
	'pl-man':dynamicProperties(get(GID, limits, LIMS)),
	(
	 'pl-man':see(EID, normal, DIR, OBJ), 
	 member(OBJ, LIMS), !,
	 p_setNewMoveDir(EID, GID)
	;
	 'pl-man':see(EID, normal, here, OBJ), 
	 member(OBJ, LIMS),
	 p_oppositeDir(DIR, DC),
	 'pl-man':dynamicProperties(set(GID, dir, DC)),
	 'pl-man':doAction(EID, move(DC))
	).
% Check if pacman is seen to follow 
spiderGhost_case(EID, GID):-
	'pl-man':dynamicProperties(get(GID, probFollowPacman, PFP)),
	PFP > 0,
	'pl-man':dynamicProperties(get(GID, maxFollowTimes, MFT)),
	'pl-man':dynamicProperties(get(GID, followed, FW)),
	MFT > FW,
	'pl-man':dynamicProperties(get(GID, av_dirs, L_AVD)),
	'pl-man':dynamicProperties(get(GID, dir, ACT_DIR)),
	member(DIR, L_AVD),
	DIR\=ACT_DIR,
	not(p_oppositeDir(DIR, ACT_DIR)),
	'pl-man':see(EID, list, DIR, L_SEE),
	'pl-man':dynamicProperties(get(GID, limits, LIMS)),
	not((L_SEE=[OBJ|_], member(OBJ, LIMS))),
	'pl-man':entityType(PID, pacman),
	'pl-man':entityLocation(PID, _, _, PAP),
	member(PAP, L_SEE),
	'pl-man':randomBetween(1, 100, R),
	R =< PFP,
	NFW is FW + 1,
	'pl-man':dynamicProperties(set(GID, followed, NFW)),
	'pl-man':dynamicProperties(set(GID, dir, DIR)),
	'pl-man':doAction(EID, move(DIR)).
spiderGhost_case(EID, GID):-
	'pl-man':dynamicProperties(get(GID, dir, DIR)),
	'pl-man':doAction(EID, move(DIR)).

%%%
%%% Subrules
%%%

% Set new direction of movement and make a move
p_setNewMoveDir(EID, GID):-
	'pl-man':dynamicProperties(get(GID, av_dirs, DIRS)),
	'pl-man':dynamicProperties(get(GID, limits, LIMS)),
	'pl-man':randomPermutation(DIRS, PMDIRS),
	member(D, PMDIRS),
	'pl-man':see(EID, normal, D, OBJ),
	not(member(OBJ, LIMS)),
	'pl-man':dynamicProperties(set(GID, dir, D)),
	'pl-man':dynamicProperties(set(GID, followed, 0)),
	'pl-man':doAction(EID, move(D)).
p_setNewMoveDir(  _,  _):-
	'pl-man':dynamicProperties(set(GID, dir, none)),
	'pl-man':dynamicProperties(set(GID, followed, 0)).

% Save current location
p_saveCurrentLocation(EID, GID):-
	'pl-man':dynamicProperties(get(GID, current_location, p(OX, OY))),
	'pl-man':dynamicProperties(set(GID, previous_location, p(OX, OY))),
	'pl-man':entityLocation(EID, X, Y, _),
	'pl-man':dynamicProperties(set(GID, current_location, p(X, Y))).

% Oposite dirs
p_oppositeDir(up, down).
p_oppositeDir(down, up).
p_oppositeDir(left, right).
p_oppositeDir(right, left).
