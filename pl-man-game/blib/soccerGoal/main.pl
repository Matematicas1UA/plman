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
% soccerGoal
%
% Controlls the behaviour of a soccer goal where the user
% can score goals launching balls. It creates a scoreboard
% for each goal, and that scoreboard uniquelly identifies 
% the goal.
%
% Initialization
%--------------------
%  soccerGoal(init, OID_SB, MAXGOALS, L_BALLAP, L_GOALOIDS, L_PARAMS)
%   OID_SB:   OID of the object to be considered the scoreboard
%   MAXGOALS: Number of goals to be scored until the 'match' ends (9 is Maximum)
%   L_BALLAP: Possible Appearances of the BALLs
%   L_GOALOIDS: List of OIDS of the objects to be considered part of the goal
%   L_PARAMS: List of parameters to modify standard behaviour
%
% Example
%--------------------
%
%  createGameEntity(OID_S, '0', object, 0, 2, active, soccerGoal, 
%      data(scoreboard, solid, static, norule, 'Scoreboard that cumulates goals')), 
%  createGameEntity(OID_G1, '|', object, 9, 1, inactive, norule, 
%      data(goal, solid, static, norule, 'Goal net')), 
%  createGameEntity(OID_G2, '|', object, 9, 2, inactive, norule, 
%      data(goal, solid, static, norule, 'Goal net')), 
%  soccerGoal(init, OID_S, 3, ['o'], [OID_G1, OID_G2], []),
%
%
% Creates a Goal with 2 objects acting as net to retain balls
% (OID_G1 and OID_G2) and a scoreboard which will mark results.
% If 3 goals are scored, the 2 objects that represent the
% goal will disappear. Objects represented by Appearance 'o'
% are considered to be balls.
% BEWARE! the object representing the scoreBoard must be
% active and have soccerGoal as control Rule.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(soccerGoal, [ soccerGoal/1, soccerGoal/6 ]).
:- dynamic d_sgStatus/6.

% Initialization
%
soccerGoal(init, OID_SB, MAXGOALS, L_BALLAP, L_GOALOIDS, L_PARAMS):- 
	number(OID_SB), number(MAXGOALS), MAXGOALS =< 9,
	is_list(L_BALLAP), is_list(L_GOALOIDS), is_list(L_PARAMS), 
	retractall(d_sgStatus(OID_SB, _, _, _, _)), 
	assert(d_sgStatus(OID_SB, 0, MAXGOALS, L_BALLAP, L_GOALOIDS, L_PARAMS)), !.
soccerGoal(init, OID_SB, _, _, _, _):-
        'pl-man':lang_message(soccerGoal, bad_initialization, MSG),
	maplist(user:write, [MSG, OID_SB, '\n']).


% Control Rule (To be used by the scoreboard as controller)
%
soccerGoal(OID):-
	d_sgStatus(OID, GOALS, MAXG, L_BALL, L_GOAL, L_PARAMS),
	checks(OID, GOALS, MAXG, L_BALL, L_GOAL, L_PARAMS).
soccerGoal(OID):-
        'pl-man':lang_message(soccerGoal, scoreboard_not_initializated, MSG),
	maplist(user:write, [MSG, '( OID soccerGoal: ', OID, ')\n']).

checks(OID, G, MG, _, L_GOAL, _):-
	G >= MG, 
	forall(member(GOAL, L_GOAL), 'pl-man':destroyGameEntity(GOAL)),
	'pl-man':deactivateEntity(OID), 
        'pl-man':lang_message(soccerGoal, match_end, MSG),
	user:writeln(MSG), !.
checks(_, _, _, L_BALL, L_GOAL, _):-
	forall(member(GOAL, L_GOAL), not((member(Ap, L_BALL), 'pl-man':entityLocation(_, X, Y, Ap), 'pl-man':entityLocation(GOAL, X, Y, _)))), !.
checks(OID, _, _, L_BALL, L_GOAL, _):-
	forall((member(Ap, L_BALL), 'pl-man':entityLocation(BID, X, Y, Ap), member(GOAL, L_GOAL), 'pl-man':entityLocation(GOAL, X, Y, _)), 
		('pl-man':destroyGameEntity(BID), scoreGoal(OID))).

% Score a new goal
%
scoreGoal(OID):-
	retract(d_sgStatus(OID, G, MG, L_B, L_G, L_P)),
	NewG is G + 1,
	assert(d_sgStatus(OID, NewG, MG, L_B, L_G, L_P)),
	char_code('0', Code0),
	CodeNewG is Code0 + NewG,
	char_code(AP, CodeNewG),
	'pl-man':changeEntityAppearance(OID, AP, _),
        'pl-man':lang_message(soccerGoal, goal, MSG),
	user:writeln(MSG).
