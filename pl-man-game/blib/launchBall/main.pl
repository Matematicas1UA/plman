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
% launchBall
%
% Controlls the behaviour of a ball that can be caught and
% launched. This ball will get bursted on the event of 
% touching something solid. Also, the ball will get caught
% by every mortal entity touching it.
%
% Includes sub-rules
%		launchBall(autoBallCreator)
%
% Initialization
%--------------------
%  launchBall(init, OID_B, L_PARAMS)
%   OID_SB:   OID of the ball
%   L_PARAMS: List of parameters to modify standard behaviour
%
% Example
%--------------------
%
%  createGameEntity(OID_B, 'o', object, 2, 2, inactive, launchBall, 
%     data(ball, not_solid, not_static, launchBall, 'ball to be shot at goal')), 
%  launchBall(init, OID_B, []).
%
% Creates a ball that can be caught and shot by an entity.
% BE CAREFUL! It is very important to create the ball as
% an INACTIVE object. If not, the ball will try to move
% before being launched.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% launchBall(autoBallCreator)
%
% Creates new balls and places them on the field when
% they are necessary (when other balls have been used
% or disappear). 
%
% Initialization
%--------------------
%  launchBall(autoBallCreator, SIMULBALLS, MAXBALLS, X, Y)
%     SIMULBALLS: Number of simultaneous balls that should be in the field
%     MAXBALLS:   Maximum number of new balls to create
%     X, Y:       Coordinates where new balls should be created (they could be random)
%
% Example
%--------------------
%  launchBall(autoBallCreator, 1, 10, 5, 5).
%
%  Creates a new autoBallCreator that will constantly supervise
% the number of active balls present on the field. 
% Every time that the number of present balls in the field
% is less than 1, it will create a new ball. At most, 
% it will create 10 new balls. All the balls will be
% created at the position 5, 5.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(launchBall, [ launchBall/1, launchBall/3, launchBall/5 ]).

% d_ballStatus(OID_BALL, VX, VY, LIFE)
:- dynamic d_ballStatus/4.
:- dynamic d_ballCreator/5.

%%
%% Initialization
%%

% Ball that can be launched
%
launchBall(init, OID_B, L_PARAMS):-
	number(OID_B), is_list(L_PARAMS),
	retractall(d_ballStatus(OID_B, _, _, _)),
	assert(d_ballStatus(OID_B, 0, 0, 1)), 
        !.
launchBall(init, OID_B, _):-
        'pl-man':lang_message(launchBall, bad_parameters, MSG),
	maplist(user:write, [MSG, OID_B, '\n']).

% AutoBallCreator
%
launchBall(autoBallCreator, SIMULBALLS, MAXBALLS, X, Y):-
	not(d_ballCreator(_,_,_,_,_)),
	integer(SIMULBALLS), integer(MAXBALLS),
        'pl-man':lang_message(launchBall, autoBallCreator_description, DESC),
        'pl-man':lang_message(launchBall, autoBallCreator_name, NAME),
	'pl-man':createGameEntity(OID_BC, '', object, -1, -1, active, launchBall:createNewBall, 
			[name(NAME), solid(false), static(true), use_rule(norule), description(DESC)]),
	assert(d_ballCreator(OID_BC, SIMULBALLS, MAXBALLS, X, Y)), 
        !.
launchBall(autoBallCreator, _, _, _, _):-
        'pl-man':lang_message(launchBall, bad_parameters_autoBallCreator, DESC),
	user:writeln(DESC).
	
%%
%% Control
%%
 
% Use Rule
%
launchBall(OID, EID, _, _, DIR):-
	retract(d_ballStatus(OID, _, _, LIFE)),
	p_velocity(DIR, VX, VY),
	assert(d_ballStatus(OID, VX, VY, LIFE)),
	'pl-man':dropObjectIfEntityHas(EID),
	'pl-man':activateEntity(OID),
	moveBall(OID),
        'pl-man':lang_message(launchBall, ball_launched, MSG),
        'pl-man':lang_message(launchBall, DIR, DIRMSG),
	maplist(user:write, [MSG, DIRMSG, '\n']), 
        !.
launchBall(OID, _, _, _, _):-
        'pl-man':lang_message(launchBall, bad_instantiation, MSG),
	maplist(user:write, [MSG, OID, '\n']), 
        !.

% Control Rule
%
launchBall(OID):-
	retract(d_ballStatus(OID, _, _, 0)),
	'pl-man':destroyGameEntity(OID), 
        'pl-man':lang_message(launchBall, ball_puncture, MSG),
	user:writeln(MSG), 
        !.
launchBall(OID):-
	'pl-man':isObjectGot(OID),
	'pl-man':deactivateEntity(OID), !.
launchBall(OID):-
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':entityLocation(OID_2, X, Y, OID_2_AP),
	'pl-man':entityType(OID_2, mortal),
	'pl-man':destroyGameEntity(OID),
	retract(d_ballStatus(OID, _, _, _)),
        'pl-man':lang_message(launchBall, ball_stopped, MSG),
	maplist(user:write, [MSG, OID_2_AP, '!\n']), 
        !.
launchBall(OID):-
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':getDMap(Map),
	( 'pl-man':collision(X, Y, Map);
	  'pl-man':entityLocation(EID_2, X, Y, _), 
	  EID_2 \= OID, 
	  'pl-man':solidEntity(EID_2)), 
	decreaseLife(OID), !.
launchBall(OID):-
	moveBall(OID).


% AutoBallCreation rule
%
createNewBall(OID_BC):-
	d_ballCreator(OID_BC, _, 0, _, _),
	'pl-man':deactivateEntity(OID_BC), !.
createNewBall(OID_BC):-
	d_ballCreator(OID_BC, SIMULBALLS, MAXBALLS, X, Y),
	p_deleteNonExistingBalls(NUM),
	NUM < SIMULBALLS, 
        'pl-man':lang_message(launchBall, ball_name, NAME),
        'pl-man':lang_message(launchBall, ball_description, DESC),
	'pl-man':createGameEntity(OID_BALL, 'o', object, X, Y, inactive, launchBall, 
			[name(NAME), solid(false), static(false), use_rule(launchBall), 
			 description(DESC), appearance(attribs(bold, cyan, default))]),
	launchBall(init, OID_BALL, []), 
	retract(d_ballCreator(OID_BC, _, _, _, _)),
	NEW_MAXBALLS is MAXBALLS - 1,
	assert(d_ballCreator(OID_BC, SIMULBALLS, NEW_MAXBALLS, X, Y)),
	( MAXBALLS > 0,
          'pl-man':lang_message(launchBall, balls_left, MSG),
	  maplist(user:write, [MSG, NEW_MAXBALLS, '\n'])
	; true
	), !.
createNewBall(_).


%%
%% Subrules
%%	

% Conversions from Directions to velocities
%
p_velocity(right, 1, 0).
p_velocity(left, -1, 0).
p_velocity(down, 0, 1).
p_velocity(up, 0, -1).
p_velocity(up-left, -1, -1).
p_velocity(up-right, 1, -1).
p_velocity(down-right, 1, 1).
p_velocity(down-left, -1, 1).

% Decreasing life of the ball
%
decreaseLife(OID):-
	retract(d_ballStatus(OID, _, _, LIFE)),
	NewLIFE is LIFE - 1,
	assert(d_ballStatus(OID, _, _, NewLIFE)).

% To move the ball
%
moveBall(OID):-
	d_ballStatus(OID, VX, VY, _),
	'pl-man':entityLocation(OID, X, Y, _),
	'pl-man':getDMap(M),
	X1 is X + VX, Y1 is Y + VY,
	'pl-man':moveEntity(OID, M, X1, Y1).

% Delete all the facts that controlled balls that do not exist anymore
%
p_deleteNonExistingBalls(NUM):-
	C = c(0),
	forall(d_ballStatus(EID, _,_,_), (
		not('pl-man':entityLocation(EID,_,_,_)), 
		retract(d_ballStatus(EID,_,_,_)) 
		;
		arg(1,C,N), 
		N1 is N+1, 
		nb_setarg(1, C, N1)
	)),
	C = c(NUM).

