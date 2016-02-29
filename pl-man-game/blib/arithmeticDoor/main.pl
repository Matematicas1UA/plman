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
% arithmeticDoor
%
% Implements the behaviour of a door that has an arithmetic
% operation written in. To open the door it is necessary to
% use in it an object with the appearance of the number that
% is the result of the arithmetic operation.
%
% Includes a subrule for the creation of a pool of random unuseful
% keys randomly placed (arithmeticDoor(createRandomKeys...))
% 
%
% Initialization
%--------------------
%  arithmeticDoor(init, OID_OP, OID_N1, OID_N2, -RESULT, L_PARAMS):-
%    OID_OP: Identifier of the operator object
%    OID_N1: Identifier of the object that represents the number 1
%    OID_N2: Identifier of the object that represents the number 2
%    RESULT: This parameter will be unified with the result of the 
%            random operation for the arithmetic door (i.e. the key value).
%    L_PARAMS: Params to control the way the door behaves
%        operators(LIST): Only use the operators in the LIST
%
% Creation of a pool of random unuseful keys
%--------------------------------------------
%  arithmeticDoor(createRandomKeys, OID_OP, NUMKEYS, location(X, Y), -L_KEYS, L_PARAMS)
%    OID_OP:  Identifier of the operator object of the door the keys are for
%    NUMKEYS: Number of random unuseful keys to create
%    X, Y:    Coordinates where to create the keys (they could be rnd(A,B) and rnd(LIST))
%    L_KEYS:  List of OIDs of the randomly created keys
%    L_PARAMS: Params to control the way the random creator behaves
%        distributed: It ensures that every key created is located in a free place 
%                     (i.e. a place where no other object was placed before)
%        add_properties(LIST): The list given is added to the list of properties
%                     of every new key created.
%
% Example
%--------------------
%  createGameEntity(OID_OP, '+', object, 5, 2, inactive, norule, 
%	[name(operator), solid(true), static(true), use_rule(norule), description('Operand door') ]),
%  createGameEntity(OID_N1, 1, object,  4, 2, inactive, norule, 
%	[name(number1), solid(true), static(true), use_rule(norule), description('First Operand') ]),
%  createGameEntity(OID_N2, 2, object,  6, 2, inactive, norule, 
%	[name(number2), solid(true), static(true), use_rule(norule), description('Second Operand') ]),
%
%  arithmeticDoor(init, OID_OP, OID_N1, OID_N2, RES, [operators(['+', '-', '/'])]),
%	arithmeticDoor(createRandomKeys, OID_OP, 4, location(rnd(1,16), rnd(5, 7)), _,
%      [ distributed, add_properties([appearance(attribs(bold, cyan, default))]) ]),
%
%  createGameEntity(OID_KEY, RES, object, 10, 6, inactive, norule,
%	[name(llave_aritmetica), solid(false), static(false), use_rule(arithmeticDoorKey),
%    description('Objeto llave para puertas aritmeticas') ]).
% 
%   Creates one arithmetic Door that will appear with
% a random operation to solve. The random operation will
% not include a times sign (*), because the election is
% limited to +,-,/ as it is stated by operators/1 parameter.
% Before creating the arithmeticDoor, we create a valid 
% key to open it.
%
% It also creates a pool of 4 random unuseful keys, all
% of them placed randomly at the coordinate interval
% ([1,16], [5,7]). This keys will be coloured cyan and
% no pair of two randomly created keys will be placed
% in the same cell (because of the "distributed" param)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(arithmeticDoor, [arithmeticDoor/6, arithmeticDoorKey/5]).
:- dynamic d_doorParam/1.
:- dynamic d_doorStatus/6.

%%%
%%% Initialization
%%%
% init a mine
arithmeticDoor(init, OID_OP, OID_N1, OID_N2, RES, L_PARAMS):-
	integer(OID_OP), integer(OID_N1), integer(OID_N2),
	is_list(L_PARAMS),
	retractall(d_doorParam(_)),
	retractall(d_doorStatus(_, _, _, _, _, _)),
	assert(d_doorStatus(OID_OP, OID_N1, OID_N2, 0, '+', 0)), 
	maplist(p_assertParam(d_doorParam), L_PARAMS),
	p_randomizeDoorOperand(OID_OP),
	p_randomizeDoorNumbers(OID_OP),
	d_doorStatus(OID_OP, _, _, N1, OP, N2),
	p_calculate(N1,OP,N2,RES), !.
arithmeticDoor(init, OID_OP, _, _, _, _):-
        'pl-man':lang_message(arithmeticDoor, bad_parameters, BPMSG),
	maplist(user:write, [BPMSG, OID_OP, '\n']).

%%%
%%% Support rule for creating random keys
%%%
arithmeticDoor(createRandomKeys, OID_OP, NUMKEYS, location(X, Y), L_KEYS, L_PARAMS):-
	integer(NUMKEYS), is_list(L_PARAMS), var(L_KEYS),
	d_doorStatus(OID_OP, _, _, N1, OP, N2),
        'pl-man':lang_message(arithmeticDoor, key_object_description, KODESC),
	(member(add_properties(L_EXTRAPAR), L_PARAMS) ; L_EXTRAPAR = []),
	append([name(llave_aritmetica), solid(false), static(false), use_rule(arithmeticDoorKey), 
		description(KODESC) ], L_EXTRAPAR, L_KEY_PARAMS),
	p_calculate(N1, OP, N2, KEYVAL),
	arithmeticDoor(createRandomKeys, NUMKEYS, KEYVAL, X, Y, L_KEY_PARAMS, L_KEYS, [], L_PARAMS), !.
arithmeticDoor(createRandomKeys, OID_OP, _, _, _, _):-
        'pl-man':lang_message(arithmeticDoor, bad_parameters_randomkeys, BPMSG),
	maplist(user:write, [BPMSG, OID_OP,'\n']), !.

% Recursive support rule for creating randomkeys
%
arithmeticDoor(createRandomKeys, 0, _, _, _, _, L_KEYS, L_KEYS, _):-!.
arithmeticDoor(createRandomKeys, NUMKEYS, KEYVAL, X, Y, L_KEY_PARAMS, L_KEYS, L_NOWKEYS, L_PARAMS):-
	repeat, VAL is random(10), VAL \= KEYVAL,
	'pl-man':createGameEntity(OID_KEY, VAL, object, X, Y, inactive, norule, L_KEY_PARAMS),
	(  member(distributed, L_PARAMS)
	-> not('pl-man':checkSamePlaceAsOtherObjectAndDestroy(OID_KEY))
	 ; true ),
	NEWNUMKEYS is NUMKEYS - 1,
	append(L_NOWKEYS, [ OID_KEY ], NEWL_NOWKEYS),
	arithmeticDoor(createRandomKeys, NEWNUMKEYS, KEYVAL, X, Y, L_KEY_PARAMS, L_KEYS, NEWL_NOWKEYS, L_PARAMS).
	
%%%
%%% Control
%%%

%% Use of an keyNumber to try to open an arithmeticDoor
arithmeticDoorKey(_, EID, _, _, _):-
	not('pl-man':entityType(EID, pacman)), !.
arithmeticDoorKey(OID, _, X, Y, _):-
	not(('pl-man':entityLocation(EID, X, Y, _),
	     d_doorStatus(EID,_,_,_,_,_) )),
	'pl-man':entityLocation(OID, _,_,AP),
        'pl-man':lang_message(arithmeticDoor, cannot_use_object_there, MSG),
	maplist(user:write, ['(', AP, '): ', MSG, '\n']), !.
arithmeticDoorKey(OID, _, X, Y, _):-
	'pl-man':entityLocation(EID, X, Y, _),
	'pl-man':entityLocation(OID, _, _, AP),
	d_doorStatus(EID, _, _, N1, OP, N2),
	p_calculate(N1, OP, N2, AP),
	'pl-man':destroyGameEntity(EID),
        'pl-man':lang_message(arithmeticDoor, door_open, MSG),
	maplist(user:write, ['(', AP, '): ', MSG, '\n']), !.
arithmeticDoorKey(OID, _, _, _, _):-
	'pl-man':entityLocation(OID, _, _, AP),
        'pl-man':lang_message(arithmeticDoor, wrong_key, MSG),
	maplist(user:write, ['(', AP, '): ', MSG, '\n']), !.

%%%
%%% Subrules
%%%

% Make assertions type F(P) knowing functor-name F and param P
p_validParam(operators(L)):- is_list(L), forall(member(M, L), member(M, ['+', '-', '*', '/'])).
p_assertParam(F, P):- 
	p_validParam(P),
	functor(FF, F, 1), 
	arg(1, FF, P), 
	assert(FF).
p_assertParam(_, P):-
	with_output_to(string(S), write_term(P, [])),
        'pl-man':lang_message(arithmeticDoor, error_parameter_invalid, MSG),
	maplist(user:write, ['(', S,'): ', MSG, '\n' ]),
	fail.

% Pick up random operator for constructing the door
p_randomizeDoorOperand(_):-
	not(d_doorParam(operators(_))),
	assert(d_doorParam(operators(['+', '-', '*', '/']))),
	fail.
p_randomizeDoorOperand(OID_OP):-
	d_doorParam(operators(L_OP)),
	'pl-man':randomFromList(OP, L_OP),
	'pl-man':changeEntityAppearance(OID_OP, OP, _),
	retract(d_doorStatus(_, OID_N1, OID_N2, N1, _, N2)),
	assert(d_doorStatus(OID_OP, OID_N1, OID_N2, N1, OP, N2)).
% Pick up random numbers for constructing the door
p_randomizeDoorNumbers(OID_OP):-
	d_doorStatus(OID_OP, OID_N1, OID_N2, _, OP, _),
	N1 is random(10),
	numlist(0, 9, LN),
	'pl-man':randomPermutation(LN, PLN),
	member(N2, PLN),
	p_calculate(N1, OP, N2, RES),
	RES >= 0, RES =< 9,
	retract(d_doorStatus(OID_OP, _, _, _, _, _)),
	assert(d_doorStatus(OID_OP, OID_N1, OID_N2, N1, OP, N2)),
	'pl-man':changeEntityAppearance(OID_N1, N1, _),
	'pl-man':changeEntityAppearance(OID_N2, N2, _), !.

% p_calculate results of operations
p_calculate(N1, '+', N2, RES):- RES is N1 + N2, !.
p_calculate(N1, '*', N2, RES):- RES is N1 * N2, !.
p_calculate(N1, '-', N2, RES):- RES is N1 - N2, !.
p_calculate( _, '/',  0, _):- !, fail.
p_calculate(N1, '/', N2, RES):- 
	RES is N1 /  N2, 
	RES is N1 // N2, !.
	

