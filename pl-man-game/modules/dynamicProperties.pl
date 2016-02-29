%
% dynamicProperties is part of cheeseTools, a set of utility rules and primitives 
% for simplifying common tasks in PROLOG language. This concrete module is for 
% creating and maintaining lists of properties that may vary in real-time.
%
% Copyright (C) 2007-2010 Francisco Gallego (ronaldo/Cheesetea) <ronaldo@cheesetea.com>
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
:- module(dynamicProperties, [dynamicProperties/1]).

%% Next ID of list of properties, and list of properties dynamic predicates
:- dynamic d_propListNextID/1.
:- dynamic d_propList/2.

%************************************************************************************
% dynamicProperties(reset)
%  Resets all the dynamic predicates associated to lists of properties
%************************************************************************************
dynamicProperties(reset):-
	retractall(d_propList(_,_)),
	retractall(d_propListNextID(_)),
	assert(d_propListNextID(0)).

%************************************************************************************
% dynamicProperties(checkExistence(+PLID))
%     PLID: Identifier of the list
%
% Succeed if PLID is a valid List identifier. Fails in other case.
%
% dynamicProperties(checkPropertiesList(+PLIST))
%     PLIST: A list of properties (predicates of arity 1)
%
% Succeeds if PLIST is a valid list of properties (every element is a predicate
% of arity 1).
%************************************************************************************
dynamicProperties(checkExistence(PLID)):-
	d_propList(PLID, _).
dynamicProperties(checkPropertiesList(PLIST)):-
    is_list(PLIST),
    forall( member(M, PLIST),
            functor(M, _, 1) ).

%************************************************************************************
% dynamicProperties(create(-PLID, +L_PROPS))
% dynamicProperties(create(-PLID))
%     PLID: Identifier of the list
%     L_PROPS: List of properties (functors of arity 1)
%
% Creates a new managed list of properties
%************************************************************************************
dynamicProperties(create(PLID, L_PROPS)):-
	dynamicProperties(create(PLID)),
	dynamicProperties(addList(PLID, L_PROPS)), !.
dynamicProperties(create(PLID)):-
	var(PLID),
	retract(d_propListNextID(PLID)),
	NextPLID is PLID+1,
	assert(d_propListNextID(NextPLID)),
	retractall(d_propList(PLID, _)),
	assert(d_propList(PLID, [])), !.
dynamicProperties(create(PLID)):-
	maplist(system:write, ['ERROR: Properties Lists not initialized or bad parameter for dynamicProperties(create(', PLID, ')).\n',
		'Parameter must be a non instantiated variable.\n']),
	abort.

%************************************************************************************
% dynamicProperties(destroy(+PLID))
%     PLID: Identifier of the list
%
% Destroys a managed list of properties, releasing some memory space :)
%************************************************************************************
dynamicProperties(destroy(PLID)):-
	number(PLID),
	d_propList(PLID, _),
	retractall(d_propList(PLID, _)), !.
dynamicProperties(destroy(PLID)):-
	maplist(system:write, ['ERROR: Bad identifier for dynamicProperties(destroy(', PLID, ')).\n',
		'Either it is not a number or it does not correspond to a valid set of managed list of properties.\n']),
	abort.

%************************************************************************************
% dynamicProperties(add(PLID, +P))
% dynamicProperties(addList(PLID, +LNEWP))
%    PLID:  Identifier of the properties list
%    P:     New property to add (it must be a functor of arity 1)
%    LNEWP: List of new properties to add to the properties list
%
%  Adds a new property/a new list of properties to the list of properties checking 
% that properties added were not there before.
%************************************************************************************
dynamicProperties(add(PLID, P)):-
	functor(P, PNAME, 1),
	retract(d_propList(PLID, PL)),
	((member(X, PL), functor(X, PNAME, 1))
	-> NewPL = PL
	 ; append([P], PL, NewPL) 
	),
	assert(d_propList(PLID, NewPL)), !.
dynamicProperties(add(PLID, P)):-
	maplist(system:write, ['ERROR: bad parameters for dynamicProperties(add(',PLID,',',P,')).\n',
			'This rule requires an valid identifier of Parameter list (PLID, a number) as 1st parameter, and a functor of arity 1 as 2nd parameter.\n']),
	abort.
dynamicProperties(addList(PLID, LNEWP)):-
	is_list(LNEWP),
	forall(member(P, LNEWP), dynamicProperties(add(PLID, P))), !.
dynamicProperties(addList(PLID, LNEWP)):-
	maplist(system:write, ['ERROR: bad parameters for dynamicProperties(addList(',PLID,',',LNEWP,')).\n',
			'This rule requires an valid identifier of Parameter list (PLID, a number) as 1st parameter, and a list of functors of arity 1 as 2nd parameter.\n']),
	abort.

%************************************************************************************
% dynamicProperties(get(PLID, +PROP))
% dynamicProperties(get(PLID, +P, +V))
%    PLID:  Identifier of the properties list
%    PROP:  functor of arity 1 representng the property to get
%    P:     Name of the property to get
%    V:     Value of the property
%
%  Gets the value of the given property from the properties list
%************************************************************************************
dynamicProperties(get(PLID, PROP)):-
	d_propList(PLID, PL),
	functor(PROP, _, 1),
	member(PROP, PL), !.
dynamicProperties(get(PLID, P, V)):-
	d_propList(PLID, PL),
	member(X, PL),
	functor(X, P, 1), 
	arg(1, X, V), !.

%************************************************************************************
% dynamicProperties(set(PLID, +PROP))
% dynamicProperties(set(PLID, +P, +V))
%    PLID:  Identifier of the properties list
%    PROP:  functor of arity 1 representing the property to set, with its new value
%    P:     Name of the property to get
%    V:     New value of the property that will overwrite previous one
%
%  Sets a new value for a given property
%************************************************************************************
dynamicProperties(set(PLID, PROP)):-
	functor(PROP, P, 1),
	arg(1, PROP, V),
	dynamicProperties(set(PLID, P, V)), !.
dynamicProperties(set(PLID, PROP)):-
	maplist(system:write, ['ERROR: bad parameters for dynamicProperties(set(',PLID,',',PROP,')).\n',
			'This rule requires an valid identifier of Parameter list (PLID, a number) as 1st parameter, and a valid property functor of arity 1 that currently exist in the list as 2nd parameter.\n']),
	abort.	

dynamicProperties(set(PLID, P, V)):-
	d_propList(PLID, PL),
	functor(FIND_P, P, 1),
	functor(NEW_P, P, 1), 
	arg(1, NEW_P, V), 
	append(Before, [FIND_P|After], PL),
	append(Before, [NEW_P|After], NEW_PL), 
	retractall(d_propList(PLID,_)),
	assert(d_propList(PLID, NEW_PL)), !.

dynamicProperties(set(PLID, P, V)):- 
	maplist(system:write, ['ERROR: bad parameters for dynamicProperties(set(',PLID,',',P,',',V,')).\n',
			'This rule requires an valid identifier of Parameter list (PLID, a number) as 1st parameter, an atom with a valid name for an existing property of the list as 2nd parameter, and the new value of the property as 3rd parameter.\n']),
	abort.

%************************************************************************************
% dynamicProperties(setOrAdd(PLID, +PROP))
% dynamicProperties(setOrAdd(PLID, +P, +V))
%    PLID:  Identifier of the properties list
%    PROP:  functor of arity 1 representing the property to set, with its new value
%    P:     Name of the property to get
%    V:     New value of the property that will overwrite previous one
%
%  Sets a new value for a given property, or creates the property if it does not exist
%************************************************************************************
dynamicProperties(setOrAdd(PLID, PROP)):-
	functor(PROP, P, 1),
	arg(1, PROP, V),
	dynamicProperties(setOrAdd(PLID, P, V)), !.
dynamicProperties(set(PLID, PROP)):-
	maplist(system:write, ['ERROR: bad parameters for dynamicProperties(setOrAdd(',PLID,',',PROP,')).\n',
			'This rule requires an valid identifier of Parameter list (PLID, a number) as 1st parameter, and a valid property functor of arity 1 as 2nd parameter.\n']),
	abort.	

dynamicProperties(setOrAdd(PLID, P, V)):-
    dynamicProperties(get(PLID, P, _)), !,
    dynamicProperties(set(PLID, P, V)).
dynamicProperties(setOrAdd(PLID, P, V)):-
    dynamicProperties(add(PLID, P, V)).
