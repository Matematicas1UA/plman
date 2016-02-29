%
% cheeseTools is a set of utility rules and primitives for simplifying 
% common tasks in PROLOG language. It includes higher level data structures
% suitable for many diferent tasks.
%
% Copyright (C) 2007-2014 Francisco Gallego (ronaldo/Cheesetea) <ronaldo@cheesetea.com>
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

%%
%%  MODULO: cheeseTools
%%	
%%	Utilidades diversas
%%	
:- module(cheeseTools, 
	[randomBetween/3, randomFromList/2, randomPermutation/2, subList/4, 
	stringListConcat/2, repeatNTimes/2, replaceNth0/4, dynamicProperties/1, 
	logToFile/1, maxOfList/2, minOfList/2, countClauses/2, shiftListLeft/3, 
	shiftListRight/3, removeFromList/3, appendRepeatNTimes/4, removeNth0/4, 
    getParamFromList/4, checkMandatoryParameters/3, showLangMessage/3,
    checkListElementsType/2, manhattanDistance/3, atomMap2charMap/2]).

:- use_module('dynamicProperties').

%************************************************************************************
% atomMap2charMap(AT, CH)
%     +AT: List of atoms, each one representing a list of characters
%     -CH: List of characters, extracted from the atoms 
%
% It transforms a list of atoms into a matrix of characters
%************************************************************************************
atomMap2charMap(AMAP, CMAP):-
	atomMap2charMap(AMAP, [], CMAP), !.
atomMap2charMap([], CHM, CHM):- !.
atomMap2charMap([A | AL], CL, CHM):-
	atom_chars(A, ACHARS),
	append(CL, [ACHARS], NCL),
	atomMap2charMap(AL, NCL, CHM).

%************************************************************************************
% manhattanDistance(+p(X1, Y1), +p(X2, Y2), -D)
%     +p(X, Y): Points to use in measuring their distance
%     -D:       Manhattan Distance between the points 
%
% It unifies D with the manhattan distance between the two given points.
%************************************************************************************
manhattanDistance(p(X1, Y1), p(X2, Y2), D):-
    D is abs(X1-X2) + abs(Y1-Y2).

%************************************************************************************
% checkListElementsType(+LIST, +TYPELIST)
%     +LIST:     List of elements to be checked against type
%     +TYPELIST: List of callable type checkers that will be called against each element 
%                of the list to see if one of them succeeds
%
% It calls a given list of functor type checkers (like integer/1 or is_list/1) against all 
% the elements in a given list. It succeeds if no one of the elements fails againts
% all the type-checking calls.
%************************************************************************************
checkListElementsType([], _):- !.
checkListElementsType([E | L], TYPELIST):-
    member(TYPE, TYPELIST),
    call(TYPE, E), !,
    checkListElementsType(L, TYPELIST).

%************************************************************************************
% showLangMessage(+MODULE, +MESSAGE, +LIST)
%     +MANDPARAMLIST: Module that provides the error message
%     +MESSAGE:       Atom that identifies the concrete error message
%     +LIST:          List of additional informations to print at the end of the message
%
% Prints a multilanguage message using 'pl-man':lang_message facts. These 
% facts should be provided by each behaviour or module in different languages.
%************************************************************************************
showLangMessage(MODULE, MESSAGE, LIST):-
    not(is_list(LIST)), !,
    maplist(user:write, [ 'ERROR: showErrorMessage requires list as 3rd parameter (', MODULE, ', ', MESSAGE, ', ', LIST, ')\n' ]).
showLangMessage(MODULE, MESSAGE, LIST):-
    'pl-man':lang_message(MODULE, MESSAGE, TEXT),
    append(LIST, [ '\n' ], LISTN),
    append([ TEXT ], LISTN, PRINTLIST), 
    maplist(user:write, PRINTLIST), !.
showLangMessage(MODULE, MESSAGE, LIST):-
    maplist(user:write, [ 'ERROR: Message not found (', MODULE, ', ', MESSAGE, ', ', LIST, ')\n' ]).

%************************************************************************************
% checkMandatoryParameters(+MANDPARAMLIST, +PARAMS, +CALLERNAME)
%     +MANDPARAMLIST: List of required mandatory parameters with their type checking
%                     predicates
%     +PARAMS:        List of received parameters for checking
%     +CALLERNAME:    Name of the rule that calls. This is for printing error messages
%
% Given a list of parameters in the form [ p1(V1,V2,...,Vn), p2(V1,V2,...Vn)...]
% it checks if there exists a subset of mandatory parameters, and that these 
% mandatory parameters have the required arguments that accomplish some given 
% predicates (i.e. being lists or being integers)
% This rule prints out error messages when params are not found or do not cover
% required requisites.
%************************************************************************************
checkMandatoryParameters([], _, _):- !.
checkMandatoryParameters([P | MORE], PARAMS, CALLERNAME):-
    checkMandatoryParameters(checkParam, P, PARAMS, CALLERNAME),
    checkMandatoryParameters(MORE, PARAMS, CALLERNAME).
checkMandatoryParameters(checkParam, P, PARAMS, CALLERNAME):-
    functor(P, NAME, ARITY),
    functor(NP, NAME, ARITY),
    not(member(NP, PARAMS)),
    maplist(user:write, ['ERROR: ',CALLERNAME,' lacks mandatory param ', NAME, '/', ARITY, '\n']), !.
checkMandatoryParameters(checkParam, P, PARAMS, CALLERNAME):-
    functor(P, NAME, ARITY),
    functor(NP, NAME, ARITY),
    member(NP, PARAMS),
    forall(arg(N, P, PROP), checkMandatoryParameters(checkParamArgument, NP, N, PROP, CALLERNAME)).
checkMandatoryParameters(checkParamArgument, FUNCWITHARGS, ARGN, CHECKTYPE, _):-
    functor(TOCHECK, CHECKTYPE, 1), 
    arg(ARGN, FUNCWITHARGS, VALUE), 
    arg(1, TOCHECK, VALUE), 
    call(TOCHECK).
checkMandatoryParameters(checkParamArgument, FUNCWITHARGS, ARGN, CHECKTYPE, CALLERNAME):-
    functor(FUNCWITHARGS, NAME, ARITY),
    maplist(user:write, ['ERROR: ',CALLERNAME,' argument ', ARGN,' from parameter ',NAME,'/',ARITY,' fails at type checking (',CHECKTYPE,').\n']),
    fail.

%************************************************************************************
% getParamFromList(-VALUE, PNAME, DEFAULT, LIST)
%    VALUE:   Return Value 
%    PNAME:   Name of the parameter to get
%    DEFAULT: Default value to return when param does not exist
%    LIST:    List where to look for the parameter
%
% Searches through a list to find a param in the form param(value) and get the value
%************************************************************************************
getParamFromList(VALUE, PNAME, _, LIST):-
    functor(F, PNAME, 1),
    member(F, LIST),
    arg(1, F, VALUE), !.
getParamFromList(VALUE, _, VALUE, _).

%************************************************************************************
% appendRepeatNTimes(E, T, L, NL)
%    E:  Element add repeated times
%    T:  Times to repeat the Element
%    L:  Initial list where to append new elements
%   NL:  New List with T elements E appended
%
% Creates a new list NL by appending T elements E at the end of the list L.
%************************************************************************************
appendRepeatNTimes(_, T, L,  L):- T =< 0, !.
appendRepeatNTimes(E, T, L, NL):- 
	append(L, [E], PL),
	T1 is T-1,
	appendRepeatNTimes(E, T1, PL, NL).

%************************************************************************************
% removeNth0(N, L, NL)
%    N:  Index of the nth0 element to remove (0=first element)
%    E:  The element that has been removed (if any)
%    L:  List 
%   NL:  New List with Nth0 Element removed
%
%  Creates a new list NL from list L by removing the element at the N position, 
% being 0 the index of the first element.
%************************************************************************************
removeNth0(N, _, L, L) :- (N < 0 ; length(L, LEN), N >= LEN), !.
removeNth0(0, E, [E|REST], REST):- !.
removeNth0(N, E, L, NL):-
	append(PRE, [E|POST], L),
	length(PRE, N),
	append(PRE, POST, NL), !.

%************************************************************************************
% removeFromList(E, L, NL)
%    E:  Element to remove
%    L:  List 
%   NL:  New List with Element E removed
%
%  Creates a new list NL from list L by removing the first occurence of element E
%  in L. If element E is not in L, NL becomes L.
%************************************************************************************
removeFromList(_, [], []):- !.
removeFromList(E, L, NL) :-
	append(PRE, [E|POST], L),
	append(PRE, POST, NL), !.
removeFromList(_, L, L).

%************************************************************************************
% shiftListLeft(L, N, NL)
%    L:  List to shift
%    N:  Number of elements to shift
%   NL:  New Shifted list
%
%  Shifts a list to the left N positions. N should be less than the number of elements
%************************************************************************************
shiftListLeft([], _, []):- !.
shiftListLeft(L, N, NL) :- append(H, T, L), length(H, N), append(T, H, NL), !.

%************************************************************************************
% shiftListRight(L, N, NL)
%    L:  List to shift
%    N:  Number of elements to shift
%   NL:  New Shifted list
%
%  Shifts a list to the right N positions. N should be less than the number of elements
%************************************************************************************
shiftListRight([], _, []):- !.
shiftListRight(L, N, NL) :- append(H, T, L), length(T, N), append(T, H, NL), !.

%************************************************************************************
% countClauses(Template, NUM)
%   Template:  Clause template to unify with all the clauses to count
%   NUM:       Number of total clauses of the Template counted
%
%  Counts the number of clauses that unify with the clause Template given
%************************************************************************************
countClauses(Template,Num):-
       C = c(0),
       forall(nth_clause(Template, N, _), nb_setarg(1, C, N)),
       C = c(Num).

%************************************************************************************
% maxOfList(M, L)
% minOfList(M, L)
%   M:  Maximum/Minimum element
%   L:  List of numbers
%
%  Calculates the maximum/minimum element of a list of numbers
%************************************************************************************
maxOfList(M, L) :- ofList(max, M, L).
minOfList(M, L) :- ofList(min, M, L).
ofList( _, _,  [ ] ):- !, fail.
ofList( _, M, [ M ]):- !.
ofList(OP, M, [I|L]):-
	ofList(OP, M, I, L), !.

% Tail recursive rule ofList/4 for supporting ofList/3
%  ofList(OPERATION, GLOBAL_MAXORMIN, PRESENT_MAXORMIN, REST_OF_THE_LIST)
%
ofList( _, MoM, MoM, []):-!.
ofList(OP, MoM, MP, [M|L]):-
	p_operation(OP, M, MP),
	ofList( OP, MoM, M, L).
ofList(OP, MoM, MP, [_|L]):-
	ofList( OP, MoM, MP, L).  

% Checks for maximum and minimum
p_operation(max, A, B) :- A > B.
p_operation(min, A, B) :- B > A.

%************************************************************************************
% randomPermutation(L, PL)
%   L:  Original list
%   PL: Randomly permuted list
%
%  Generates a random permutation of the list L
%************************************************************************************
randomPermutation([], []):-!.
randomPermutation([ M ], [ M ]):-!.
randomPermutation(L, PL):-
	length(L, LEN_L),
	randomPermutation(L, LEN_L, PL, []).

% randomPermutation(LIST, PERMUTED_LIST, TEMPORAL_PERMUTED_LIST)
%   Tail recursive support rule for randomPermutation/2
%
randomPermutation([], _, PL, PL):-!.
randomPermutation(L, LEN_L, PL, TEMPL):-
	N is random(LEN_L),
	NEW_LEN_L is LEN_L - 1,
	removeNth0(N, ELEM, L, NEW_L),
	append(TEMPL, [ELEM], NEW_TEMPL),
	randomPermutation(NEW_L, NEW_LEN_L, PL, NEW_TEMPL). 
%************************************************************************************
% randomBetween(MIN, MAX, RND)
%   MIN:  Minimum random number to get
%   MAN:  Maximum random number to get
%   RND:  New random number
%
%  Creates a random integer between MIN and MAX (inclusive)
%************************************************************************************
randomBetween(MIN, MAX, RND):-
	RND is (random(MAX - MIN + 1) + MIN).

%************************************************************************************
% randomFromList(RND, LIST)
%   LIST: List of atoms or numbers
%   RND:  A random member of the LIST
%
%  Picks up a random element from the LIST
%************************************************************************************
randomFromList(RND, LIST):-
	length(LIST, LEN),
	ELEM is random(LEN),
	nth0(ELEM, LIST, RND).

%************************************************************************************
% stringListConcat(-ST, +LIST)
%   ST:   String with all the atoms/strings from the list concatenated
%   LIST: List with the elements that are going to be concatenated
%
%  Creates a string by concatenating all the elements from the list
%************************************************************************************
stringListConcat(ST, [], ST):-!.
stringListConcat(ST, [C|L], PART):-
	string_concat(PART, C, NEW),
	stringListConcat(ST, L, NEW).
stringListConcat(ST, LIST):-
	is_list(LIST),
	stringListConcat(ST, LIST, '').

%************************************************************************************
% Regla para obtener sublistas a partir de una dada y un rango de elementos
% subList(L, St, End, SubL)
%   L: Lista
%   St: Índice donde empieza la sublista (incluido)
%   End: Índice donde termina la sublista (incluido)
%   SubL: Sublista obtenida
%************************************************************************************
subList([], _, _, []) :- !.
subList(_, N1, N2, []):- N2 < N1, !.
subList([H|_], 0, 0, [H]) :- !.
subList([H|T], 0, N, [H|SL]) :- 
			N>0, N1 is N-1, 
			subList(T, 0, N1, SL), !.
subList([_|T], N1, N2, SL) :- 
			N1 > 0, N2 > 0,
			N11 is N1-1, N21 is N2-1,
			subList(T, N11, N21, SL), !.

%************************************************************************************
% repeatNTimes(Command, N)
%		Command: Comando que se repetira
%		N: Numero de veces que se debe repetir el comando.
%
% Repite un comando cualquiera N veces consecutivas sin pausa entre ellas.
%************************************************************************************
repeatNTimes(_, N):- N =< 0, !.
repeatNTimes(Command, N):-		       
	N1 is N - 1,
	call(Command),
	repeatNTimes(Command, N1).

%************************************************************************************
% replaceNth0 (N, New, L, NL)
%    N:   Número de elemento de la lista a sustituir (empezando en 0)
%    New: Nuevo elemento a poner en su lugar
%    L:	  Lista que se desea modificar
%    NL:  Lista obtenida después de modificar el elemento indicado
%    
% Sustituye el valor del N-esimo elemento de una lista, empezando en el 0
%************************************************************************************
p_replaceNth0(_, _, [], _):-!, fail.           % Si he llegado a la lista vac�a, es que N > num elementos de la lista
p_replaceNth0(0, New, [_|Mas], [New|Mas]):- !. % Caso base: Modificar el elemento 0 es cambiar la cabeza de la lista
p_replaceNth0(N, New, [C|Mas], [C|ModList]):-  % Caso general: La cabeza se mantiene y sigo buscando el elemento en la lista
	N >= 1,	N1 is N-1,		   % Me aseguro de que N sea positivo
	replaceNth0(N1, New, Mas, ModList), !.  
replaceNth0(N, New, L, NL):-
	number(N), is_list(L),
	length(L, MAX),
	N >= 0, N < MAX, !, 
	p_replaceNth0(N, New, L, NL).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATING AND MAINTAINING LOGFILES
%
%  Rules in this category are designed to create and control writing
% to logfiles.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic d_openedLogs/1.

%************************************************************************************
% logToFile(reset)
%
%  Sets up the log system for starting to log to files
%************************************************************************************
logToFile(reset):-
	d_openedLogs(L_LOGS),
	forall(member(L, L_LOGS), ( (dynamicProperties(checkExistence(L)), dynamicProperties(destroy(L))) ; true ) ),
	fail.
logToFile(reset):-
	retractall(d_openedLogs(_)),
	assert(d_openedLogs([])).

%************************************************************************************
% logToFile(create(LOGID, FILE_NAME))
%   LOGID: Log identifier
%   FILE_NAME: Name of the file to create as log_file
%
%  Creates a new log file. If the file already exists, it gives an error msg.
%************************************************************************************
logToFile(create(LOGID, FILE_NAME)):-
	not(var(LOGID)),
	maplist(write, 
		['ERROR: Bad call to logToFile(create(', LOGID, ',', FILE_NAME, ').',	 
		 'First parameter should be a free variable\n']), !. 
logToFile(create(_, FILE_NAME)):-
	catch(open(FILE_NAME, read, _), _, fail),
	maplist(write, ['WARNING: trying to log to already existing file (',FILE_NAME,')\n']),
	fail.
logToFile(create(_, FILE_NAME)):-
	not(d_openedLogs(_)),
	maplist(write, ['ERROR: Trying to create a log for filename (',FILE_NAME,') without initializing log system.\n']), !.
logToFile(create(LOGID, FILE_NAME)):-
	open(FILE_NAME, write, FILE_ID),
	string_to_atom(CS, ''),
	dynamicProperties(create(LOGID, [cacheString(CS), fileID(FILE_ID), filename(FILE_NAME)])),
	retract(d_openedLogs(L_LOGS)),
	append(L_LOGS, [LOGID], NEW_L_LOGS),
	assert(d_openedLogs(NEW_L_LOGS)).

%************************************************************************************
% logToFile(append(LOGID, STR))
%   LOGID: Log identifier
%   STR:   String to append to the log
%
%  Appends the string STR to the cacheString of the log. It does not physically
% write to the file. Writting to the file requieres flushing the log cache.
%************************************************************************************
logToFile(append(LOGID, _)):-
	not(dynamicProperties(checkExistence(LOGID))),
	maplist(write, ['ERROR: Trying to append text to a non existing log with LOGID (', LOGID, ')\n']), !.
logToFile(append(LOGID, STR)):-
	not(integer(LOGID)),
	maplist(write, ['ERROR: Bad call to logToFile(append(', LOGID, ',',STR,')). First parameter should be an integer.\n']), !.
logToFile(append(LOGID, STR)):-
	dynamicProperties(get(LOGID, cacheString(CS))),
	with_output_to(string(ADDS), system:write_term(STR, [])),
	string_concat(CS, ADDS, NEW_CS),
	dynamicProperties(set(LOGID, cacheString(NEW_CS))), !.

%************************************************************************************
% logToFile(flush(LOGID))
%   LOGID: Log identifier
%
%  Flush all the text saved in the cache string to the actual file.
%************************************************************************************
logToFile(flush(LOGID)):-
	not(dynamicProperties(checkExistence(LOGID))),
	maplist(write, ['ERROR: Trying to flush text of a non existing log with LOGID (', LOGID, ')\n']), !.
logToFile(flush(LOGID)):-
	not(integer(LOGID)),
	maplist(write, ['ERROR: Bad call to logToFile(flush(', LOGID, ')). Parameter should be an integer.\n']), !.
logToFile(flush(LOGID)):-
	dynamicProperties(get(LOGID, cacheString(CS))),
	dynamicProperties(get(LOGID, fileID(FILE_ID))),
	write(FILE_ID, CS),
	flush_output(FILE_ID),
	string_to_atom(NEW_CS, ''),
	dynamicProperties(set(LOGID, cacheString(NEW_CS))).

%************************************************************************************
% logToFile(close(LOGID))
%   LOGID: Log identifier
%
%  Flush all the text saved in the cache string to the actual file, and closes
% the log file, destroying LOGID data and making LOGID invalid.
%************************************************************************************
logToFile(close(LOGID)):-
	not(dynamicProperties(checkExistence(LOGID))),
	maplist(write, ['ERROR: Trying to close a non existing log with LOGID (', LOGID, ')\n']), !.
logToFile(close(LOGID)):-
	not(integer(LOGID)),
	maplist(write, ['ERROR: Bad call to logToFile(close(', LOGID, ')). Parameter should be an integer.\n']), !.
logToFile(close(LOGID)):-
	logToFile(flush(LOGID)),
	dynamicProperties(get(LOGID, fileID(FILE_ID))),
	close(FILE_ID),
	dynamicProperties(destroy(LOGID)),
	retract(d_openedLogs(L_LOGS)),
	delete(L_LOGS, LOGID, NEW_L_LOGS),
	assert(d_openedLogs(NEW_L_LOGS)).

%************************************************************************************
% logToFile(closeAll)
%
%  Flush all the text saved in the cache string to the actual file of every opened
% log, and closes all of them, finally leaving the system resetted
%************************************************************************************
logToFile(closeAll):-
	d_openedLogs(L_LOGS),
	forall(member(L, L_LOGS), logToFile(close(L))),
	logToFile(reset).




