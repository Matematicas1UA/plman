%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2014 Francisco Gallego <ronaldo@cheesetea.com>
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
% passwordDigit
%
% Implements the behaviour of a password Digit that
% acts as a piece to form a password 
%
% Initialization
%--------------------
% passwordDigit(init, OID, [ L_PARAMS ]),
%    OID:  Identifier of the password Digit object
%    L_PARAMS: Params to set the behaviour of the password Digit (++ = mandatory)
%       * ++ digits(DIGLIST): list of available digits to selects. List of 1-character atoms.
%       * ++ switchPos(X, Y): Location were an entity should be placed to make the digit change.
%
% Example
%--------------------
%   createGameEntity(EID_1, 'P', object, 1, 2, active, passwordDigit, 
%                   [name(digit1), solid(true), static(true), appearance(attribs(normal, black, white)), 
%                   description('Modifiable digit')]),
%   passwordDigit(init, EID_1, [ digits([a,b,c,d,1,2,3,4]), switchPos(1,3) ]),
%
% Creates a modifiable password digit, that can have as content a character
% between a-d or 1-4. Placing an entity in position (1,3) makes the digint change.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% passwordChecker
%
% Implements the behaviour of an entity that checks if
% a given password is correctly set. When the password
% is correctly set, it executes a given action.
%
% Initialization
%--------------------
% passwordChecker(init, EID, [ L_PARAMS ]),
%    EID:  Identifier of the password Checker entity
%    L_PARAMS: Params to set the behaviour of the password Checker (++ = mandatory)
%       * ++ passwordDigits(DIGLIST): ordered list of OIDs of passwordDigits that are used to set the password.
%       * ++ password(P): SHA1 of the password used to verify when the correct password is set (atom). Use variant_sha1/2 to encode.
%       * ++ action(A): Callable action to be called whenever the correct password gets set.
%
% Example
%--------------------
%  createGameEntity(EID_0, '#', object, 0, 0, active, passwordChecker, []),
%  passwordChecker(init, EID_0, [ passwordDigits([EID_1, EID_2]), 
%                                 password('e7a2ff776a43a54c92aacce86073eb7eb4c09423'),
%                                 action('pl-man':destroyGameEntity(EID_3)) ] ).
%
% Creates a password Checker that waits for a 2-digit password to be correctly set.
% When the password is set, entity EID_3 is destroyed.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(password, [passwordDigit/1, passwordDigit/3, passwordChecker/1, passwordChecker/3]).


:- dynamic d_pass/2.

%%%
%%% PasswordDigit Initialization
%%%
passwordDigit(init, EID, PARAMS):-
    not(is_list(PARAMS)),
    'pl-man':showLangMessage(password, bad_parameter_list, [ 'EID: ', EID ]), !.
passwordDigit(init, EID, _):-
    d_pass(EID, _),
    'pl-man':showLangMessage(password, bad_parameters_already_initialized, [ 'EID: ', EID ]), !.
passwordDigit(init, EID, PARAMS):-
    'pl-man':checkMandatoryParameters([digits(is_list), switchPos(integer, integer)], PARAMS, passwordDigit),
    member(digits(DIGLIST), PARAMS),
    'pl-man':checkListElementsType(DIGLIST, [password:passwordDigit(oneCharacterCheck)]),
    member(switchPos(SPX, SPY), PARAMS),
    'pl-man':dynamicProperties(create(PID)),
    assert(d_pass(EID, PID)),
    'pl-man':dynamicProperties(add(PID, digits(DIGLIST))),
    'pl-man':dynamicProperties(add(PID, switchPos(p(SPX, SPY)))),
    'pl-man':dynamicProperties(add(PID, currentDigit(-1))),
    length(DIGLIST, LD),
    'pl-man':dynamicProperties(add(PID, totalDigits(LD))).
passwordDigit(init, EID, _):-
    'pl-man':showLangMessage(password, unknown_error, [ 'PasswordDigit EID: ', EID ]), !.

% Check if we are to switch the digit
passwordDigit(EID) :-
    not(d_pass(EID, _)),
    'pl-man':showLangMessage(password, not_initialized, [ 'PasswordDigit EID: ', EID ]), !.
passwordDigit(EID) :-
    d_pass(EID, PID),
    'pl-man':dynamicProperties(get(PID, switchPos(p(X,Y)))),
    'pl-man':entityLocation(_, X, Y, _),
    passwordDigit(EID, pushNextDigit).
passwordDigit(_).

% Push next digit
passwordDigit(EID, pushNextDigit) :-
    d_pass(EID, PID),
    'pl-man':dynamicProperties(get(PID, currentDigit(CD))),
    'pl-man':dynamicProperties(get(PID, totalDigits(NumD))),
    'pl-man':dynamicProperties(get(PID, digits(DIGLIST))),
    NCD is (CD + 1) mod NumD,
    nth0(NCD, DIGLIST, Ap),
    'pl-man':changeEntityAppearance(EID, Ap, _),
    'pl-man':dynamicProperties(set(PID, currentDigit(NCD))).

% Get current digit value
passwordDigit(EID, getCurrentDigitValue(CDV)):-
    d_pass(EID, PID),
    'pl-man':dynamicProperties(get(PID, currentDigit(CD))),
    'pl-man':dynamicProperties(get(PID, digits(DIGLIST))),
    nth0(CD, DIGLIST, CDV).

% Check that a given EID is a passwordDigit entity
passwordDigit(EID, check) :-
    d_pass(EID, _).

% Check that a given digit is an atom or a number of 1 character lenght
passwordDigit(oneCharacterCheck, DIGIT):- 
    (atom(DIGIT) ; number(DIGIT)), !, 
    atom_length(DIGIT, 1).

%
%% PASSWORDCHECKER
%
:- dynamic d_passChecker/2.
passwordChecker(init, EID, PARAMS):-
    not(is_list(PARAMS)),
    'pl-man':showLangMessage(password, not_initialized, [ 'PasswordChecker EID: ', EID ]), !.
passwordChecker(init, EID, _):-
    d_passChecker(EID, _),
    'pl-man':showLangMessage(password, checker_already_initialized, [ 'EID: ', EID ]), !.
passwordChecker(init, EID, PARAMS):-
    %'pl-man':checkMandatoryParameters([passwordDigits(is_list), password(atom), action(callable)], PARAMS, passwordChecker),
    % FIX FOR BAD VARIANT_SHA1/2
    'pl-man':checkMandatoryParameters([passwordDigits(is_list), password(integer), action(callable)], PARAMS, passwordChecker),
    member(passwordDigits(DIGLIST), PARAMS),
    passwordChecker(checkDigits, EID, DIGLIST),
    'pl-man':dynamicProperties(create(PID)),
    assert(d_passChecker(EID, PID)),
    member(password(PASS), PARAMS),
    member(action(ACT), PARAMS),
    'pl-man':dynamicProperties(add(PID, passwordDigits(DIGLIST))),
    'pl-man':dynamicProperties(add(PID, password(PASS))),
    'pl-man':dynamicProperties(add(PID, action(ACT))).
passwordChecker(init, EID, _) :-
    'pl-man':showLangMessage(password, unknown_error, [ 'PasswordChecker EID: ', EID ]), !.

% Check that all the digits passed to passwordChecker are PasswordDigits
passwordChecker(checkDigits, _, DIGLIST) :-
    forall(member(D, DIGLIST), passwordDigit(D, check)), !.
passwordChecker(checkDigits, EID, _) :-
    'pl-man':showLangMessage(password, checker_bad_digits, [ 'EID Checker: ', EID ]),
    fail.

passwordChecker(concatDigits, DIGLIST, CONCATD) :- 
    passwordChecker(concatDigits, DIGLIST, CONCATD, '').
passwordChecker(concatDigits, [], CONCATD, CONCATD) :- !.
passwordChecker(concatDigits, [D | LIST], CONCATD, CC) :-
    passwordDigit(D, getCurrentDigitValue(CDV)),
    atom_concat(CC, CDV, NEWCC),
    passwordChecker(concatDigits, LIST, CONCATD, NEWCC).

% Checking if the password is set
passwordChecker(checkPassword, EID, CONCATD, PASS) :-
    d_passChecker(EID, PID),
    term_hash(CONCATD, 1, 2147483647, CHASH), 
    %variant_sha1(CHASH, PASS),     %%% FIX FOR BAD VARIANT_SHA1/2
    'pl-man':dynamicProperties(get(PID, action(ACT))),
    'pl-man':showLangMessage(password, correct_password, []), 
    ignore(call(ACT)),
    'pl-man':dynamicProperties(destroy(PID)),
    'pl-man':destroyGameEntity(EID), !.
passwordChecker(checkPassword, _, _, _).
passwordChecker(EID) :-
    not(d_passChecker(EID, _)),
    'pl-man':showLangMessage(password, not_instantiated, [ 'PasswordCheker EID: ', EID ]), !. 
passwordChecker(EID) :-
    d_passChecker(EID, PID),
    'pl-man':dynamicProperties(get(PID, passwordDigits(DIGLIST))),
    'pl-man':dynamicProperties(get(PID, password(PASS))),
    passwordChecker(concatDigits, DIGLIST, CONCATD),
    passwordChecker(checkPassword, EID, CONCATD, PASS).

