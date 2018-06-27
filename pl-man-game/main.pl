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

%%
%%  Main
%%    
%%	Loader of the Prolog-pacman game
%%	
:- module(main, [havingObject/0, havingObject/1, see/3, hear/2, play/2, play/3, play/4, replay/2]).

% Modules
:- use_module('pl-man').
:- use_module('modules/cheeseEngine').
:- use_module('modules/cheeseText').

%%	
%% System-predicates redefinitions
%%	
:-redefine_system_predicate(user:write(_)).
user:write(T) :- 
	p_logWrite(T),
	msgWindowWrite(T).
:-redefine_system_predicate(user:writeln(_)).
user:writeln(T) :- 
	p_logWrite(T),
	p_logWrite('\n'),
	msgWindowWriteln(T).
:-redefine_system_predicate(user:nl).
user:nl :-
	p_logWrite('\n'), 
	msgWindowNl.

p_logWrite(_):- not('pl-man':d_logging(_)), !.
p_logWrite(MSG):-
	swritef(S, '%q', [MSG]),
	'pl-man':mainLog(append_c(w(S))).

