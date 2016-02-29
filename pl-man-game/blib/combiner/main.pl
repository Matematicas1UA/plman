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
% combiner
%
% Combines several behaviours to be performed by the
% same entity.
%
% Initialization
%--------------------
% It does not requiere initialization
%
% Example
%--------------------
%  createGameEntity(EID, 'E', mortal, 11, 1, active, combiner([enemyFollower, automaticArcher]), []),
%  enemyFollower(init, EID, ....),
%  automaticArcher(init, EID, ....).
%
% Creates an enemy that will follow pacman and through
% arrows at him at the same time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(combiner, [ combiner/2, combiner/6 ]).

%%% Combine actions
combiner(L_BEHAVIOURS, EID):-
	forall(member(B, L_BEHAVIOURS), ignore(call('pl-man':B, EID))).
%%% Combine use rules
combiner(L_BEHAVIOURS, OID, EID, X, Y, DIR):-
	forall(member(B, L_BEHAVIOURS), ignore(call('pl-man':B, OID, EID, X, Y, DIR))).

