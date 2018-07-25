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

%%
%% Archivo con la configuración del entorno (Environment Config)
%%

%% Configuración de los flags del sistema
%%
:- set_prolog_flag(debug, false).
:- set_prolog_flag(optimise, true).    % activar optimizaciones de compilación de prolog
:- set_prolog_flag(tail_recursion_optimisation, true). % optimización de la recursión de cola
:- set_prolog_flag(history, 0).        % Sin historial de comandos de consola

%% Select appropiate language
%%   Warning: It is necessary to have the appropiate your_language.pl file
%%   in /lang to load it. If not, english will be selected as default.
%%   If there is no your_language.pl file, you can make a copy of english.pl,
%%   rename it to your_language.pl then translate the messages.
%%
language(selected, 'spanish').
language(default, 'english').

%
%% Posiciones y tamaños de elementos en pantalla
%%	
%% properties(Obj, Ancho, Alto, PosX, PosY, ColorMarco)
%%	
%
properties(map, 35, 18, 2, 2, magenta).
properties(msgWindow, 35, 18, 40, 2, white). 

%%
%% Entity types
%% 
is_entityType(pacman).
is_entityType(object).
is_entityType(mortal).
is_entityType(solid).
