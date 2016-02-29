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
%% Warning messages
%%
lang_message(cauldron, bad_parameters, 'ERROR: Bad parameters intializing cauldron/1.').
lang_message(cauldron, bad_creation, 'ERROR: Bad parameters creating cauldron/1.').
lang_message(cauldron, bad_destroy, 'ERROR: Trying to destroy nonexistent cauldron.').
lang_message(cauldron, not_initialized, 'ERROR: Trying to use a non-initialized cauldron.').
lang_message(cauldron, problem_adding_ingredient, 'ERROR: Problem ocurred trying to add a new ingredient to cauldron.').
lang_message(cauldron, bad_recipe_params, 'ERROR: Bad parameters trying to initialize a recipe for cauldrons.').
lang_message(cauldron, ingredient_added, '[Potion]: New ingredient added').
lang_message(cauldron, potion_complete, '[Potion]: Complete!').
lang_message(cauldron, potion_working, '[Potion]: Working!').
