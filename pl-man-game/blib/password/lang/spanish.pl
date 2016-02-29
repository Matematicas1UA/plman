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
lang_message(password, bad_parameter_list, 'ERROR inicializando passwordDigit. Se requiere una lista de parametros.').
lang_message(password, bad_parameters_already_initialized, 'ERROR: intento de inicializar un passwordDigit previamente inicializado.').
lang_message(password, not_initialized, 'ERROR: Objeto incorrectamente inicializado.').
lang_message(password, checker_already_initialized, 'ERROR: se intenta inicializar un passwordChecker previamente inicializado.').
lang_message(password, checker_bad_digits, 'ERROR: No todos los passwordDigits pasados al passwordChecker son realmente passwordDigits.').
lang_message(password, unknown_error, 'ERROR: Ha sucedido un error sin especificar al inicializar.'). 
lang_message(password, correct_password, 'Password correcto!').


