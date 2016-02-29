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

%%
%% Warning messages
%%
lang_message(password, bad_parameter_list, 'ERROR initializing passwordDigit. Param list required.').
lang_message(password, bad_parameters_already_initialized, 'ERROR: trying to initialize already initialized passwordDigit.').
lang_message(password, not_initialized, 'ERROR: Not propperly initialized.').
lang_message(password, checker_already_initialized, 'ERROR: trying to initialize already initialized passwordChecker.').
lang_message(password, checker_bad_digits, 'ERROR: Not all the passwordDigits given to passwordChecker are actually passwordDigits.'). 
lang_message(password, unknown_error, 'ERROR: some not specified error happened initializing.').
lang_message(password, correct_password, 'Correct Password!').

