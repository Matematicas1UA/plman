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
lang_message(quantumJump, bad_parameters, 'ERROR: Parametros incorrectos inicializando quantumJump para objeto con OID: ').
lang_message(quantumJump, error_parameter_invalid, 'ERROR: Parametro incorrecto para quantumJump/4').
lang_message(quantumJump, qj_started, 'El salto cuantico ha sido iniciado! ').
lang_message(quantumJump, qj_step, 'Salto: ').
lang_message(quantumJump, qj_direction, 'Dir: ').
lang_message(quantumJump, jump_outside_map, 'Oh! No! No puedes saltar fuera de la dimension del mapa!').
lang_message(quantumJump, jump_over_solid, 'Oh! No! No puedes saltar sobre una entidad solida!').
lang_message(quantumJump, jump_success, 'El salto cuantico ha sido un exito!').
lang_message(quantumJump, qj_destroyed, 'El saltador cuantico desaparece entre tus manos!').
