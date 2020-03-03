%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2020 Francisco J. Gallego-Durán <fjgallego@ua.es>
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
lang_message(launchBall, bad_parameters, 'ERROR: Parametros incorrectos inicializando launchBall/3 para el objeto con OID: ').
lang_message(launchBall, bad_instantiation, 'ERROR: Se instancio incorrectamente el objeto launchBall con OID: ').
lang_message(launchBall, bad_parameters_autoBallCreator, 'ERROR: Intentando instanciar por 2a vez o parametros incorrectos instanciando autoBallCreator (launchBall/5)').
lang_message(launchBall, autoBallCreator_description, 'Creador automatico de pelotas').
lang_message(launchBall, autoBallCreator_name, 'generadorPelotas').
lang_message(launchBall, ball_description, 'Pelota que puede ser lanzada a puerta').
lang_message(launchBall, ball_name, 'pelota').
lang_message(launchBall, ball_launched, 'PELOTA: Lanzada hacia ').
lang_message(launchBall, ball_puncture, 'PELOTA: Se ha pinchado!').
lang_message(launchBall, ball_stopped, 'PELOTA: Ha sido detenida por ').
lang_message(launchBall, balls_left, 'CREADOR PELOTAS: Pelotas restantes por ser creadas: ').
lang_message(launchBall, left,  'izquierda').
lang_message(launchBall, right, 'derecha').
lang_message(launchBall, down,  'abajo').
lang_message(launchBall, up,    'arriba').
