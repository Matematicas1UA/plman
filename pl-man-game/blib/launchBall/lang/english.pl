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
lang_message(launchBall, bad_parameters, 'ERROR: Bad parameters intializing launchBall/3 for object with OID: ').
lang_message(launchBall, bad_instantiation, 'ERROR: Incorrect instantiation for launchBall object with OID: ').
lang_message(launchBall, bad_parameters_autoBallCreator, 'ERROR: 2nd initialization attempt or bad parameters intializing autoBallCreator (launchBall/5)').
lang_message(launchBall, autoBallCreator_description, 'Automatic Ball Creator').
lang_message(launchBall, autoBallCreator_name, 'autoBallCreator').
lang_message(launchBall, ball_name, 'ball').
lang_message(launchBall, ball_description, 'Ball that may be shot to goal').
lang_message(launchBall, ball_launched, 'BALL: Shot to the ').
lang_message(launchBall, ball_puncture, 'BALL: Got bursted!').
lang_message(launchBall, ball_stopped, 'BALL: Was stopped by ').
lang_message(launchBall, balls_left, 'BALL CREATOR: Balls left to be created: ').
lang_message(launchBall, left,  'left').
lang_message(launchBall, right, 'right').
lang_message(launchBall, down,  'down').
lang_message(launchBall, up,    'up').
