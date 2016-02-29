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
lang_message(mineExplosion, mine_bad_parameters, 'ERROR: Bad parameters intializing mine/5 for object with OID: ').
lang_message(mineExplosion, mine_incorrect_instantiation, 'ERROR: mine/5 is not correctly instantiated for object with OID: ').
lang_message(mineExplosion, explosion_bad_parameters, 'ERROR: Bad parameters intializing explosion/5 for object with OID: ').
lang_message(mineExplosion, explosion_incorrect_instantiation, 'ERROR: explosion/5 is not correctly instantiated for object with OID: ').
lang_message(mineExplosion, explosion_error_removing, 'ERROR: Trying to remove non-existing explosion from global list. OID: ').
lang_message(mineExplosion, mine_activated, 'MINE: the mine has been activated. Countdown starts.').
lang_message(mineExplosion, cicles_left_for_explosion, 'MINE: Cicles left for explosion: ').
lang_message(mineExplosion, explosion_sound, 'MINE: KABOOOOOM!!!!').
