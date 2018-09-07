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
lang_message(normal, '').
lang_message(sound, 'S').
lang_message(warning, 'WARNING').
lang_message(error, 'ERROR').
lang_message(sleep_time_error, 'Unexpected value passed as sleeptime. It must be a real value or the atom keypress.').
lang_message(error_loading_behaviours, 'Unexpected error loading behaviours (perhaps some behaviour is misspelled)').
lang_message(error_loading_behaviour_language_file, 'Unexpected error ocurred while loading language files for behaviour ').
lang_message(error_not_do_predicate_defined, 'Predicate do/1 undefined. Solutions must deduce actions as do/1 in order to be valid.').
lang_message(map_file_error, ' Either the map-file does not exists or it is not a map-file ').
lang_message(botrule_fail, 'Your botrule has ended in a failure state!').
lang_message(invalid_action, 'Your bot is trying to do an action not specified!').
lang_message(movement_collision, 'Your bot has attempted to move to a location occupied by a solid object!').
lang_message(you_win, 'Congratulations! Your bot has finished this level!').
lang_message(you_lose, 'Oh no! Your bot was unable to finish this level!').
lang_message(press_X, 'Press \'X\' key to continue.').
lang_message(getting_object, 'Trying to get an object from your ').
lang_message(no_object_found, 'There is no object there!').
lang_message(you_already_have_object, 'You are already carrying an object!').
lang_message(object_got, 'You have got a object').
lang_message(object_is_static, 'You can not carry with that object!').
lang_message(dropping_object, 'Trying to drop object on your ').
lang_message(using_object, 'Trying to use object on your ').
lang_message(no_object_to_use, 'You have no objects to use!').
lang_message(impossible_drop_object, 'Impossible to drop object there!').
lang_message(not_carrying_object, 'You are carrying no object at the momment!').
lang_message(object_dropped, 'You have dropped the object').
lang_message(right, 'right').
lang_message(left, 'left').
lang_message(up, 'up').
lang_message(down, 'down').
