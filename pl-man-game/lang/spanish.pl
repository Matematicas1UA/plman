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
lang_message(warning, 'ADVERTENCIA').
lang_message(sound, 'S').
lang_message(error, 'ERROR').
lang_message(sleep_time_error, 'Valor inesperado recibido en el parametro sleeptime. Debería ser un valor real o el atomo keypress.').
lang_message(error_loading_behaviours, 'Ocurrio un error cargando los comportamientos (alguno de los especificados no existe o esta mal escrito)').
lang_message(error_loading_behaviour_language_file, 'Ocurrio un error cargando los ficheros de idioma para el comportamiento ').
lang_message(error_not_do_predicate_defined, 'El predicado do/1 no ha sido definido. Los ficheros de solución deben deducir las acciones usando do/1 para ser válidos.').
lang_message(map_file_error, ' El archivo de mapa no existe o no tiene el formato correcto.' ).
lang_message(botrule_fail, 'La regla de control de tu personaje ha fracasado!').
lang_message(invalid_action, 'Tu personaje intenta realizar una acción no especificada!').
lang_message(movement_collision, 'Tu personaje ha intentado moverse hacia una posición que está ocupada por un objecto solido!').
lang_message(you_win, 'Enhorabuena! Tu personaje ha terminado este nivel!').
lang_message(you_lose, 'Oh no! Tu personaje no ha podido terminar este nivel!').
lang_message(press_X, 'Pulsa \'X\' para continuar.').
lang_message(getting_object, 'Intentando coger un objeto de ').
lang_message(no_object_found, 'No hay ningún objeto ahí!').
lang_message(you_already_have_object, 'Ya llevas un objeto contigo!').
lang_message(object_got, 'Obtienes un objeto').
lang_message(object_is_static, 'No puedes cargar con ese objeto!').
lang_message(dropping_object, 'Intentando dejar el objeto en ').
lang_message(using_object, 'Intentando usar objeto en ').
lang_message(no_object_to_use, 'No tienes ningún objeto para usar!').
lang_message(impossible_drop_object, 'No se puede dejar el objeto ahí!').
lang_message(not_carrying_object, 'En este momento no llevas ningún objeto!').
lang_message(object_dropped, 'Dejas el objeto').
lang_message(right, 'derecha').
lang_message(left, 'izquierda').
lang_message(up, 'arriba').
lang_message(down, 'abajo').
