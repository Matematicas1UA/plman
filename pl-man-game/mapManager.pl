%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2008 Francisco Gallego <ronaldo@cheesetea.com>
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
%%  MODULE: Map Manager
%%
%%    Rules to create and manage 2D maps as lists of lists
%%
:- module(mapManager, 
	[isSolid/1, addSolidObject/1, startNewMap/1, updateDMap/1, getDMap/1, 
	getCellContent/4, collision/2, collision/3, updateCellContent/5,
        setDCellContent/3, getDCellContent/3, isInsideMap/2]).
:- use_module('modules/cheeseTools').
:- dynamic d_map/1.
:- dynamic d_solidObject/1.
:- dynamic d_mapSize/2.

%************************************************************************************
% updateDMap(NewMap)
%   NewMap: Matriz con la nueva definición del mapa
% 
% Sustituye la antigua definición dinámica del mapa de juego por una nueva
% definición que le es dada como parámetro. Esta regla se utiliza para actualizar
% todo lo que cambie en el mapa.
%************************************************************************************
updateDMap(NM):-
        clause('pl-man':map_size(X,Y), _),
        p_updateDMap(NM,X,Y), !.

updateDMap(NM):-
        length(NM, Y), 
        nth0(0, NM, Fila),
        length(Fila, X),
        p_updateDMap(NM,X,Y).

p_updateDMap(NM,X,Y):-
        retractall(d_map(_)),
        retractall(d_mapSize(_,_)),
	assert(d_map(NM)),
        assert(d_mapSize(X, Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  startNewMap(Map)
%%	Map: 2D Map of the world
%%	
%%    Prepares the module to start managing a new map.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startNewMap(Map):-
	retractall(d_solidObject(_)),
	updateDMap(Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  getDMap(Map)
%%	Map: 2D Map of the world
%%	
%%    Unifies Map with the dynamic map of the world
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getDMap(Map):-
	d_map(Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  addSolidObject([Ap | More])
%%    Ap: Object Appearance
%%    More: Other object appearances
%%  addSolidObject(Ap)
%%	   Ap: Object Appearance
%%	
%%   Adds a new solid object (typically walls) by means
%%   of its Character appearance. It can add many objects
%%   by providing a list of objects appearances.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addSolidObject([]) :- !.
addSolidObject([Ap | More]):-
      addSolidObject(Ap)
   ,  addSolidObject(More).
addSolidObject(Ap):-
      not(is_list(Ap))
	,  d_solidObject(Ap)
   ,  !
   .
addSolidObject(Ap):-
      not(is_list(Ap))
	,  assert(d_solidObject(Ap)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% isSolid(Ap)
%%	Ap: Object Appearance
%%	
%%   Checks if this Appearance correlates to a solid object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isSolid(Ap):-
	d_solidObject(Ap), !.

%************************************************************************************
% getCellContent (X, Y, M, Cont)
%    X, Y: Coordenadas de la casilla del mapa que se quiere obtener (empezando en 0,0)
%    M:    Mapa (lista de listas de car�cteres)
%    Cont: Contenido de la casilla (car�cter)
%    
% Regla para obtener contenido de una casilla del mapa
%************************************************************************************
getCellContent(X,Y,M,Cont):- 
	nth0(Y,M,Fila),     % primero obtengo la fila Y de la matriz (empezando en 0)
	nth0(X,Fila,Cont).  % Y luego obtengo el elemento X de la Fila (tb empezando en 0).

%***********************************************************************************
% collision(X,Y,M)
%    X,Y: Coordenadas de la casilla concreta
%    M:   Mapa del juego
%
% Comprobar si una casilla del mapa est� ocupada est�ticamente, por lo
% que habr�a colisi�n si otro elemento intentara ocuparla tambi�n.
%***********************************************************************************
collision(X,Y,M):-
	getCellContent(X,Y,M,Cas),	% Obtengo el elemento est�tico del mapa en la casilla X, Y
	isSolid(Cas).			% Si es s�lido, hay colisi�n, si no, no.
collision(X, Y):-
        getDMap(M),
        collision(X, Y, M).

%************************************************************************************
% modificarCasillaMapa(X, Y, NCont, M, NMapa)
%    X, Y:  Coordenadas de la casilla a modificar
%    NCont: Nuevo contenido de la casilla
%    M:	    Mapa original
%    NMapa: Mapa Modificado
% Modificar el contenido de una casilla del mapa. Se apoya en modifnth0
% para modificar las listas de filas y las de columnas.
%***********************************************************************************
updateCellContent(X, Y, NCont, M, NMapa):-
	nth0(Y,M,Fila),                       % obtengo la fila Y-esima del mapa
	replaceNth0(X, NCont, Fila, NewFila),   % modifico el elemento X-esimo de la fila Y-esima
	replaceNth0(Y, NewFila, M, NMapa).      % modifico la fila Y-esima del mapa

%************************************************************************************
% setDCellContent(X, Y, NCont)
%    X, Y:  Coordenadas de la casilla a modificar
%    NCont: Nuevo contenido de la casilla
%
% Sets a new value for a specific cell in the present dynamic map, and updates it
%***********************************************************************************
setDCellContent(X, Y, NCont):-
	getDMap(Map),
	updateCellContent(X, Y, NCont, Map, NMap),
	updateDMap(NMap).
%************************************************************************************
% getDCellContent(X, Y, Cont)
%    X, Y: Cell Coordinates
%    Cont: Cell contents
%
% Gets the value of a specific cell in the present dynamic map.
%***********************************************************************************
getDCellContent(X, Y, Cont):-
	getDMap(Map),
	getCellContent(X,Y,Map,Cont).

%************************************************************************************
% isInsideMap(X, Y)
%    X, Y: Cell Coordinates
%
% Exit if X, Y are coordinates inside map boundaries.
%***********************************************************************************
isInsideMap(X, Y):-
        X >= 0, Y >= 0,
        d_mapSize(MAXX, MAXY),
        X < MAXX, Y < MAXY.
