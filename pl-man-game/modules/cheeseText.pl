%
% cheeseText is a set of rules aimed for providing a way to easily interface
% other PROLOG programs with the terminal. It lets you locate the cursor and 
% print text in colors using ANSI escape sequences. It is a basic layer for 
% constructing terminal-based graphics engines.
%
% Copyright (C) 2007-2008 Francisco Gallego (ronaldo/Cheesetea) <ronaldo@cheesetea.com>
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
%% MODULO: Manipulador del terminal
%%
%%		Provee rutinas multiplataforma para el manejo del 
%%		terminal en modo texto.
%%
:- module(cheeseText, [attribCode/2, colorCode/3, cls/0, setCursor/2, setColors/3, resetColors/0]).

%%%
%%% Códigos de attributos y colores 
%%%
attribCode(normal, 0).
attribCode(bold, 1).
attribCode(faint, 2).
attribCode(standout, 3).
attribCode(underline, 4).
attribCode(blink, 5).
attribCode(reverse, 7).
attribCode(invisible, 8).
attribCode(no_standout, 23).
attribCode(no_underline, 24).
attribCode(no_blink, 25).
attribCode(no_reverse, 27).
colorCode(foreground, black, 30).
colorCode(foreground, red, 31).
colorCode(foreground, green, 32).
colorCode(foreground, yellow, 33).
colorCode(foreground, magenta, 35).
colorCode(foreground, cyan, 36).
colorCode(foreground, white, 37).
colorCode(foreground, default, 39).
colorCode(background, black, 40).
colorCode(background, red, 41).
colorCode(background, green, 42).
colorCode(background, yellow, 43).
colorCode(background, magenta, 45).
colorCode(background, cyan, 46).
colorCode(background, white, 47).
colorCode(background, default, 49).


%************************************************************************************
% cls - Borrado de pantalla, 
% setCursor(X,Y) - posicionamiento cursor
% setColors(Atrib, TextC, BackC) - cambio de colores
% resetColors - Restaura los colores por defecto
%************************************************************************************
cls:-
	write('\033[H\033[2J').

setCursor(X, Y):-
	write('\033[H\033['), write(Y), 
	write('B\033['), write(X), write('C').

%%
%% setColors sólo funcionará si el sistema operativo es unix.
%% En windows setColors no hará nada.
%%
%%   NOTA: Este código sólo compila a partir de la versión 5.6.43
%%         de SWI-Prolog, ya que anteriormente no se soportaba
%%			  compilación condicional. Debajo está comentada una versión 
%%         más lenta pero que compila en versiones anteriores.
%%	
:- if(prolog_flag(unix, true)).
setColors(Atrib, TextC, BackC):-
	attribCode(Atrib, ACode),
	colorCode(foreground, TextC, TCode),
	colorCode(background, BackC, BCode),
	write('\033['), write(ACode), write(';'),
	write(TCode), write(';'), write(BCode), write('m'), !.
setColors(_,_,_):-
	writeln('Unknown Attrib/Color at setColor').
:- else.
setColors(_,_,_).
:- endif.
/**************************************************************
*** Utilizar estas rutinas en versiones de SWI-prolog
*** anteriores a la 5.6.43.
***
setColors(Atrib, TextC, BackC):-
	prolog_flag(unix, true),
	attribCode(Atrib, ACode),
	colorCode(foreground, TextC, TCode),
	colorCode(background, BackC, BCode),
	write('\033['), write(ACode), write(';'),
	write(TCode), write(';'), write(BCode), write('m'), !.
setColors(_,_,_):-
	prolog_flag(unix, true),
	writeln('Unknown Attrib/Color at setColor'), !.
setColors(_,_,_).
**************************************************************/
resetColors:-
	setColors(normal, default, default).


