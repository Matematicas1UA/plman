%
% cheeseEngine is a graphics engine which uses the terminal as drawing canvas. 
% It provides primitives and several groups of rules to construct high-level 
% visual entityes. CheeseEngine makes use of cheeseText and cheeseTools.
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
%% MODULO: Motor Gráfico
%%
%%	 Provee todas las rutinas para pintar lo que se ve por pantalla
%%
:- module(cheeseEngine, 
	  [msgWindowWrite/1, msgWindowWriteln/1, msgWindowNl/0, setMsgWindowSize/2,
	   getMsgWindowSize/2, clearMsgWindow/0, drawMsgWindow/2, drawFrame/5, drawMap/8, drawEntity/5]).

:- use_module('cheeseTools').
:- use_module('cheeseText').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Declaraciones dinámicas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Predicados de la ventana de mensajes
:- dynamic d_msgWindowBuffer/1. % Lista de mensajes almacenados
:- dynamic d_msgWindowSize/2.   % Tamaño de la ventana
:- dynamic d_buffTemp/1.        % Almacenamiento temporal
:- dynamic d_screenCoords/4.    % Scroll window corners coords

%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$  SECCIÓN 1.
%$$  
%$$  Implementación de la ventana de mensajes.
%$$
%$$    Esta ventana de mensajes permite sustituir write para que
%$$    todo lo que se escriba al terminar vaya a parar a la ventana
%$$    definida en la configuración. De esta forma, evitamos la
%$$    posibilidad de que la salida de texto del usuario sobre-
%$$    escriba la zona donde se desarrolla la acción.
%$$    
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Redefinición de predicados write/1, writeln/1 y nl/0.
%%
%%   Para que estos predicados sean efectivos, es necesario redefi-
%%   nir los predicados de sistema y sustituirlos por estos, de 
%%   modo que podamos interceptar las peticiones del usuario, 
%%   y pintarlas en la ventana de mensajes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Descompone la cadena en caracteres individuales y los añade
% uno a uno el Buffer de la ventana.
%
msgWindowWrite(Txt):-
	var(Txt),
	msgWindowWrite('_NOT_INSTANTIATED_'), !.
% Patch for Mac-Version (Supposed to work either in Windows or Linux)
msgWindowWrite(Txt):-
        string_to_list(Txt, TxtList),
	forall(
                member(M, TxtList),
	        (char_code(A, M), addCharToWindowBuffer(A))
	      ).
%msgWindowWrite(Txt):-
%	forall(
%	       sub_string(Txt,_,1,_,Char), 
%	       (string_to_atom(Char, A), addCharToWindowBuffer(A))
%	      ).
msgWindowWriteln(Txt):-	msgWindowWrite(Txt), msgWindowWrite('\n').
msgWindowNl:- msgWindowWrite('\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  clearMsgWindow
%%
%%   Limpia la ventana de mensajes 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clearMsgWindow:-
	retractall(d_msgWindowBuffer(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  addCharToWindowBuffer(Char, MsgList, Last, ListLen, W, H)
%%   Char:    Caracter a añadir al buffer
%%   MsgList: Lista de mensajes que hay en el buffer
%%   Last:    Último mensaje del buffer
%%   ListLen: Longitud de la lista de mensajes del buffer
%%   Width:   Ancho de la ventana de mensajes
%%   Height:  Alto de la ventana de mensajes
%%   
%%  addCharToWindowBuffer(Char)
%%   Char:    Caracter a Añadir
%%	
%%  Conjunto de reglas para cubrir los distintos casos que se 
%%  pueden producir al añadir un nuevo caracter al buffer de 
%%  mensajes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Caso 1: Añadir un caracter distinto de \n cuando queda espacio horizontal en la última
% línea que se ha escrito hasta ahora en el buffer.
%   En este caso, añadimos el nuevo caracter en la última cadena que hay en la lista
%   que simula el buffer de texto.
%   
addCharToWindowBuffer(Char, MsgList, Last, ListLen, Width, _):-
        string_length(Last, Length),
	Char \= '\n',                            
	Length < Width,                          
	string_concat(Last, Char, NewLast),	 
	ListLen_1 is ListLen-1,
%	subList(MsgList, 0, ListLen_2, ListTemp), 
%	append(ListTemp, [NewLast], NewMsgList),
	replaceNth0(ListLen_1, NewLast, MsgList, NewMsgList),
	updateMsgWindowBuffer(NewMsgList).
%
% Caso 2: Añadir un caracter distinto de \n cuando la última cadena escrita hasta el momento
% ocupa ya todo el espacio horizontal del buffer.
%   En este caso, tenemos que hacer un salto de línea artificial y crear una nueva cadena
%   que sólo contenga el carácter que queremos añadir
%   
addCharToWindowBuffer(Char, MsgList, Last, ListLen, Width, Height):-
        string_length(Last, Length),
	Char \= '\n',                            
	Length >= Width,
	addCharToWindowBuffer('\n', MsgList, Last, ListLen, Width, Height),
	addCharToWindowBuffer(Char).
%
% Caso 3: Añadir un salto de línea cuando todas las líneas del buffer están llenas, lo
% que provoca que el buffer haga scroll vertical.
%    Eliminamos la primera cadena del buffer y añadimos una cadena vacía al final
%   
addCharToWindowBuffer('\n', MsgList, _, ListLen, _, Height):-
	ListLen >= Height,
	subList(MsgList, 1, ListLen, ListTemp),
	string_to_list(EmptyStr, []),
	append(ListTemp, [EmptyStr], NewMsgList),
	updateMsgWindowBuffer(NewMsgList).
%
% Caso 4: Añadir un salto de línea cuando aún no se han llenado todas las líneas del buffer
% y, por tanto, aún no hay que hacer scroll.
%   Añadimos una cadena vacía como último elemento de la lista que simula el buffer
%   
addCharToWindowBuffer('\n', MsgList, _, ListLen, _, Height):-
	ListLen < Height,
	string_to_list(EmptyStr, []),
	append(MsgList, [EmptyStr], NewMsgList),
	updateMsgWindowBuffer(NewMsgList).
%
% Tratamiento especial para el caracter de tabulación
% 
addCharToWindowBuffer('\t'):-
	repeatNTimes(addCharToWindowBuffer(' '), 3), !.
%
% Rutina general para añadir caracteres al Buffer
%
addCharToWindowBuffer(Char):-
	d_msgWindowBuffer(MsgList),
	last(MsgList, Last),
	length(MsgList, Length),
	d_msgWindowSize(Width, Height),
	addCharToWindowBuffer(Char, MsgList, Last, Length, Width, Height), !.
%
% Inicializar el buffer cuando aún no está creado
%
addCharToWindowBuffer(Char):-
	not(d_msgWindowBuffer(_)),
	(Char \= '\n'
	 -> 
	 assert(d_msgWindowBuffer([Char]))
	 ;  
 	 string_to_list(EmptyStr, []),
 	 assert(d_msgWindowBuffer([Char, EmptyStr]))
	), !.
%
% Pequeña llamada para actualizar el Buffer cuando hayan cambios
%
updateMsgWindowBuffer(NewMsgList):-
	retractall(d_msgWindowBuffer(_)),
	assert(d_msgWindowBuffer(NewMsgList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  drawMsgLine(X, Line)
%%    X: Alineamiento horizontal (desplazamiento en X)
%%    Line: Linea de caracteres a dibujar
%%
%%   Dibuja una linea de la ventana de mensajes.
%%
%%  drawMsgWindow(X, Y)
%%    X, Y: Coordenadas de la esquina de la ventana de mensajes
%%	
%%   Dibuja la ventana de mensajes en pantalla. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawMsgLine(X, Line):-
	system:write('\033['), system:write(X), system:write('C'),
	system:writeln(Line).
drawMsgWindow(X, Y):-
	d_msgWindowBuffer(MsgList),
	Y1 is Y	- 1,
	setCursor(0,Y1),
	system:nl,
	maplist(drawMsgLine(X), MsgList).
drawMsgWindow(_, _):-
	not(d_msgWindowBuffer(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  setMsgWindowSize(W, H)
%%   W, H: Ancho y Alto de la ventana
%%	
%%   Cambia el tamaño de la ventana de mensajes. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
correctEndCR(Text, TextOk):-
	string_length(Text, Len),
	Len_1 is Len-1,
	sub_string(Text, Len_1, 1, _, CR),
	string_to_atom(CR, CRA),
	(CRA \= '\n'
	 ->  TextOk = Text
	  ;  sub_string(Text, 0, Len_1, _, TextOk)).
updateDBuffTemp(SizeX, X):-
	d_buffTemp(BT),
	string_length(X, Length),	
	string_concat(BT, X, StrC1),
	(Length < SizeX 
	-> string_concat(StrC1, '\n', StrC)
	;  StrC = StrC1),
	abolish(d_buffTemp/1),
	assert(d_buffTemp(StrC)).
setMsgWindowSize(W, H):-
	not(d_msgWindowSize(_,_)),
	assert(d_msgWindowSize(W, H)), !.
setMsgWindowSize(W, H):-
	not(d_msgWindowBuffer(_)),
	retractall(d_msgWindowSize(_,_)),
	assert(d_msgWindowSize(W,H)), !.
setMsgWindowSize(W, H):-
	d_msgWindowBuffer(MsgList),
	d_msgWindowSize(SizeX, _),
	abolish(d_buffTemp/1),
	string_to_list(Str, []),
	assert(d_buffTemp(Str)),
	maplist(updateDBuffTemp(SizeX), MsgList),
	retractall(d_msgWindowSize(_,_)),
	assert(d_msgWindowSize(W,H)),
	clearMsgWindow,
	d_buffTemp(Text),
	correctEndCR(Text, TextOk),
	msgWindowWrite(TextOk).

% Por defecto, tamaño 60, 10
:- setMsgWindowSize(60, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  getMsgWindowSize(W, H)
%%   W, H: Ancho y Alto de la ventana
%%	
%%   Obtener tamaño de la ventana de mensajes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getMsgWindowSize(W, H):- d_msgWindowSize(W, H).

%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$  SECCIÓN 2.
%$$  
%$$ Reglas para dibujar el mapa en pantalla posicionándolo donde 
%$$ se quiera, dándole el tamaño que se quiera y mostrándolo con 
%$$ scroll y con el personaje centrado. 
%$$
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drawFrame(X, Y, Width, Heigth, Color)
%%   X, Y: Coordenadas de la esquina superior izquierda del marco
%%   Width, Height: Ancho y Alto del marco
%%   Color: Color en que se pintará el Marco
%% 
%%   Dibuja un marco al rededor de una zona de pantalla para 
%%   resaltarla.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawFrame(X, Y, Width, Height, Color):-
		setColors(normal, Color, Color),
		Y1 is Y-1, YN is Y+Height,
		X1 is X-1, XN is X+Width,
		setCursor(X, Y1),
		repeatNTimes(system:write('_'), Width),
		setCursor(X, YN),
		repeatNTimes(system:write('^'), Width),
		setCursor(X1, Y),
		repeatNTimes(system:write('(\033[B\033[D'), Height),
		setCursor(XN, Y),
		repeatNTimes(system:write(')\033[B\033[D'), Height),
		resetColors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% calculateScreenCoord(P, ScrSize, MapSize, SCR)
%%   P: Posición horizontal o vertical del personaje
%%   ScrSize: Tamaño horizontal o vertical de la pantalla de scroll
%%            a mostrar
%%   MapSize: Tamaño horizontal o vertical del mapa total
%%   SCR:     Coordenada horizontal o vertical de la Esquina superior
%%	      izquierda con respecto al mapa donde empieza la vista 
%%            de scroll.
%%
%%  Calcula la coordenada de pantalla (tanto horizontal como 
%%  vertical) dónde se debe pintar. Esto va en función del tamaño
%%  que tenga la vista del mapa y el tamaño del mapa.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculateScreenCoord(P, ScrSize, MapSize, SCR):-
		S is P - ScrSize // 2,
		LIM1 is MapSize - ScrSize,
		((LIM1 < 0 -> LIM = 0); LIM = LIM1),
		((S < 0 -> S1 = 0); S1 = S),
		((S1 > LIM -> SCR = LIM); SCR = S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drawRow(X, Y, Fila)
%%   X, Y: Coordenadas X e Y de pantalla donde debe comenzar 
%%	   a pintarse la fila
%%   Fila: Lista de caracteres que contiene una fila del mapa
%%	
%%   Dibuja una fila del mapa
%%	
%% drawMap(X, Y, M)
%%   X, Y: Coordenadas de pantalla de la esquina superior
%%	   izquierda del mapa 
%%   PX, PY: Coordenadas de posición del pacman en el mapa
%%   Width, Height: Ancho y alto del mapa
%%   Map: Mapa implementado como lista de listas de caracteres
%%   PacManAp: Pacman's appearance (character)
%%	
%%   Dibuja el mapa completo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawRow(X, Y, Fila):-
		setCursor(X, Y),
		string_to_list(Str, Fila), % Convierto la lista de caracteres en un string
		system:write(Str).         % Escribo el string por pantalla
%
drawMap(X, Y, PX, PY, Width, Height, Map, PacManAp):-
		% Coordenadas esquina superior izquierda pantalla scroll
		% Y coordenadas l�mite para las demas esquinas
		length(Map, SizeY),
		nth0(0, Map, R0),
		length(R0, SizeX),
		calculateScreenCoord(PX, Width, SizeX, SCRX),
		calculateScreenCoord(PY, Height, SizeY, SCRY),
		MaxX is SCRX + Width - 1,		
		MaxY is SCRY + Height - 1,
		retractall(d_screenCoords(_,_,_,_)),
		assert(d_screenCoords(SCRX, SCRY, MaxX, MaxY)),

		% Obtengo sublista de Filas para pintar del mapa
		subList(Map, SCRY, MaxY, MapToDraw),

		% Pinto todas las filas de la sublista del mapa, pero 
		% sólo los elementos que me interesan. Y el marco!
		forall(
		       nth0(N, MapToDraw, Row), 
		       (
		        PosY is Y + N,
			subList(Row, SCRX, MaxX, RowToDraw),
			drawRow(X, PosY, RowToDraw)
		       )
		      ),

		% Ahora pinto al personaje
		ScrPX is X + (PX - SCRX),
		ScrPY is Y + (PY - SCRY),
		setCursor(ScrPX, ScrPY),
		system:write(PacManAp),

		% Al final, me llevo el cursor a otro sitio
		setCursor(20,20).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drawEntity(MapX, MapY, EntX, EntY)
%%   MapX, MapY: Screen Coordinates of the upper left corner of
%%		 de map view.
%%   EntX, EntY: Map coordinates of the location of the entity
%%   Ap:         Appearance of the entity
%%
%%  Draws a new entity on the screen if it is visible now.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawEntity(_, entity(EX, EY), limits(MinX, MinY, MaxX, MaxY), _):-
	(EX < MinX ; EY < MinY; EX > MaxX; EY > MaxY), !.
drawEntity(map(MX,MY), entity(EX, EY), limits(MinX, MinY,_,_), Ap):-
	LocationX is MX + EX - MinX,
	LocationY is MY + EY - MinY,
	setCursor(LocationX, LocationY),
	system:write(Ap).
drawEntity(MapX, MapY, EntX, EntY, appearance(Atr, FC, BC, Ap)):-
	setColors(Atr, FC, BC),
	drawEntity(MapX, MapY, EntX, EntY, Ap),
	setColors(normal, default, default), !.
drawEntity(MapX, MapY, EntX, EntY, Ap):-
	% Coordenadas esquina superior izquierda pantalla scroll
	% Y coordenadas limite para las demas esquinas 
	% (You should have launched drawMap/8 first)
	d_screenCoords(SCRX, SCRY, MaxX, MaxY),
	drawEntity(map(MapX,MapY), entity(EntX, EntY), limits(SCRX, SCRY, MaxX, MaxY), Ap).
