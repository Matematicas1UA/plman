%%
%%    KeycontrolledPacman
%%	
%%	Rules to control pacman made by players
%%	
:- use_module('pl-man-game/main').

%% Teclas para moverse
%%
keyAction('1', say(hello)).
keyAction('2', say('Slytherin')).
keyAction('3', use(flIpEnDO, left)).
keyAction('4', use(flIpEnDO, right)).
keyAction('5', use(flIpEnDO, up)).
keyAction('6', use(flIpEnDO, down)).
keyAction('Z', use(aLoHoM0rA, left)).
keyAction('X', use(aLoHoM0rA, right)).
keyAction('C', use(aLoHoM0rA, up)).
keyAction('V', use(aLoHoM0rA, down)).
keyAction('Q', move(up)).
keyAction('A', move(down)).
keyAction('O', move(left)).
keyAction('P', move(right)).
keyAction('W', get(up)).
keyAction('S', get(down)).
keyAction('L', get(left)).
keyAction('Ñ', get(right)).
keyAction('E', drop(up)).
keyAction('D', drop(down)).
keyAction('.', drop(left)).
keyAction('-', drop(right)).
keyAction('R', use(up)).
keyAction('F', use(down)).
keyAction('9', use(left)).
keyAction('0', use(right)).
keyAction('\033', endGame).

hearAndPrint:-
    hear(normal, msg(WHO, TYPE, WHAT)), !,
    maplist(user:write, [ '[OIDO] ', WHO, '(',TYPE,')\n >>', WHAT, '\n' ]).
hearAndPrint.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reglas de control de pacman de prueba
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Regla para controlar a Pacman con teclas
%%
do(Action) :-
   hearAndPrint,
	repeat, 
		get_single_char(Code),   % esperamos a que se pulse una tecla
	   char_code(Char, Code),   % obtenemos el caracter de la tecla pulsada
      upcase_atom(Char, CUP),  % lo ponemos en mayúscula
	keyAction(CUP, Action), !.  % obtenemos la acción asociada al caracter (si la hay)