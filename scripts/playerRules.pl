%%
%%    PLAYER RULES
%%	
%%	Rules to control pacman made by players
%%	
:- use_module('pl-man-game/main').

%% Teclas para moverse
%%
keyAction('1', say(hola)).
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
keyControlledPacman:-
        hearAndPrint,
	repeat, 
	  get_single_char(Code),   % esperamos a que se pulse una tecla
	  char_code(Char, Code),   % obtenemos el caracter de la tecla pulsada
          upcase_atom(Char, CUP),  % lo ponemos en mayúscula
	keyAction(CUP, Action),    % obtenemos la acción asociada al caracter (si la hay)
	doAction(Action).          % Añadimos la nueva acción a realizar a las pendientes

seenormalKeyControlledPacman:-
	write('I saw...\n'),
	forall(member(DIR, [up-left, up, up-right, '\n', left, ' ', right, '\n', down-left, down, down-right, '\n']),
		(see(normal, DIR, X), write(X), ! ; write(DIR))),
	keyControlledPacman.

seelistKeyControlledPacman:-
	forall(see(list, DIR, X), (maplist(write, [DIR, ': ']), maplist(write, X), nl)),
	keyControlledPacman.

%%
%% Pacman que se mueve aleatorio
%% 
randomPacman:-
	X is random(4),
	((X=0, A=up, write('arriba '));
  	 (X=1, A=down, write('abajo '));
  	 (X=2, A=left);
  	 (X=3, A=right)),
	doAction(move(A)).

%%	
%% Pacman que ve los cocos y los sigue
%%  y se mueve aleatorio cuando no ve cocos
%%  pero evitando fantasmas y paredes
%%	
:-dynamic myDir/1.
selectivePacman:-
	(see(normal, up, UP), see(normal, down, DO), see(normal, left, LE), see(normal, right, RI),
	 see(normal, up-left, UL), see(normal, up-right, UR), see(normal, down-left, DL), see(normal, down-right, DR),
	write(UL), write(UP), writeln(UR),
	write(LE), write('@'), writeln(RI), 
	write(DL), write(DO), writeln(DR)),
	(member(DIR, [up, down,left,right]),
	 see(normal, DIR, '-') 
	 -> doAction(get(DIR))
	 ;
	 ( (member(DIR, [up, down,left,right]), see(normal, DIR, '|'), havingObject) 
	  -> doAction(use(DIR))
	  ;
	  (
	   ( ( member(DIR, [up, down,left,right]),
	       (   
	        see(normal, DIR, '.');
	        see(normal, DIR, '[');
	        myDir(DIR),
	        see(normal, DIR, ' ')
	       )
	     );
	     D is random(4),
	     nth0(D, [up, down, left, right], DIR)
	   ),
	   retractall(myDir(_)),
	   assert(myDir(DIR)),
	   ignore((write('dir: '), writeln(DIR), doAction(move(DIR))))
	  )
	 )
	).
