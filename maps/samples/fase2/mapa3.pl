%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DIF:	5
%%% PT:	00:20	[STS: 01:00]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_format_version(1.0).

map([['#', '#', '#', '#', '#', '#', '#', '#', '#']
    ,['#', '.', '.', '.', '.', '.', '.', '.', '#']
    ,['#', '.', '#', ' ', ' ', ' ', '#', '.', '#']
    ,['#', '.', '.', '.', '.', '.', '.', '.', '#']
    ,['#', '#', '#', '#', '#', '#', '#', '#', '#']]).

map_size(9, 5).
num_dots(16).
pacman_start(1, 1).
initMap:- 
	   addSolidObject('#')
   ,  dotGenerator(init, 4, 2, []).

%%
%% Initialize a dot creator
%%
:- dynamic d_dotGenerator/4.
dotGenerator(init, X, Y, L_PARAMS):-
      is_list(L_PARAMS)
   ,  createGameEntity(EID_DG, '+', object, X, Y, active, dotGenerator_rule, 
         [name(generadorCocos), solid(true), static(false), use_rule(norule),
         description('Genera cocos mientra no sea detenido'), 
         appearance(attribs(bold, green, black))])
   ,  X1 is X-1
   ,  X2 is X+1
   ,  createGameEntity(EID_N1, '#', object, X1, Y, inactive, norule, 
         [name(numeroDerecho), solid(true), static(true), use_rule(norule),
         description('Primer operador del generador de cocos'), 
         appearance(attribs(bold, green, black))])
   ,  createGameEntity(EID_N2, '#', object, X2, Y, inactive, norule, 
         [name(numeroIzquierdo), solid(true), static(true), use_rule(norule),
         description('Segundo operador del generador de cocos'), 
         appearance(attribs(bold, green, black))])
   ,  retractall(d_dotGenerator(_, _, _, _))
   ,  assert(d_dotGenerator(EID_DG, EID_N1, EID_N2, mortal))
   ,  randomizeOperands(EID_DG)
   ,  maplist(write, ['Generador de cocos activado.\nPara detener, coger en verde.\n']).

randomizeOperands(EID_DG):-
      d_dotGenerator(EID_DG, EID_N1, EID_N2, _)
   ,  entityLocation(EID_DG, X, Y, _)
   ,  not(p_isPlmanNearby(X, Y))
   ,  N1 is random(10)
   ,  N2 is random(10)
   ,  p_dotGenStatus(N1, N2, Status)
   ,  p_setGeneratorAppearance(EID_DG, Status)
   ,  changeEntityAppearance(EID_N1, N1, bold, white, black)
   ,  changeEntityAppearance(EID_N2, N2, bold, white, black)
   ,  retractall(d_dotGenerator(_, _, _, _))
   ,  assert(d_dotGenerator(EID_DG, EID_N1, EID_N2, Status)).

p_setGeneratorAppearance(EID_DG, mortal) :-
   changeEntityAppearance(EID_DG, '+', bold,   red, black).
p_setGeneratorAppearance(EID_DG,   open) :-
   changeEntityAppearance(EID_DG, '+', bold, green, black).


p_dotGenStatus(N1, N2,   open) :- S is N1 + N2, S > 9, !.
p_dotGenStatus( _,  _, mortal).

%% Collision effects
p_effect(     _, mortal) :-
      playerStats(set(game_status(finished)))
   ,  maplist(write,['El generador de cocos te ha destruido!\n']), !.

p_effect(EID_DG,   open) :-
      destroyGameEntity(EID_DG)
   ,  retractall(d_dotGenerator(_,_,_,_))
   ,  maplist(write,['Generador de Cocos Desactivado!\n']).

%% Control rule for collision with PLMan
dotGenerator_rule(EID_DG):-
      isObjectGot(EID_DG)
   ,  d_dotGenerator(EID_DG, _, _, Status)
   ,  p_effect(EID_DG, Status), !.

%% Control rule for generating new dots and randomizing
dotGenerator_rule(EID_DG):-
      randomizeOperands(EID_DG)
   ,  maplist(p_genDot 
              , [ 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 1, 7 ]
              , [ 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 2, 2 ]).

%% Subtract in absolute value
p_subabs(D, A, B) :- A > B, D is A - B, !.
p_subabs(D, A, B) :- D is B - A.

%% Generate dots where required
p_isPlmanNearby(X, Y) :-
      entityType(EID_P, pacman)
   ,  entityLocation(EID_P, PX, PY, _)
   ,  p_subabs(DX, PX, X), DX =< 1
   ,  p_subabs(DY, PY, Y), DY =< 1.

%% Generate a new dot
p_genDot(X, Y):-
      getDMap(Map)
   ,  getCellContent(X, Y, Map, ' ')
   ,  not(p_isPlmanNearby(X, Y))
   ,  updateCellContent(X, Y, '.', Map, NewMap)
   ,  updateDMap(NewMap)
   ,  playerStats(add(remaining_dots,  1)).
p_genDot(_, _).
