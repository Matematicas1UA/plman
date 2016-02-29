%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prolog Code for Map-file generated from mapafase1-1.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Dificultad: 1
%Puntuaci�n: 120

map([['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '#', '#', '#', '#', '.', '.', '.', '.', '.', '#', '#', '#', '#', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '#', '.', '.', '.', '.', ' ', '.', '#', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', ' ', '.', '#', '#', '.', '.', '.', '#', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '.', '.', '.', '#', '#', '.', '.', '.', '#', '.', '.', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '.', '.', '#', '#', '#', '#', '.', '.', '#', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '#', '#', '.', '.', '.', '.', '#', '#', '.', '.', '#', '#', '.', '.', '#'],
['#', '.', '.', '.', '.', '.', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '#', '.', '.', '#'],
['#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#']]).
map_size(20, 11).
num_dots(118).
pacman_start(7, 5).
initMap:- 
	addSolidObject('#'), 
	createGameEntity('E', mortal, 12, 4, active, vigilaFantasma, 0),
	createGameEntity('E', mortal, 3, 9, active, vigilaFantasma, 0),
	createGameEntity('E', mortal, 15, 6, active, vigilaFantasma, 0),
	createGameEntity('E', mortal, 9, 1, active, vigilaFantasma, 0).
norule(_).

:-dynamic next_movement/2.

vigilaFantasma(EID):-
	not(next_movement(EID, _)),
	assert(next_movement(EID, right)),!.

vigilaFantasma(EID):-
	next_movement(EID, X),
	(
	 X=right,
	 see(EID,normal,X,'#')
	 ->
	  retractall(next_movement(EID, _)),
	  assert(next_movement(EID, left)),
	  asserta(d_doAction(EID,move(left)))
	 ;	
	 (
	  X=left,
	  see(EID,normal,X,'#')
	  ->
	   retractall(next_movement(EID, _)),
	   assert(next_movement(EID, right)),
	   asserta(d_doAction(EID,move(right)))
	 )
	 ;
	 (
	  asserta(d_doAction(EID,move(X)))
	 )).
	

