:- use_module('pl-man-game/main').

%% 
%% Simplify checking and doing
%%
dir(h, here).
dir(d, down).
dir(u, up).
dir(l, left).
dir(r, right).
dir(ul, up-left).
dir(ur, up-right).
dir(dl, down-left).
dir(dr, down-right).
checkConditions(  []) :- !.
checkConditions(LIST) :- 
    forall( member(M, LIST), 
            ( functor(M, D, 1), arg(1, M, OBJ), dir(D, DIR), see(normal, DIR, OBJ) ) ).
cdo(CHECKLIST, ACTION) :-
    checkConditions(CHECKLIST),
    doAction(ACTION).

r(=):- cdo([ l('.') ],  move( left)).
r(=):- cdo([ u('.') ],  move(   up)).
r(=):- cdo([ d('.') ],  move( down)).
r(=):- cdo([ r('.') ],  move(right)).
r(=):- cdo([ u('P'), l(' ') ],  move( left)).
r(=):- cdo([ u('P') ],  move( down)).
r(=):- cdo([ l('#'), u(' ') ],  move( left)).
r(=):- cdo([ h('#') ],  move(right)).
r(=):- cdo([ r(' ') ],  move(right)).
r(=):- cdo([ r('[') ],   use(right)).
r(=):- cdo([ d(' ') ],  move( down)).
r(=):- cdo([ ur('[') ], move(   up)).

plman :- havingObject(appearance(OBJ)), r(OBJ).
plman :- cdo([ r('.') ],  move(right)).
plman :- cdo([ u('.') ],  move(   up)).
plman :- cdo([ dr('E') ], move(   up)).
plman :- cdo([ d('.') ],  move( down)).
plman :- cdo([ l('.') ],  move( left)).
plman :- cdo([ l(' ') ],  move( left)).
plman :- cdo([ d('E') ],  move( none)).
plman :- cdo([ u('=') ],   get(   up)).
plman :- cdo(        [],  move( down)).
