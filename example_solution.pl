:- use_module('pl-man-game/main').

do(move( left)) :- see(normal,  left, '.').
do(move(right)) :- see(normal, right, '.').
do(move( down)) :- see(normal,  down, '.').
do(move(   up)) :- see(normal,    up, '.').