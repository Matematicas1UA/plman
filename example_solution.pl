:- ['game/main'].

do(move( left)) :- see( left, '.').
do(move(right)) :- see(right, '.').
do(move( down)) :- see( down, '.').
do(move(   up)) :- see(   up, '.').
