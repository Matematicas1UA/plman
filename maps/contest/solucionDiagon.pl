:- use_module('pl-man-game/main').

% Aliases
seeDo(DS, C, ACT) :- see(normal, DS, C), doAction(ACT).
seeMove(DS, C, DM) :- see(normal, DS, C), doAction(move(DM)).
seeMove(DIR, C)    :- seeMove(DIR, C, DIR).

r(v) :- seeMove(down, '.').
r(v) :- seeMove(up, '.').
r(v) :- seeMove(left, '.').
r(v) :- seeMove(right, '.').
r(v) :- seeMove(right, 'b').
r(v) :- doAction(move(down)).

r(b) :- seeMove(down, '.').
r(b) :- seeMove(up, '.').
r(b) :- seeMove(left, '.').
r(b) :- seeMove(right, '.').
r(b) :- seeMove(left, 'g').
r(b) :- seeMove(  up, ' ').
r(b) :- seeDo(  up, 'v', drop(right)).
r(b) :- doAction(move(left)).

r(g) :- seeDo(up, '-', use(up)).
r(g) :- seeMove(down, '.').
r(g) :- seeMove(up, '.').
r(g) :- seeMove(left, '.').
r(g) :- seeMove(right, '.').
r(g) :- seeDo(left, 'l', move(down)).
r(g) :- seeMove(right, ' ').
r(g) :- seeMove(right, 'b').
r(g) :- seeDo(right-down, 'b', move(down)).
r(g) :- seeDo(down, 'b', drop(left)).
r(g) :- doAction(move(down)).

r(l) :- seeMove(up, '.').
r(l) :- seeMove(down, '.').
r(l) :- seeMove(left, '.').
r(l) :- seeMove(right, '.').
r(l) :- seeMove(right, 'g').
r(l) :- seeDo(up-left, 'g', move(left)).
r(l) :- seeDo(     up, 'g', drop(left)).
r(l) :- doAction(move(up)).

r(h) :- seeMove(right, 'p').
r(h) :- seeMove(down, '.').
r(h) :- seeMove(left, '.').
r(h) :- seeMove(up, '.').
r(h) :- seeMove(right, '.').
r(h) :- seeDo(down-right, 'l', move(right)).
r(h) :- seeMove(left, 'l').
r(h) :- seeDo(down, 'l', drop(left)).
r(h) :- doAction(move(up)).

r(p) :- seeMove(down, '.').
r(p) :- seeMove(left, '.').
r(p) :- seeMove(up, '.').
r(p) :- seeMove(right, '.').
r(p) :- seeMove(down, ' ').
r(p) :- seeMove(left, ' ').
r(p) :- seeMove(left, 'h').
r(p) :- seeDo(down, 'h', drop(right)).
r(p) :- doAction(move(right)).

r(nil) :- seeMove( right-up, 'p', right). 
r(nil) :- seeMove(       up, 'p').
r(nil) :- seeMove(       up, '.').
r(nil) :- see(normal, up, 'E').
r(nil) :- seeMove( down, '.').
r(nil) :- seeMove(right, '.').
r(nil) :- seeDo(  up, 'v', get(up)).
r(nil) :- seeDo(  up, 'g', get(up)).
r(nil) :- seeDo(down, 'b', get(down)).
r(nil) :- seeDo(down, 'h', get(down)).
r(nil) :- seeDo(down, 'l', get(down)).
r(nil) :- seeDo(right, 'p', get(right)).
r(nil) :- seeMove( left, '.').
r(nil) :- seeMove( left, ' ').
r(nil) :- seeMove( left-up, '.', up). 

plman :- havingObject(appearance(OBJ)), r(OBJ).
plman :- r(nil).
