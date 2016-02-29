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

r('!') :- cdo([ l('.'), dl('.'), d('.'), r(' ') ],  			move( down)).
r('!') :- cdo([ ul(#), l('.'), dl('.'), d(' '), r(' '), u('.') ],	move(   up)).
r('!') :- cdo([ dl('O'), d('.'), dr(f) ],  				move( down)).
r('!') :- cdo([ ul(#), l('.') ],  					move( left)).
r('!') :- cdo([ ul(p), l('.') ],  					move( left)).
r('!') :- cdo([ ul('E'), r('.') ],  					move( right)).
r('!') :- cdo([ ul('E'), l('.') ],  					move( left)).
r('!') :- cdo([ r('.'), dr('.'), d('.'), dl(' '), l(' '), ur(#), u(#), ul(#) ],		move(right)). 
r('!') :- cdo([ d('.'), r(#), l('.') ],  		move( left)).
r('!') :- cdo([ r('.'), d('.'), l(' '), u(' ') ],	move(right)).
r('!') :- cdo([ d('.') ],  				move(	 down)).
r('!') :- cdo([ dr(b), l('.') ],  			move( left)).
r('!') :- cdo([ dr('S'), u('.') ],  			move( up)).
r('!') :- cdo([ l('.'), u('.'), ur('#'), dl(' ') ],  	move(left)).
r('!') :- cdo([ l('.'), u('.'), ur('#') ],  		move(  up)).
r('!') :- cdo([ d('s') ],  				use(flIpEnDO, down)).
r('!') :- cdo([ r('.') ],  				move(right)).
r('!') :- cdo([ dl(b), d(' ') ],			move( down)).
r('!') :- cdo([ ul(s),   dl('L') ],			move( none)).
r('!') :- cdo([ u(s),    dl('L') ],			use(flIpEnDO, up)).
r('!') :- cdo([ ul('.'), dl('L') ],			move(   up)).
r('!') :- cdo([ u('.'), d('E') ], 			move(   up)).
r('!') :- cdo([ l('.') ],  				move( left)).
r('!') :- cdo([ r('D'), ur(#) ],			use(flIpEnDO, right)).
r('!') :- cdo([ u('.') ],  				move(   up)).
r('!') :- cdo([ r(#), ur(#), dr(#) ],			move( left)).
r('!') :- cdo([ dl('.'), d(' ') ], 			move( down)).
r('!') :- cdo([ dl('0') ],  				move( left)).
r('!') :- cdo([ d('0') ],  				move( left)).
r('!') :- cdo([ dr('0') ],  				move( left)).
r('!') :- cdo([ dl(n), l(' ') ],			move( left)).
r('!') :- cdo([ l(b) ],  				use(flIpEnDO, right)).
r('!') :- cdo([ d(n), l(' ') ],				move( left)).
r('!') :- cdo([ dr(n), d(' ') ],			move( down)).
r('!') :- cdo([ dl(f), r(n), d(b) ],			move( left)). 
r('!') :- cdo([ dl(f), r(n) ],				move( down)). 
r('!') :- cdo([ d(f), r(b) ],				use(flIpEnDO, down)).
r('!') :- cdo([ u(' ') ],  				move(   up)).
r('!') :- cdo([ u(f) ],  				use(flIpEnDO, up)).
r('!') :- cdo([ r(f) ],  				use(flIpEnDO, right)).
r('!') :- cdo([ r(' '), d(' '), dr(#) ],  		move(right)).
r('!') :- cdo([ ur('.'), r(' ') ],  			move(right)).
r('!') :- cdo([ l(f), d(' ') ],  			use(flIpEnDO, left)).
r('!') :- cdo([ ur('#'), r(' '), d(' ') ],  		move(right)).
r('!') :- cdo([ l(' '), dr(' '), r(' '), d(#), ul(#) ],	move(right)).
r('!') :- cdo([ l(' ') ],  				move( left)).
r('!') :- cdo([ l(n) ],  				use(flIpEnDO, left)).
r('!') :- cdo([ l('D'), d('O') ], 			use(flIpEnDO, left)).
r('!') :- cdo([ r(' ') ],  				move( right)).
r('!') :- cdo(        [],  			move(left)).

plman :- havingObject(appearance(OBJ)), r(OBJ).
plman :- cdo([ r('!') ],   get(right)).
plman :- cdo(        [],  move(right)).
