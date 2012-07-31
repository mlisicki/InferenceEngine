t( t( nil, b, nil ), a, t( t( nil, d, nil ), c, nil) ).

% BINARY TREE
binarytree(nil).

binarytree(t(Left,_,Right)) :-
    binarytree(Left),
    binarytree(Right).
    
bt_in(X,t(_,X,_)).

bt_in(X,t(L,_,_)) :-
    bt_in(X,L).

bt_in(X,t(_,_,R)) :-
    bt_in(X,R).

% BINARY DICTIONARY
dictionary(nil).

dictionary(t(Left,Root,Right)) :-
    Left = t(_,RL,_),
    Right = t(_,RR,_),
    gt(Root,RL),
    gt(RR,Root),
    dictionary(Left),
    dictionary(Right).

in(X,t(_,X,_)).

in(X,t(Left,Root,_)) :-
    gt(Root,X),
    in(X,Left).

in(X,t(_,Root,Right)) :-
    gt(X,Root),
    in(X,Right).

addleaf(nil,X,t(nil,X,nil)).

addleaf(t(Left,X,Right),X,t(Left,X,Right)).

addleaf(t(Left,Root,Right),X,t(Left1,Root,Right)) :-
    gt(Root,X),
    addleaf(Left,X,Left1).

addleaf(t(Left,Root,Right),X,t(Left,Root,Right)) :-
    gt(X,Root),
    addleaf(Right,X,Right1).

del(t(nil,X,Right),X,Right).

del(t(Left,X,nil),X,Left).

del(t(Left,X,Right),X,t(Left,Y,Right1)) :-
    delmin(Right,Y,Right1).

del(t(Left,Root,Right),X,t(Left1,Root,Right)) :-
    gt(Root,X),
    del(Left,X,Left1).

del(t(Left,Root,Right),X,t(Left,Root,Right1)) :-
    gt(X,Root),
    del(Right,X,Right1).

delmin(t(nil,Y,Right),Y,Right).

delmin(t(Left,Root,Right),Y,t(Left1,Root,Right)) :-
    delmin(Left,Y,Left1).

gt(X,Y) :-
    X>Y.
