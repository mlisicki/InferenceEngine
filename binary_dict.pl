t( t( nil, b, nil ), a, t( t( nil, d, nil ), c, nil) ).

in(X,t(_,X,_)).

in(X,t(Left,Root,Right)) :-
    gt(Root,X),
    in(X,Left).

in(X,t(Left,Root,Right)) :-
    gt(X,Root),
    in(X,Right).

add(Tree,X,NewTree) :-
    addroot(Tree,X,NewTree).

add(t(L,Y,R),X,t(L1,Y,R)) :-
    gt(Y,X),
    add(L,X,L1).

add(t(L,Y,R),X,t(L,Y,R1)) :-
    gt(X,Y),
    add(R,X,R1).

addroot(nil,X,t(nil,X,nil)).

addroot(t(L,Y,R),X,t(L1,X,t(L2,Y,R))) :-
    gt(Y,X),
    addroot(L,X,t(L1,X,L2)).

addroot(t(L,Y,R),X,t(t(L,Y,R1),X,R2)) :-
    gt(X,Y),
    addroot(R,X,t(R1,X,R2)).

gt(X,Y) :-
    X>Y.
