seg([XA,YA,ZA],[XB,YB,ZB]) :-
    vertex(XA,YA,ZA),
    vertex(XB,YB,ZB),
    (XA == XB,
     YA == YB,
     ZA =\= ZB;
     XA =\= XB,
     YA == YB,
     ZA == ZB;
     XA == XB,
     YA =\= YB,
     ZA == ZB).

list_equal(X,X).

list_equal([X|R1],[X|R2]) :-
    list_equal(R1,R2).

seg_equal(seg(V1,V2), seg(V3,V4)) :-
    (list_equal(V1,V3),
     list_equal(V2,V4));
    (list_equal(V1,V4),
     list_equal(V2,V3)).

% not(P) :-
%    P,!,fail;
%    true.

connected(seg(V1,V2), seg(V3,V4)) :-
    seg(V1,V2),
    seg(V3,V4),
    not( seg_equal(seg(V1,V2), seg(V3,V4)) ),
    (list_equal(V1,V3);
    list_equal(V2,V4);
    list_equal(V1,V4);
    list_equal(V2,V3)).
