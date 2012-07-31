%% CONCEPTS

object(vertex,Graph,Position) :-
    graph([],[],Graph),
    vertex(X,Y,Z),
    Position = [X,Y,Z]. 

object(segment, Graph, Position) :-
    Object1 =.. [object,vertex,_,[XA,YA,ZA]],
    Object2 =.. [object,vertex,_,[XB,YB,ZB]],
    call(Object1),call(Object2),
    quicksort([Object1,Object2],[Object1,Object2]),
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,unlimited,_)],
          Graph),
    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
    Position = [A,B,C].

object(face, Graph, Position) :-
    Object1 =.. [object,segment,_,[XA,YA,ZA]],
    Object2 =.. [object,segment,_,_],
    Object3 =.. [object,segment,_,[XB,YB,ZB]],
    Object4 =.. [object,segment,_,_],
    call(Object1),call(Object2),call(Object3),call(Object4),
    not(equal(Object1,Object2)),
    not(equal(Object1,Object3)),
    not(equal(Object1,Object4)),
    not(equal(Object2,Object3)),
    not(equal(Object2,Object4)),
    not(equal(Object3,Object4)),
    quicksort([Object1,Object2,Object3,Object4],[Object1,Object2,Object3,Object4]),
    once((permutation([Object1,Object2,Object3,Object4],[PObject1,PObject2,PObject3,PObject4]),
%    call(PObject1),call(PObject2),call(PObject3),call(PObject4),
%    quicksort([PObject1,PObject2],[SObject1,SObject2]), 
    graph([Object1,Object2,Object3,Object4],
                  [cv_arc(PObject1,PObject2,connected,O12c),
                   cv_arc(PObject2,PObject3,connected,O24c),
                   cv_arc(PObject3,PObject4,connected,O34c),
                   cv_arc(PObject4,PObject1,connected,O13c),
                   cv_arc(PObject1,PObject3,parallel,O23p),
                   cv_arc(PObject2,PObject4,parallel,O14p),
                   cv_arc(PObject1,PObject3,similar_size,O23ss),
                   cv_arc(PObject2,PObject4,similar_size,O14ss)],
          Graph))),
%    permutation([Object1,Object2,Object3,Object4],[PObject1,PObject2,PObject3,PObject4]),
%    quicksort([PObject1,PObject2],[SObject1,SObject2]),
%    call(Graph),
    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
    Position = [A,B,C].

%% RELATIONS

unlimited(Object1,Object2) :-
    functor(Object1,object,3),
    functor(Object2,object,3).

% relation stationg that points lay on the line parallel to some axis
axis_parallel(Object1,Object2) :-
    Object1 =.. [object,vertex,_,[XA,YA,ZA]],
    Object2 =.. [object,vertex,_,[XB,YB,ZB]],
    (XA == XB,
     YA == YB,
     ZA =\= ZB;
     XA =\= XB,
     YA == YB,
     ZA == ZB;
     XA == XB,
     YA =\= YB,
     ZA == ZB).

connected(Object1,Object2) :-
    Object1 =.. [object,_,graph(V1,_),_],
    Object2 =.. [object,_,graph(V2,_),_],
    any_equal(V1,V2).

parallel(Object1, Object2) :-
    Object1 =.. [object,segment,graph([V1A,V2A],_),_],
    Object2 =.. [object,segment,graph([V1B,V2B],_),_],
    V1A =.. [object,vertex,_,[XA,YA,ZA]],
    V2A =.. [object,vertex,_,[XB,YB,ZB]],
    V1B =.. [object,vertex,_,[XC,YC,ZC]],
    V2B =.. [object,vertex,_,[XD,YD,ZD]],
    Dot = (XA-XB)*(XC-XD)+(YA-YB)*(YC-YD)+(ZA-ZB)*(ZC-ZD),
    Length1 = sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2),
    Length2 = sqrt((XC-XD)^2+(YC-YD)^2+(ZC-ZD)^2),
    Cos is abs(Dot/(Length1*Length2)),
    Eps is 0.01,
    1-Cos =< Eps.
%    abs((XA-XB)*(YC-YD)-(XC-XD)*(YA-YB)) =< Eps*abs((XC-XD)*(YC-YD)),        % based on the equality of x-y, y-z, z-x planes
%    abs((ZA-ZB)*(YC-YD)-(ZC-ZD)*(YA-YB)) =< Eps*abs((ZC-ZD)*(YC-YD)),        % based on the equality of x-y, y-z, z-x planes
%    abs((XA-XB)*(ZC-ZD)-(XC-XD)*(ZA-ZB)) =< Eps*abs((XC-XD)*(ZC-ZD)).        % based on the equality of x-y, y-z, z-x planes

%parallel(Object1,Object2) :-
%    Object1 =.. [object,_,graph(V1,_),_],
%    Object2 =.. [object,_,graph(V2,_),_],
%    permutation(V1,P1),
%    permutation(V2,P2),
%    list_parallel(P1,P2).

similar_size(Object1, Object2) :-
    Object1 =.. [object,segment,graph([V1A,V2A],_),_],
    Object2 =.. [object,segment,graph([V1B,V2B],_),_],
    V1A =.. [object,vertex,_,[XA,YA,ZA]],
    V2A =.. [object,vertex,_,[XB,YB,ZB]],
    V1B =.. [object,vertex,_,[XC,YC,ZC]],
    V2B =.. [object,vertex,_,[XD,YD,ZD]],
    Eps is 0.2,
    abs(sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2)-sqrt((XC-XD)^2+(YC-YD)^2+(ZC-ZD)^2)) =< Eps.

%connected(seg(V1,V2), seg(V3,V4)) :-
%    seg(V1,V2),
%    seg(V3,V4),
%    not( seg_equal(seg(V1,V2), seg(V3,V4)) ),
%    (list_equal(V1,V3);
%    list_equal(V2,V4);
%    list_equal(V1,V4);

 %connected(Face1,Face2) :-
%    Face1 =.. [object, face, [Seg1A,Seg2A,Seg3A,Seg4A],_],
%    Face2 =.. [object, face, [Seg1B,Seg2B,Seg3B,Seg4B],_],
%    not( equal(Face1, Face2) ),
%    permutation([Seg1A,Seg2A,seg3A,Seg4A],[Seg1C,Seg2C,Seg3C,Seg4C]),
%    permutation([Seg1B,Seg2B,Seg3B,Seg4B],[Seg1D,Seg2D,Seg3D,Seg4D]),
%    (seg_equal(Seg1C,Seg1D);
%     seg_equal(Seg2C,Seg2D);
%     seg_equal(Seg3C,Seg3D);
%     seg_equal(Seg4C,Seg4D)).
%   list_equal(V2,V3)).

% if objects have the same features - in our case position
% 
% vertices doesn't have to be exactly equal
% TODO: mark equal similar vertices. make it possible to detect two faces in current vertices configuration
%equal(Object1,Object2) :-
%    Object1 =.. [object,vertex,graph([],[]),[XA,YA,ZA]],
%    Object2 =.. [object,vertex,graph([],[]),[XB,YB,ZB]],
%    Eps is sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2),
%    Eps =< 0.01,!. 

equal(Object1,Object2) :-
    Object1 =.. [object,Name,graph(V1,_),[XA,YA,ZA]],
    Object2 =.. [object,Name,graph(V2,_),[XB,YB,ZB]],
    Eps is 0.001,
    sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2) =< Eps,
    permutation(V1,P1),
    permutation(V2,P2),
    list_equal(P1,P2).

% ADDITIONAL PROCEDURES

graph(V,E,Graph) :-
    object_list(V),
%    quicksort(V,V1),
    arc_list(E,E1),
%    quicksort(E1,E1),
    Graph = graph(V,E1).

cv_arc(Object1,Object2,RelationName,Arc) :-
%    functor(Object1,object,_),
%    functor(Object2,object,_),
%    call(Object1),
%    call(Object2),
    not( equal(Object1,Object2) ),
    (Arc =.. [arc,Object1,Object2,RelationName];
     Arc =.. [arc,Object2,Object1,RelationName]),
    call(Arc).

arc(Object1, Object2, RelationName) :-
    quicksort([Object1,Object2],[Object1,Object2]),
    Relation =.. [RelationName,Object1,Object2],
    call(Relation).

%    functor(Object1,object,_),
%    functor(Object2,object,_),
%    call(Object1),
%    call(Object2),
%    gt(Object2,Object1),
%    (arc_data(Object1, Object2, RelationName);
%     arc_data(Object2, Object1, RelationName)).

%arc_data(Object1, Object2, RelationName) :-
%    not( equal(Object1,Object2) ),
%    quicksort([Object1,Object2],[Object1,Object2]),
%    Relation =.. [RelationName,Object1,Object2],
%    call(Relation).

object_list([]).

object_list([Object|V]) :-
    functor(Object,object,_),
    call(Object),
    object_list(V).

arc_list([],[]).

arc_list([CVArc|R],[Arc|R1]) :-
    CVArc=..[cv_arc,Object1,Object2,RelationName,Arc],
    call(CVArc),
%    Arc = arc(Object1,Object2,RelationName),
    arc_list(R,R1).

list_equal([],[]).

list_equal([Object1|R1],[Object2|R2]) :-
    equal(Object1,Object2),
    list_equal(R1,R2).

% checks if any element on list 1 is equal to any element on list 2
any_equal([X|_],[Y|_]) :-
    equal(X,Y),!.

any_equal([_|R1],L2) :-
    any_equal(R1,L2),!.

any_equal(L1,[_|R2]) :-
    any_equal(L1,R2).
    
list_parallel([],[]).

list_parallel([Object1|R1],[Object2|R2]) :-
    parallel(Object1,Object2),
    list_equal(R1,R2).

gt(Object1,Object2) :-
    Object1 =.. [object,_,_,[XA,YA,ZA]],
    Object2 =.. [object,_,_,[XB,YB,ZB]],
    (XA > XB;
    XA == XB,
    YA > YB;
    XA == XB,
    YA == YB,
    ZA > ZB). 

gt(Arc1,Arc2) :-
    Arc1=..[arc,ObjectA1,ObjectA2,_,_],
    Arc2=..[arc,ObjectB1,ObjectB2,_,_],
    (gt(ObjectB2,ObjectA2);
     ObjectB2=ObjectA2,
     not(gt(ObjectB1,ObjectA1))).
    

count(Rule,M) :-
    retractall(m(_)),
    assert(m(0)),
    call(Rule),
    m(N),
    M is N+1,
    retract(m(N)),
    assert(m(M)),
    write(M),nl.

%% setof([V1,V2,V3,V4],V1^V2^V3^V4^connected(seg(V1,V2), seg(V3,V4)),Bag), writelist(Bag).
%
%parallel(seg([XA,YA,ZA],[XB,YB,ZB]), seg([XC,YC,ZC],[XD,YD,ZD])) :-
%    seg([XA,YA,ZA],[XB,YB,ZB]),
%    seg([XC,YC,ZC],[XD,YD,ZD]),
%    not( seg_equal(seg([XA,YA,ZA],[XB,YB,ZB]), seg([XC,YC,ZC],[XD,YD,ZD])) ),
%    Eps is 0.01,
%    abs((XA-XB)*(YC-YD)-(XC-XD)*(YA-YB)) =< Eps*abs((XC-XD)*(YC-YD)),        % based on the equality of x-y, y-z, z-x planes
%    abs((ZA-ZB)*(YC-YD)-(ZC-ZD)*(YA-YB)) =< Eps*abs((ZC-ZD)*(YC-YD)),        % based on the equality of x-y, y-z, z-x planes
%    abs((XA-XB)*(ZC-ZD)-(XC-XD)*(ZA-ZB)) =< Eps*abs((XC-XD)*(ZC-ZD)).        % based on the equality of x-y, y-z, z-x planes
%%    abs((XA-XB)/(XC-XD)-(YA-YB)/(YC-YD)) < eps,        % based on the equality of x-y, y-z, z-x planes
%%    abs((ZA-ZB)/(ZC-ZD)-(YA-YB)/(YC-YD)) < eps,
%%    abs((XA-XB)/(XC-XD)-(ZA-ZB)/(ZC-ZD)) < eps,
%%    abs((YA-YB)/(XA-XB)-(YC-YD)/(XC-XD)) < eps,        % based on the equality of x-y, y-z, z-x planes
%%    abs((ZA-ZB)/(YA-YB)-(ZC-ZD)/(YC-YD)) < eps,
%%    abs((XA-XB)/(ZA-ZB)-(XC-XD)/(ZC-ZD)) < eps,
%
%similar_length(seg([XA,YA,ZA],[XB,YB,ZB]), seg([XC,YC,ZC],[XD,YD,ZD])) :-
%    seg([XA,YA,ZA],[XB,YB,ZB]),
%    seg([XC,YC,ZC],[XD,YD,ZD]),
%    not( seg_equal(seg([XA,YA,ZA],[XB,YB,ZB]), seg([XC,YC,ZC],[XD,YD,ZD])) ),
%    Eps is abs(sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2)-sqrt((XC-XD)^2+(YC-YD)^2+(ZC-ZD)^2)),
%    Eps =< 0.2. 
%
%
%arc(Seg1, Seg2, RelationName) :-
%    Seg1 =.. [seg,_,_],
%    Seg2 =.. [seg,_,_],
%    call(Seg1),
%    call(Seg2),
%    not( seg_equal(Seg1,Seg2) ),
%    Relation =.. [RelationName,Seg1,Seg2],
%    call(Relation).
%
%object(face, Graph, Position) :-
%    Arc1 =.. [arc,Seg1,Seg2,connected],
%    Arc2 =.. [arc,Seg2,Seg3,connected],
%    Arc3 =.. [arc,Seg3,Seg4,connected],
%    Arc4 =.. [arc,Seg4,Seg1,connected],
%    Arc5 =.. [arc,Seg2,Seg4,parallel],
%    Arc6 =.. [arc,Seg1,Seg3,parallel],
%    Arc7 =.. [arc,Seg1,Seg3,similar_length],
%    Arc8 =.. [arc,Seg2,Seg4,similar_length],
%    call(Arc1), call(Arc2), call(Arc3), call(Arc4), call(Arc5), call(Arc6), call(Arc7), call(Arc8),
%    Graph = graph([Seg1,Seg2,Seg3,Seg4],[Arc1,Arc2,Arc3,Arc4,Arc5,Arc6,Arc7,Arc8]),
%    Seg1 =.. [seg,[XA,YA,ZA],[XB,YB,ZB]],
%    Seg2 =.. [seg,[XC,YC,ZC],[XD,YD,ZD]],
%    A is (XA-XD)/2, B is (YA-YD)/2, C is (ZA-ZD)/2,
%    Position = [A,B,C].
%
%% setof([Seg1,Seg2,Seg3,Seg4],Seg1^Seg2^Seg3^Seg4^object(face, [Seg1, Seg2, Seg3, Seg4]),Bag), writelist(Bag).
%% setof([Seg1,Seg2,Seg3,Seg4],Seg1^Seg2^Seg3^Seg4^object(face, graph([Seg1, Seg2, Seg3, Seg4],_),_),Bag), writelist(Bag).
%% setof([A,B,C],A^B^C^object(face,_,[A,B,C]),Bag), writelist(Bag).
%
%equal(Face1,Face2) :-
%    Face1 =.. [object, face, [Seg1A,Seg2A,Seg3A,Seg4A],_],
%    Face2 =.. [object, face, [Seg1B,Seg2B,Seg3B,Seg4B],_],
%    permutation([Seg1A,Seg2A,seg3A,Seg4A],P1),
%    permutation([Seg1B,Seg2B,Seg3B,Seg4B],P2),
%    not( seg_list_equal(P1,P2) ).
%
%seg_list_equal(Seg1,Seg2) :-
%    seg_equal(Seg1,Seg2).
%
%seg_list_equal([Seg1|R1],[Seg2|R2]) :-
%    seg_list_equal(R1,R2),
%    seg_equal(Seg1,Seg2).
%

%%parallel(Face1,Face2) :-
%%    Face1 =.. [object, face, [Seg1A,Seg2A,Seg3A,Seg4A],_],
%%    Face2 =.. [object, face, [Seg1B,Seg2B,Seg3B,Seg4B],_],
%%    not( equal(Face1, Face2) ),
%%    permutation([Seg1A,Seg2A,seg3A,Seg4A],[Seg1C,Seg2C,Seg3C,Seg4C]),
%%    permutation([Seg1B,Seg2B,Seg3B,Seg4B],[Seg1D,Seg2D,Seg3D,Seg4D]),
%%    parallel(Seg1C,Seg1D),
%%    parallel(Seg2C,Seg2D),
%%    parallel(Seg3C,Seg3D),
%%    parallel(Seg4C,Seg4D)).
%%
%similar_size(Face1,Face2) :-
%    Face1 =.. [object, face, [Seg1A,Seg2A,Seg3A,Seg4A],_],
%    Face2 =.. [object, face, [Seg1B,Seg2B,Seg3B,Seg4B],_],
%    not( equal(Face1, Face2) ),
%    Eps is abs(Seg1A*Seg2A-Seg1B*Seg2B),
%    Eps =< 0.2.
%
%arc(Face1, Face2, RelationName) :-
%    Seg1 =.. [seg,_,_],
%    Seg2 =.. [seg,_,_],
%    call(Seg1),
%    call(Seg2),
%    not( seg_equal(Seg1,Seg2) ),
%    Relation =.. [RelationName,Seg1,Seg2],
%    call(Relation).
%
%%object(leg, Graph, Position) :-
%%    Arc1 =.. [arc,Face1,Face3,connected],
%%    Arc1 =.. [arc,Face1,Face4,connected],
%%    Arc1 =.. [arc,Face1,Face5,connected],
%%    Arc1 =.. [arc,Face1,Face6,connected],
%%    Arc1 =.. [arc,Face2,Face3,connected],
%%    Arc1 =.. [arc,Face2,Face4,connected],
%%    Arc1 =.. [arc,Face2,Face5,connected],
%%    Arc1 =.. [arc,Face2,Face6,connected],
%%    Arc1 =.. [arc,Face3,Face4,connected],
%%    Arc1 =.. [arc,Face4,Face5,connected],
%%    Arc1 =.. [arc,Face5,Face6,connected],
%%    Arc1 =.. [arc,Face6,Face3,connected],
%%    Arc1 =.. [arc,Face1,Face2,parallel],
%%    Arc1 =.. [arc,Face3,Face5,parallel],
%%    Arc1 =.. [arc,Face4,Face6,parallel],
%%    Arc1 =.. [arc,Face1,Face2,similar_size],
%%    Arc1 =.. [arc,Face3,Face4,similar_size],
%%    Arc1 =.. [arc,Face3,Face5,similar_size],
%%    Arc1 =.. [arc,Face3,Face6,similar_size],
%%    Arc1 =.. [arc,Face4,Face5,similar_size],
%%    Arc1 =.. [arc,Face4,Face6,similar_size],
%%    Arc1 =.. [arc,Face5,Face6,similar_size],
%%    call(Arc1), call(Arc2), call(Arc3), call(Arc4), call(Arc5), call(Arc6), call(Arc7), call(Arc8),
%%    Graph = graph([Seg1,Seg2,Seg3,Seg4],[Arc1,Arc2,Arc3,Arc4,Arc5,Arc6,Arc7,Arc8]),
%%    Seg1 =.. [seg,[XA,YA,ZA],[XB,YB,ZB]],
%%    Seg2 =.. [seg,[XC,YC,ZC],[XD,YD,ZD]],
%%    A is (XA-XD)/2, B is (YA-YD)/2, C is (ZA-ZD)/2,
%%    Position = [A,B,C].
%%
%


writelist([]).

writelist([X|L]) :-
    write(X),nl,
    writelist(L).

permutation([],[]).

permutation([X|L],P) :-
    permutation(L,L1),
    insert(X,L1,P).

insert(X,List,BiggerList) :-
    del(X,BiggerList,List).

del(X,[X|Tail],Tail).

del(X,[Y|Tail],[Y|Tail1]) :-
    del(X,Tail,Tail1).

quicksort([],[]).

quicksort([X|Tail],Sorted)  :-
   split( X, Tail, Small, Big),
   quicksort( Small, SortedSmall),
   quicksort( Big, SortedBig),
   conc( SortedSmall, [X|SortedBig], Sorted).

split(_,[],[],[]).

split(X,[Y|Tail],[Y|Small],Big)  :-
   gt( X, Y), !,
   split( X, Tail, Small, Big).

split(X,[Y|Tail], Small,[Y|Big])  :-
   split( X, Tail, Small, Big).

conc([],L,L).

conc([X|L1],L2,[X|L3]) :-
   conc(L1,L2,L3).
