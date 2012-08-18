%% INFERENCE ENGINE
%%% by Michal Lisicki (2012)
%% SUPPORTING PROCEDURES

% GENERAL

list_call(_,[],[]).

list_call(Name,[X|R1],[Y|R2]) :-
    Proc =.. [Name,X,Y],
    call(Proc),
    list_call(Name,R1,R2).

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

diff_list([],L2,[],L2).

diff_list([X|R],L,DL1,DL2) :-
    diff_list(R,L,DL1,DL),
    del(X,DL,DL2),!.

diff_list([X|R],L,[X|DL1],DL2) :-
    diff_list(R,L,DL1,DL2).

combination(_,[]).

combination([X|T],[X|Comb]):-
    combination(T,Comb).

combination([_|T],[X|Comb]):-
    combination(T,[X|Comb]).

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

% concatenate with no repetition
nr_conc([],L,L).

nr_conc([X|L1],L2,L3) :-
   nr_conc(L1,L2,L3),
   member(X,L3),!.

nr_conc([X|L1],L2,[X|L3]) :-
   nr_conc(L1,L2,L3).

nr_add_list([],L,L).

nr_add_list([X|L1],L2,L4) :-
    nr_add(X,L2,L3),
    nr_add_list(L1,L3,L4).

nr_add(X,L,L) :-
    member(X,L),!.

nr_add(X,L,[X|L]).

count(Rule,M) :-
    retractall(m(_)),
    assert(m(0)),
    call(Rule),
    m(N),
    M is N+1,
    retract(m(N)),
    assert(m(M)),
    write(Rule),nl,
    write(M),nl.

max(X,Y,X) :-
    X >= Y.

max(X,Y,Y) :-
    X < Y.

maxlist([X],X).

maxlist([X,Y|Rest],Max) :-
    maxlist([Y|Rest],MaxRest),
    max(X,MaxRest,Max).



% PROBLEM SPECIFIC

% returns positions of vertices
show_vertices(Object,[P]) :-
    Object =.. [object,vertex,_,P],!.
    
show_vertices(Object,OL) :-
    Object =.. [object,_,graph(Vs,_),_],
    show_vertices_list(Vs,OL).

show_vertices_list([],[]).

show_vertices_list([V|R],L) :-
    show_vertices_list(R,L1),
    show_vertices(V,L2),
    nr_add_list(L1,L2,L).

% returns actual vertices
show_vertices2(Object,[Object]) :-
    Object =.. [object,vertex,_,P],!.

show_vertices2(Object,OL) :-
    Object =.. [object,_,graph(Vs,_),_],
    show_vertices_list2(Vs,OL).

show_vertices_list2([],[]).

show_vertices_list2([V|R],L) :-
    show_vertices_list2(R,L1),
    show_vertices2(V,L2),
    nr_add_list(L1,L2,L).

list_fp_vertices(Object,L) :-
    (Type = face_pair ; Type = square_face_pair),
    Object =.. [object,Type,graph([Object1,Object2],_),_],
    show_vertices(Object1,L1),
    show_vertices(Object2,L2),
    diff_list(L1,L2,L3,L4),
    conc(L3,L4,L).

max_dist([DX,X],[DY,Y],[DX,X]) :-
    DX >= DY,!.

max_dist([DX,X],[DY,Y],[DY,Y]).

max_dist_list([X],X).

max_dist_list([X,Y|Rest],Max) :-
    max_dist_list([Y|Rest],MaxRest),
    max_dist(X,MaxRest,Max).

vertex_dist([A,B],D) :-
    arg(3,A,[XA,YA,ZA]),
    arg(3,B,[XB,YB,ZB]),
    D is sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2).
graph(V,E,Graph) :-
    arc_list(E,E1),
    Graph = graph(V,E1).

cv_arc(Object1,Object2,RelationName,Arc) :-
    not( equal(Object1,Object2) ),
    (Arc =.. [arc,Object1,Object2,RelationName];
     Arc =.. [arc,Object2,Object1,RelationName]),
    call(Arc).

arc(Object1, Object2, RelationName) :-
    quicksort([Object1,Object2],[Object1,Object2]),
    Relation =.. [RelationName,Object1,Object2],
    call(Relation).

object_list([]).

object_list([Object|V]) :-
    functor(Object,object,_),
    call(Object),
    object_list(V).

member_list([],_).

member_list([X|R],L) :-
    member(X,L),
    member_list(R,L).

arc_list([],[]).

arc_list([CVArc|R],[Arc|R0]) :-
    CVArc=..[cv_arc,_,_,_,Arc],
    call(CVArc),
    arc_list(R,R0).

list_equal([],[]).

list_equal([Object1|R1],[Object2|R2]) :-
    equal(Object1,Object2),
    list_equal(R1,R2).

%equal(Object1,Object2) :-
%    Object1 =.. [object,vertex,G,P],!,
%    Object2 =.. [object,vertex,G,P].

equal(Object1,Object2) :-
    Object1 =.. [object,Name,graph(V1,_),[XA,YA,ZA]],
    Object2 =.. [object,Name,graph(V2,_),[XB,YB,ZB]],
    Eps is 0.001,
    sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2) =< Eps,
    permutation(V1,P1),
    permutation(V2,P2),
    list_equal(P1,P2).

equal_pos(Object1,Object2) :-
    Object1 =.. [object,Name,graph(_,_),[XA,YA,ZA]],
    Object2 =.. [object,Name,graph(_,_),[XB,YB,ZB]],
    Eps is 0.001,
    sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2) =< Eps.

% checks if any element on list 1 is equal to any element on list 2

any_equal([X|_],[Y|_]) :-
    equal(X,Y),!.

any_equal([_|R1],L2) :-
    any_equal(R1,L2),!.

any_equal(L1,[_|R2]) :-
    any_equal(L1,R2).

pair_diff([]).

pair_diff([X|L]) :-
    not(any_equal([X],L)),
    pair_diff(L).

list_parallel([],[]).

list_parallel([Object1|R1],[Object2|R2]) :-
    parallel(Object1,Object2),
    list_parallel(R1,R2).

list_oparallel([],[]).

list_oparallel([Object1|R1],[Object2|R2]) :-
    opposite_parallel(Object1,Object2),
    list_oparallel(R1,R2).

gt(Object1,Object2) :-
    (Type = face_pair; Type = segment_pair; Type = square_face_pair),
    Object1 =.. [object,Type,graph([_,SA],_),_],
    Object2 =.. [object,Type,graph([SB,_],_),_],!,
    gt(SB,SA).

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

corner(Object,PV1A) :-
    Object =.. [object,segment_pair,graph([SO1,SO2],_),_],
    SO1 =.. [object,segment,graph([V1A,V2A],_),_],
    SO2 =.. [object,segment,graph([V1B,V2B],_),_],
    permutation([V1A,V2A],[PV1A,PV2A]),
    permutation([V1B,V2B],[PV1B,PV2B]),
    equal(PV1A,PV1B).

extract_objects(_,[],[]).

extract_objects(Type,[Node|RNodes],Objects) :-
    Node =.. [ot,_,_,_,Elements],
    Bucket =.. [Type,OL],
    del(Bucket,Elements,_),
    extract_objects(Type,RNodes,RObjects),
    nr_add_list(OL,RObjects,Objects).

size(Object,Size) :-
    Object =.. [object,segment,graph(Vs,_),_],
    vertex_dist(Vs,Size).

size(Object,Size) :-
    Object =.. [object,face,graph([SP,_],_),_],
    Object =.. [object,segment_pair,graph([SP1,SP2],_),_],
    SP1 =.. [object,segment,graph(SP1V,_),_],
    SP2 =.. [object,segment,graph(SP2V,_),_],
    vertex_dist(SP1V,SP1D),
    vertex_dist(SP2V,SP2D),
    Size is SP1D * SP2D.

dim(Object,H,W) :-
    Object =.. [object,square_cuboid,graph([FP,_],_),_],
    FP =.. [object,face_pair,graph([FA,FB],_),_],
    FA =.. [object,face,graph(SPA,_),_],
    FB =.. [object,face,graph(SPB,_),_],
    permutation(SPA,[SPA1,_]),
    permutation(SPB,[SPB1,_]),
    SPA1 =.. [object,segment_pair,graph(SPA1S,_),_],
    SPB1 =.. [object,segment_pair,graph(SPB1S,_),_],
    permutation(SPA1S,[P,P1]),
    permutation(SPB1S,[P,P2]),
    size(P,H),
    size(P1,W1),
    size(P2,W2),
    W is (W1+W2)/2,!.

side_size(Object,Size) :-
    Object =.. [object,square_cuboid,graph([FP,_],_),_],
    FP =.. [object,face_pair,graph([F,_],_),_],
    size(F,Size).
   
% calculate plane from 3 points based on Cramers rule
plane([PA,PB,PC],[A,B,C,D]) :-
    PA = [XA,YA,ZA],
    PB = [XB,YB,ZB],
    PC = [XC,YC,ZC],
    DD is XA*YB*ZC + YA*ZB*XC + ZA*XB*YC - XC*YB*ZA - YC*ZB*XA - XB*YA*ZC,
    D = 1,  % can be any number besides 0
    A is -D/DD*(1*YB*ZC + YA*ZB*1 + ZA*1*YC - ZA*YB*1 - YA*1*ZC - 1*ZB*YC),
    B is -D/DD*(XA*1*ZC + 1*ZB*XC + ZA*XB*1 - XC*1*ZA - 1*ZB*XA - XB*1*ZC),
    C is -D/DD*(XA*YB*1 + YA*1*XC + 1*XB*YC - XC*YB*1 - YC*1*XA - XB*YA*1).

cmpr(_,[]).

cmpr(Points,[Face|RFaces]) :-
    show_vertices(Face,[P1,P2,P3,P4]),
    plane([P1,P2,P3],Plane),
    (cmpr_gt(Points,Plane);
     cmpr_lt(Points,Plane)),
     cmpr(Points,RFaces).

%check if all the points lay on the same side of the plane 

cmpr_gt([],_). 

cmpr_gt([Point|RPoints],Plane) :-
    cmpr_res(Point,Plane,Res),
    Res >= -0.1,
    cmpr_gt(RPoints,Plane).

cmpr_lt([],_).

cmpr_lt([Point|RPoints],Plane) :-
    cmpr_res(Point,Plane,Res),
    Res =< 0.1,
    cmpr_lt(RPoints,Plane).

cmpr_res([X,Y,Z],[A,B,C,D],Res) :-
    Res is A*X+B*Y+C*Z+D.

% octree(cuboid,OT,NE), object(leg,G1,P1,_,NE), object(seat,G2,P2,_,NE), O1=..[object,cuboid,G1,P1], O2=..[object,cuboid,G2,P2], cv_arc(O1,O2,not_colliding,_).

top_FP_plane([F1,F2],Plane) :-
    FA =.. [object,face,graph(SPA,_),_],
    FB =.. [object,face,graph(SPB,_),_],
    permutation(SPA,[SPA1,_]),
    permutation(SPB,[SPB1,_]),
    SPA1 =.. [object,segment_pair,graph(SPA1S,_),_],
    SPB1 =.. [object,segment_pair,graph(SPB1S,_),_],
    permutation(SPA1S,[P,P1]),
    permutation(SPB1S,[P,P2]),
    show_vertices(P1,P1A),    
    show_vertices(P2,P2A),
    permutation(P1A,[A,B]),
    permutation(P2A,[A,C]),
    plane([A,B,C],Plane).

bb_max(Object,[X,Y,Z]) :-
    show_vertices(Object,Vs),
    member([X,_,_],Vs), not((member([X1,_,_],Vs),X1>X)), 
    member([_,Y,_],Vs), not((member([_,Y1,_],Vs),Y1>Y)), 
    member([_,_,Z],Vs), not((member([_,_,Z1],Vs),Z1>Z)),!. 

bb_min(Object,[X,Y,Z]) :-
    show_vertices(Object,Vs),
    member([X,_,_],Vs), not((member([X1,_,_],Vs),X1<X)), 
    member([_,Y,_],Vs), not((member([_,Y1,_],Vs),Y1<Y)), 
    member([_,_,Z],Vs), not((member([_,_,Z1],Vs),Z1<Z)),!. 

points_between([XA,YA,ZA],[XB,YB,ZB],L) :-
    points_between([XA,YA,ZA],[XB,YB,ZB],1,L).

points_between(_,_,R,[]) :-
    R < 0,!.

points_between([XA,YA,ZA],[XB,YB,ZB],R,[[XC,YC,ZC]|L]) :-
    XC is R*XA + (1-R)*XB,
    YC is R*YA + (1-R)*YB,
    ZC is R*ZA + (1-R)*ZB,
    points_between([XA,YA,ZA],[XB,YB,ZB],(R-0.05),L).
