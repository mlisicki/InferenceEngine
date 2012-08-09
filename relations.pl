%% INFERENCE ENGINE
%%% by Michal Lisicki (2012)
%% RELATIONS

unlimited(Object1,Object2) :-
    functor(Object1,object,3),
    functor(Object2,object,3).

% relation stationg that points lay on the line parallel to some axis
axis_parallel(Object1,Object2) :-
    Object1 =.. [object,vertex,_,[XA,YA,ZA]],
    Object2 =.. [object,vertex,_,[XB,YB,ZB]],
    Eps is 0.001,
    (abs(XA-XB)<Eps,
     abs(YA-YB)<Eps;
     abs(YA-YB)<Eps,
     abs(ZA-ZB)<Eps;
     abs(XA-XB)<Eps,
     abs(ZA-ZB)<Eps).     

connected(Object1,Object2) :-
    Object1 =.. [object,face,graph([S1A,S2A],_),_],
    Object2 =.. [object,face,graph([S1B,S2B],_),_],!,
    S1A =.. [object,segment_pair,graph(S1AS,_),_],
    S2A =.. [object,segment_pair,graph(S2AS,_),_],
    S1B =.. [object,segment_pair,graph(S1BS,_),_],
    S2B =.. [object,segment_pair,graph(S2BS,_),_],
    conc(S1AS,S2AS,SA), % get all face segments
    conc(S1BS,S2BS,SB),
    any_equal(SA,SB),!.

connected(Object1,Object2) :-
    (Type = segment_pair; Type = face_pair ; Type = square_face_pair),
    Object1 =.. [object,Type,graph([S1A,S2A],_),_],
    Object2 =.. [object,Type,graph([S1B,S2B],_),_],!,
    permutation([S1A,S2A],[PS1A,PS2A]),
    permutation([S1B,S2B],[PS1B,PS2B]),
    connected(PS1A,PS1B),
    connected(PS2A,PS2B),!.

connected(Object1,Object2) :-
    Object1 =.. [object,_,graph(V1,_),_],
    Object2 =.. [object,_,graph(V2,_),_],
    any_equal(V1,V2).

opposite_parallel(Object1,Object2) :-
    (Type = segment_pair; Type = face_pair; Type = square_face_pair),
    Object1 =.. [object,Type,graph([S1A,S2A],_),[XSPA,YSPA,ZSPA]],
    Object2 =.. [object,Type,graph([S1B,S2B],_),[XSPB,YSPB,ZSPB]],
    permutation([S1A,S2A],[PS1A,PS2A]),
    permutation([S1B,S2B],[PS1B,PS2B]),
    parallel(PS1A,PS1B),
    parallel(PS2A,PS2B),!,
    arg(3,PS1A,[XS1A,YS1A,ZS1A]),
    arg(3,PS1B,[XS1B,YS1B,ZS1B]),
    arg(3,PS2A,[XS2A,YS2A,ZS2A]),
    arg(3,PS2B,[XS2B,YS2B,ZS2B]),
    Eps is 0.001,
    A1 is (XS1A+XS1B)/2, B1 is (YS1A+YS1B)/2, C1 is (ZS1A+ZS1B)/2,
    sqrt((A1-XSPA)^2+(B1-YSPA)^2+(C1-ZSPA)^2) =< Eps,
    A2 is (XS2A+XS2B)/2, B2 is (YS2A+YS2B)/2, C2 is (ZS2A+ZS2B)/2,
    sqrt((A2-XSPA)^2+(B2-YSPA)^2+(C2-ZSPA)^2) =< Eps,!.

parallel(Object1, Object2) :-
    Object1 =.. [object,segment,graph([V1A,V2A],_),_],
    Object2 =.. [object,segment,graph([V1B,V2B],_),_],!,
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

parallel(Object1,Object2) :-
    Object1 =.. [object,face,graph([S1A,S2A],_),_],
    Object2 =.. [object,face,graph([S1B,S2B],_),_],
    S1A =.. [object,segment_pair,graph(S1AS,_),_],
    S2A =.. [object,segment_pair,graph(S2AS,_),_],
    S1B =.. [object,segment_pair,graph(S1BS,_),_],
    S2B =.. [object,segment_pair,graph(S2BS,_),_],
    conc(S1AS,S2AS,SA), % get all face segments
    conc(S1BS,S2BS,SB),
    permutation(SA,PA),
    permutation(SB,PB),
    list_parallel(PA,PB),!.

similar_size(Object1, Object2) :-
    Object1 =.. [object,segment,graph([V1A,V2A],_),_],
    Object2 =.. [object,segment,graph([V1B,V2B],_),_],
    V1A =.. [object,vertex,_,[XA,YA,ZA]],
    V2A =.. [object,vertex,_,[XB,YB,ZB]],
    V1B =.. [object,vertex,_,[XC,YC,ZC]],
    V2B =.. [object,vertex,_,[XD,YD,ZD]],
    Eps is 0.1,
    abs(sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2)-sqrt((XC-XD)^2+(YC-YD)^2+(ZC-ZD)^2)) =< Eps.

%similar_size(Object1, Object2) :-
%    Object1 =.. [object,segment_pair,graph([S1A,S2A],_),_],
%    Object2 =.. [object,segment_pair,graph([S1B,S2B],_),_],
%    S1A =.. [object,segment,graph(S1AV,_),_],
%    S2A =.. [object,segment,graph(S2AV,_),_],
%    S1B =.. [object,segment,graph(S1BV,_),_],
%    S2B =.. [object,segment,graph(S2BV,_),_],
%    vertex_dist(S1AV,S1AD),
%    vertex_dist(S2AV,S2AD),
%    vertex_dist(S1BV,S1BD),
%    vertex_dist(S2BV,S2BD),
%    Eps is 0.25,
%    write(S1AD*S2AD-S1BD*S2BD),nl,
%    abs(S1AD*S2AD-S1BD*S2BD) =< Eps.

similar_size(Object1, Object2) :-
    Object1 =.. [object,segment_pair,graph(SA,_),_],
    Object2 =.. [object,segment_pair,graph(SB,_),_],
    permutation(SA,[S1A,S2A]),
    permutation(SB,[S1B,S2B]),
    S1A =.. [object,segment,graph(S1AV,_),_],
    S2A =.. [object,segment,graph(S2AV,_),_],
    S1B =.. [object,segment,graph(S1BV,_),_],
    S2B =.. [object,segment,graph(S2BV,_),_],
    vertex_dist(S1AV,S1AD),
    vertex_dist(S2AV,S2AD),
    vertex_dist(S1BV,S1BD),
    vertex_dist(S2BV,S2BD),
    Eps is 0.03,
    abs(S1AD-S1BD) =< Eps,
    abs(S2AD-S2BD) =< Eps,!.

similar_size(Object1, Object2) :-
    Object1 =.. [object,face,graph([S1A,_],_),_],
    Object2 =.. [object,face,graph([S1B,_],_),_],
    similar_size(S1A,S1B).
%    S1A =.. [object,segment_pair,graph(S1AS,_),_],
%    S2A =.. [object,segment_pair,graph(S2AS,_),_],
%    S1B =.. [object,segment_pair,graph(S1BS,_),_],
%    S2B =.. [object,segment_pair,graph(S2BS,_),_],
%    conc(S1AS,S2AS,SA), % get all face segments
%    conc(S1BS,S2BS,SB),
%    permutation(SA,PA),
%    permutation(SB,PB),
%    list_call(similar_size,PA,PB),!.

%similar_size(Object1, Object2) :-
%    Object1 =.. [object,face_pair,graph([F1A,F2A],_),_],
%    Object2 =.. [object,face_pair,graph([F1B,F2B],_),_],
%    similar_size(F1A,F2A).
%    similar_size(F1A,F1B),
%    similar_size(F2A,F2B).

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

equal_pos(Object1,Object2) :-
    Object1 =.. [object,Name,graph(_,_),[XA,YA,ZA]],
    Object2 =.. [object,Name,graph(_,_),[XB,YB,ZB]],
    Eps is 0.001,
    sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2) =< Eps.
 
% ADDITIONAL PROCEDURES

graph(V,E,Graph) :-
%    object_list(V),
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

member_list([],_).

member_list([X|R],L) :-
    member(X,L),
    member_list(R,L).

arc_list([],[]).

arc_list([CVArc|R],[Arc|R0]) :-
    CVArc=..[cv_arc,_,_,_,Arc],
    call(CVArc),
%    Arc = arc(Object1,Object2,RelationName),
    arc_list(R,R0).

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
%    corner(Object1,C1),
%    corner(Object2,C2),
%    gt(C1,C2).

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
