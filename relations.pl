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

connected(Object1,Object2) :-
    Object1 =.. [object,cuboid,_,_],
    Object2 =.. [object,cuboid,_,_],
    show_vertices2(Object1,V1),
    show_vertices2(Object2,V2),
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

not_colliding(Object1,Object2) :-
    not_colliding2(Object1,Object2),
    not_colliding2(Object2,Object1).

not_colliding2(Object1,Object2) :-
    Object1 =.. [object,cuboid,graph([FP1,FP2],_),_],
    Object2 =.. [object,cuboid,graph([FP3,FP4],_),_],
    bb_min(Object1,[XA,YA,ZA]),
    bb_max(Object1,[XB,YB,ZB]),
    bb_min(Object2,O2Min),
    bb_max(Object2,O2Max),
    points_between(O2Min,O2Max,L),
    Eps is 0.01,
    not((member(P,L),
         ot_gt(object(vertex,_,P),object(vertex,_,[XA+Eps,YA+Eps,ZA+Eps])),
         ot_gt(object(vertex,_,[XB-Eps,YB-Eps,ZB-Eps]),object(vertex,_,P)))).

%    FP1 =.. [object,face_pair,graph([F1A,F2A],_),_],
%    FP2 =.. [object,face_pair,graph([F1B,F2B],_),_],
%    FP3 =.. [object,face_pair,graph([F1C,F2C],_),_],
%    FP4 =.. [object,face_pair,graph([F1D,F2D],_),_],
%    show_vertices(Object1,Vs1), 
%    show_vertices(Object2,Vs2), 
%    cmpr(Vs2,[F1A,F2A,F1B,F2B]),
%    cmpr(Vs1,[F1C,F2C,F1D,F2D]).
%    findall(Plane,top_FP_plane([FA1,F2A],Plane),[Plane1,Plane2]),
%    (cmpr_gt(Vs,Plane1);
%     cmpr_lt(Vs,Plane1)),
%    (cmpr_gt(Vs,Plane2);
%     cmpr_lt(Vs,Plane2)).

