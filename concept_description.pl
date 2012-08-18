%% INFERENCE ENGINE
%% by Michal Lisicki (2012)
%% CONCEPT DESCRIPTION

:- dynamic object/3.

% Concepts built with part links

object(vertex,Graph,Position) :-
    graph([],[],Graph),
    vertex(X,Y,Z),
    Position = [X,Y,Z].

object(segment, Graph, Position) :-
    findall(object(vertex,Graph,Position),object(vertex,Graph,Position),Objects),
    quicksort(Objects,SortedObjects),
    OL = [Object1,Object2],
    combination(SortedObjects,OL),
    graph(OL,
          [cv_arc(Object1,Object2,axis_parallel,_)],
          Graph),
    arg(3,Object1,[XA,YA,ZA]),
    arg(3,Object2,[XB,YB,ZB]),
    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
    Position = [A,B,C].

object(segment_pair, Graph, Position) :-
    octree(segment,OT,NE),
    object(segment_pair,Graph,Position,OT,NE).

object(segment_pair, Graph, Position, OT, NewElements) :-
    member(Object1,NewElements),
    find(OT,Object1,Nodes),
    extract_objects(segment,Nodes,Objects),
    member(Object2,Objects),gt(Object2,Object1),
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,connected,_)],
          Graph),
    Object1 =.. [object,segment,graph([V1A,V2A],_),_], % calculate position
    Object2 =.. [object,segment,graph([V1B,V2B],_),_],
    permutation([V1A,V2A],[PV1A,PV2A]),
    permutation([V1B,V2B],[PV1B,PV2B]),
    equal(PV1A,PV1B),
    arg(3,PV2A,[XA,YA,ZA]),
    arg(3,PV2B,[XB,YB,ZB]),
    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
    Position = [A,B,C].

object(face, Graph, Position) :-
    octree(segment_pair,OT,NE),
    object(face,Graph,Position,OT,NE).

object(face, Graph, Position, OT, NewElements) :-
    member(Object1,NewElements),
    find(OT,Object1,Nodes),
    extract_objects(segment_pair,Nodes,Objects),
    member(Object2,Objects),
    equal_pos(Object2,Object1),
    gt(Object2,Object1),
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,connected,_),
           cv_arc(Object1,Object2,opposite_parallel,_)],
           Graph),
    arg(3,Object1,Position).

object(face_pair, Graph, Position) :-
    octree(face,OT,NE),
    object(face_pair,Graph,Position,OT,NE).

object(face_pair, Graph, Position, OT, NewElements) :-
    member(Object1,NewElements),
    find(OT,Object1,Nodes),
    extract_objects(face,Nodes,Objects),
    member(Object2,Objects),gt(Object2,Object1),
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,connected,_)],
          Graph),
    list_fp_vertices(object(face_pair,Graph,_),Vs),
    findall([D,Vs1],(Vs1 = [[XA,YA,ZA],[XB,YB,ZB]],combination(Vs,Vs1),D is sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2)),Distances),
    max_dist_list(Distances,Max),
    Max = [_,[[XC,YC,ZC],[XD,YD,ZD]]],
    A is (XC+XD)/2, B is (YC+YD)/2, C is (ZC+ZD)/2,
    Position = [A,B,C].

object(cuboid, Graph, Position) :-
    octree(face_pair,OT,NE),
    object(cuboid,Graph,Position,OT,NE).

object(cuboid, Graph, Position, OT, NewElements) :-
    member(Object1,NewElements),
    list_fp_vertices(Object1,[Object1A|_]), % take out one of the vertices that must stay connected to other object
    find(OT,object(vertex,graph([],[]),Object1A),Nodes),
    extract_objects(face_pair,Nodes,FPObjects),
    member(Object2,FPObjects),
    equal_pos(Object2,Object1),
    gt(Object2,Object1),
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,connected,_),
           cv_arc(Object1,Object2,opposite_parallel,_)],
           Graph),
    arg(3,Object1,Position).

% Concepts built with specialization links (leg, sit, back) (?)
%object(square_face_pair, Graph, Position) :-
%    octree(face,OT,NE),
%    object(square_face_pair,Graph,Position,OT,NE).
%
%object(square_face_pair, Graph, Position, OT, NewElements) :-
%    member(Object1,NewElements),
%    find(OT,Object1,Nodes),
%    extract_objects(face,Nodes,Objects),
%    member(Object2,Objects),gt(Object2,Object1),
%    graph([Object1,Object2],
%          [cv_arc(Object1,Object2,similar_size,_),
%           cv_arc(Object1,Object2,connected,_)],
%          Graph),
%    list_fp_vertices(object(square_face_pair,Graph,_),Vs),
%    findall([D,Vs1],(Vs1 = [[XA,YA,ZA],[XB,YB,ZB]],combination(Vs,Vs1),D is sqrt((XA-XB)^2+(YA-YB)^2+(ZA-ZB)^2)),Distances),
%    max_dist_list(Distances,Max),
%    Max = [_,[[XC,YC,ZC],[XD,YD,ZD]]],
%    A is (XC+XD)/2, B is (YC+YD)/2, C is (ZC+ZD)/2,
%    Position = [A,B,C].

object(square_face_pair, Graph, Position) :-
    octree(face_pair,_,NE),
    object(square_face_pair,Graph,Position,_,NE). 

object(square_face_pair, Graph, Position, _, NewElements) :-
    member(Object,NewElements),
    Object =.. [object,face_pair,graph([Object1,Object2],CL1),Position],
    graph([Object1,Object2],
          [cv_arc(Object1,Object2,similar_size,_)],
          Graph1),
    conc(CL1,[arc(Object1,Object2,similar_size)],CL),
    Graph = graph([Object1,Object2],CL).

%object(square_cuboid, Graph, Position) :-
%    octree(square_face_pair,OT,NE),
%    object(square_cuboid,Graph,Position,OT,NE).
%
%object(square_cuboid, Graph, Position, OT, NewElements) :-
%    member(Object1,NewElements),
%    find(OT,Object1,Nodes),
%    extract_objects(square_face_pair,Nodes,FPObjects),
%    member(Object2,FPObjects),
%    equal_pos(Object2,Object1),
%    gt(Object2,Object1),
%    graph([Object1,Object2],
%          [cv_arc(Object1,Object2,connected,_),
%           cv_arc(Object1,Object2,opposite_parallel,_)],
%           Graph),
%    arg(3,Object1,Position).

object(square_cuboid, Graph, Position) :-
    octree(cuboid,_,NE),
    object(square_cuboid,Graph,Position,_,NE). 

object(square_cuboid, Graph, Position, _, NewElements) :-
    member(Object,NewElements),
    Object =.. [object,cuboid,Graph,Position],
    Graph =.. [graph,[Object1,Object2],_],
    object(square_face_pair,_,_,_,[Object1]),
    object(square_face_pair,_,_,_,[Object2]).

object(back, Graph, Position) :-
    octree(cuboid,_,NE),
    object(back,Graph,Position,_,NE). 

object(back, Graph, Position, _, NewElements) :-
    Object =.. [object,square_cuboid,Graph,Position,_,NewElements],
    call(Object),
    dim(object(square_cuboid,Graph,Position),H,W),
    W > H.

object(leg, Graph, Position) :-
    octree(cuboid,_,NE),
    object(leg,Graph,Position,_,NE). 

object(leg, Graph, Position, _, NewElements) :-
    Object =.. [object,square_cuboid,Graph,Position,_,NewElements],
    call(Object),
    dim(object(square_cuboid,Graph,Position),H,W),
    H > W.

object(seat, Graph, Position) :-
    octree(cuboid,_,NE),
    object(seat,Graph,Position,_,NE). 

object(seat, Graph, Position, _, NewElements) :-
    Object =.. [object,square_cuboid,Graph,Position,_,NewElements],
    call(Object),
    dim(object(square_cuboid,Graph,Position),H,W),
    W > H.

object(chair, Graph, Position) :-
    octree(cuboid,OT,NE),
    object(chair,Graph,Position,OT,NE).

object(chair, Graph, Position, OT, NewElements) :-
    findall(object(cuboid,Graph,Position),object(leg,Graph,Position,_,NewElements),LObjects),
    quicksort(LObjects,SortedLObjects),
    OL = [Object1,Object2,Object3,Object4],
    combination(SortedLObjects,OL),
    find(OT,Object1,Nodes),
    extract_objects(cuboid,Nodes,Objects),
    object(seat,G5,P5,OT,Objects),
    Object5 =.. [object,cuboid,G5,P5],
    object(back,G6,P6,OT,NewElements),
    Object6 =.. [object,cuboid,G6,P6],
    graph([Object1,Object2,Object3,Object4,Object5,Object6],
           [cv_arc(Object1,Object5,not_colliding,_),
            cv_arc(Object2,Object5,not_colliding,_),
            cv_arc(Object3,Object5,not_colliding,_),
            cv_arc(Object4,Object5,not_colliding,_),
            cv_arc(Object6,Object1,not_colliding,_),
            cv_arc(Object6,Object2,not_colliding,_),
            cv_arc(Object6,Object3,not_colliding,_),
            cv_arc(Object6,Object4,not_colliding,_),
            cv_arc(Object5,Object6,connected,_),
            cv_arc(Object5,Object6,not_colliding,_)],
           Graph),
    arg(3,Object5,Position).

