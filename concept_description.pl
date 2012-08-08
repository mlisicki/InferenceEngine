%% INFERENCE ENGINE
%% by Michal Lisicki (2012)
%% CONCEPT DESCRIPTION

:- dynamic object/3.

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
    octree(segment,OT),
    Object1 =.. [object,segment,_,_],
    call(Object1),
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
    octree(segment_pair,OT),
    Object1 =.. [object,segment_pair,_,_],
    call(Object1),     
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
    octree(face,OT),
    Object1 =.. [object,face,_,_],
    call(Object1),
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
%    arg(3,Object1,Position).

object(cuboid, Graph, Position) :-
    octree(face_pair,OT),
    Object1 =.. [object,face_pair,_,_],
    call(Object1),
    find(OT,Object1,Nodes),
    extract_objects(face_pair,Nodes,FPObjects),
%    extract_objects(face,Nodes,FObjects),
    member(Object2,FPObjects),
%    member(Object3,FObjects),
%    member(Object4,FObjects),
    equal_pos(Object2,Object1),
    gt(Object2,Object1),
%    gt(Object4,Object3),
     graph([Object1,Object2],
%    graph([Object1,Object2,Object3,Object4],
          [cv_arc(Object1,Object2,connected,_),
           cv_arc(Object1,Object2,opposite_parallel,_)],
%           cv_arc(Object3,Object4,parallel,_),
%           cv_arc(Object1,Object3,connected,_),
%           cv_arc(Object1,Object4,connected,_),
%           cv_arc(Object2,Object3,connected,_),
%           cv_arc(Object2,Object4,connected,_)],
           Graph).
%    arg(3,Object1,Position).

%object(face, Graph, Position) :-
%    findall(object(segment_pair,Graph,Position),object(segment_pair,Graph,Position),Objects),
%    quicksort(Objects,SortedObjects),
%    OL = [Object1,Object2,Object3,Object4],
%    combination(SortedObjects,OL),
%    once((permutation(OL,[PObject1,PObject2,PObject3,PObject4]),
%          graph(OL,
%                [cv_arc(PObject1,PObject3,parallel,_),
%                 cv_arc(PObject2,PObject4,parallel,_),
%                 cv_arc(PObject1,PObject2,connected,_),
%                 cv_arc(PObject2,PObject3,connected,_),
%                 cv_arc(PObject3,PObject4,connected,_),   % we don't need perpendicular realtions here as
%                 cv_arc(PObject4,PObject1,connected,_)],  % segments are filtered for axes parallel
%                Graph))),
%    arg(3,Object1,[XA,YA,ZA]),
%    arg(3,Object3,[XB,YB,ZB]),
%    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
%    Position = [A,B,C].
