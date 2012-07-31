%% INFERENCE ENGINE
%% by Michal Lisicki (2012)
%% CONCEPT DESCRIPTION

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

object(face, Graph, Position) :-
    findall(object(segment,Graph,Position),object(segment,Graph,Position),Objects),
    quicksort(Objects,SortedObjects),
    OL = [Object1,Object2,Object3,Object4],
    combination(SortedObjects,OL),
    once((permutation(OL,[PObject1,PObject2,PObject3,PObject4]),
          graph(OL,
                [cv_arc(PObject1,PObject3,parallel,_),
                 cv_arc(PObject2,PObject4,parallel,_),
                 cv_arc(PObject1,PObject2,connected,_),
                 cv_arc(PObject2,PObject3,connected,_),
                 cv_arc(PObject3,PObject4,connected,_),   % we don't need perpendicular realtions here as
                 cv_arc(PObject4,PObject1,connected,_)],  % segments are filtered for axes parallel
                Graph))),
    arg(3,Object1,[XA,YA,ZA]),
    arg(3,Object3,[XB,YB,ZB]),
    A is (XA+XB)/2, B is (YA+YB)/2, C is (ZA+ZB)/2,
    Position = [A,B,C].
