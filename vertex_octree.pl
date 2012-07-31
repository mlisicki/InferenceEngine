%% OCTREE
%  by Michal Lisicki (2012)

% build bounding box
bb_max(vertex(X,Y,Z)) :-
    vertex(X,_,_),not((vertex(X1,_,_),X1>X)),
    vertex(_,Y,_),not((vertex(_,Y1,_),Y1>Y)),
    vertex(_,_,Z),not((vertex(_,_,Z1),Z1>Z)),!.

bb_min(vertex(X,Y,Z)) :-
    vertex(X,_,_),not((vertex(X1,_,_),X1<X)),
    vertex(_,Y,_),not((vertex(_,Y1,_),Y1<Y)),
    vertex(_,_,Z),not((vertex(_,_,Z1),Z1<Z)),!.

% bb_min(V1),bb_max(V2),findall(vertex(X,Y,Z),vertex(X,Y,Z),Vs),add_list(ot(V1,V2,[],[]),Vs,ot(_,_,Nodes,_),3),writelist(Nodes).

add_list(Tree,[],Tree,_).

add_list(Tree,[X|R],NewTree,LeafCapacity) :-
    add(Tree,X,NewTree1,LeafCapacity),
    add_list(NewTree1,R,NewTree,LeafCapacity).

add(ot(BB_min,BB_max,Nodes,Elements),X,NewTree,LeafCapacity) :-
    geq(X,BB_min),
    geq(BB_max,X),
    add_node(ot(BB_min,BB_max,Nodes,Elements),X,NewTree,LeafCapacity).

% add to leaf
add_node(ot(BB_min,BB_max,[],Elements),X,
    ot(BB_min,BB_max,[],[X|Elements]),LeafCapacity) :-
    length([X|Elements],N),
    N =< LeafCapacity,!.

% extend if leaf capacity reached
add_node(ot(BB_min,BB_max,[],Elements),X,
         ot(BB_min,BB_max,NewNodes,[]),LeafCapacity) :-
    length([X|Elements],N),
    N > LeafCapacity,
    BB_min = vertex(XA,YA,ZA),
    BB_max = vertex(XB,YB,ZB),
    MNX is 0.5*(XA+XB), MNY is 0.5*(YA+YB), MNZ is 0.5*(ZA+ZB), 
    MiddleNode = vertex(MNX,MNY,MNZ),
    Nodes = [ot(MiddleNode,BB_max,[],[]),
             ot(vertex(XA,MNY,ZA),vertex(MNX,YB,MNZ),[],[]),
	     ot(vertex(XA,MNY,MNZ),vertex(MNX,YB,ZB),[],[]),
             ot(vertex(MNX,MNY,ZA),vertex(XB,YB,MNZ),[],[]),
             ot(BB_min,MiddleNode,[],[]),
             ot(vertex(MNX,YA,ZA),vertex(XB,MNY,MNZ),[],[]),
	     ot(vertex(XA,YA,MNZ),vertex(MNX,MNY,ZB),[],[]),
             ot(vertex(MNX,YA,MNZ),vertex(XB,MNY,ZB),[],[])],
    allocate([X|Elements],Nodes,NewNodes,LeafCapacity).

% recursively add to some of the nodes
add_node(ot(BB_min,BB_max,Nodes,[]),X,
         ot(BB_min,BB_max,NewNodes,[]),LeafCapacity) :-
    allocate([X],Nodes,NewNodes,LeafCapacity).

allocate([],Nodes,Nodes,_).

allocate([X|R],Nodes,NewNodes,LeafCapacity) :-
    add_to_nodes(X,Nodes,NewNodes1,LeafCapacity),
    allocate(R,NewNodes1,NewNodes,LeafCapacity).

% adds element to each node which fulfils requirements
add_to_nodes(_,[],[],_).

add_to_nodes(X,[Node|RNodes],[NewNode|RNNodes],LeafCapacity) :-
%    Node = ot(_,BB_max,_,_),
    add(Node,X,NewNode,LeafCapacity),!,
%    (gt(BB_max,X);	                   % prevent further search if addition succeeded
    add_to_nodes(X,RNodes,RNNodes,LeafCapacity).

add_to_nodes(X,[Node|RNodes],[Node|RNNodes],LeafCapacity) :-
    add_to_nodes(X,RNodes,RNNodes,LeafCapacity).

list_call([X|R]) :-
    call(X),
    list_call(R).

geq(vertex(XA,YA,ZA),vertex(XB,YB,ZB)) :-
     XA >= XB, YA >= YB, ZA >= ZB.

gt(vertex(XA,YA,ZA),vertex(XB,YB,ZB)) :-
     XA > XB, YA > YB, ZA > ZB.

writelist([]).

writelist([X|R]) :-
     write(X),nl,
     writelist(R).

% Display the tree
show(Tree) :-
    show(Tree,0).

show(ot(BB_min,BB_max,[],Elements),Indent) :-
    tab(Indent), write(n(BB_min,BB_max,Elements)),nl.

show(ot(BB_min,BB_max,[N1,N2,N3,N4,N5,N6,N7,N8],Elements),Indent) :-
    Ind2 is Indent+40,
    show_list([N1,N2,N3,N4],Ind2),
    tab(Indent), write(n(BB_min,BB_max,Elements)),nl,
    show_list([N5,N6,N7,N8],Ind2).

show_list([],_).

show_list([X|L],Indent) :-
    show(X,Indent),
    show_list(L,Indent).
