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

% PROBLEM SEPCIFIC

%show_vertices(Object,L) :-
%    show_vertices(Object,L).
%    writelist(L).

%show_vertices(Object,L) :-
%    Object =.. [object,vertex,_,_],
%    member(Object,L),!.

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


% list face_pair non-repeated vertices (for position calculation)
%list_fp_vertices(Object,L,DL) :-
%    list_fp_vertices(Object,[],L,[],DL).
%
%list_fp_vertices(Object,L,L,DL,[P|DL]) :-
%    Object =.. [object,vertex,_,P],
%    member(P,L),!.
%
%list_fp_vertices(Object,L,[P|L],DL,DL) :-
%    Object =.. [object,vertex,_,P],!.
%
%list_fp_vertices(Object,L,OL,DL,ODL) :-
%    Object =.. [object,_,graph(Vs,_),_],
%    list_fp_vertices_list(Vs,L,L1,DL1),
%    nr_add_list(L,L1,OL),
%    nr_add_list(DL,DL1,ODL).
%
%list_fp_vertices_list([],L,L,[]).
%
%list_fp_vertices_list([V|R],L,OL,DL) :-
%    write([V|R]),nl,
%    list_fp_vertices_list(R,L,L1,DL1),
%    list_fp_vertices(V,L1,OL,DL1,DL).
%%    nr_add_list(L1,L2,L),
%   nr_add_list(DL1,DL2,DL).

list_fp_vertices(Object,L) :-
    (Type = face_pair ; Type = square_face_pair),
    Object =.. [object,Type,graph([Object1,Object2],_),_],
    show_vertices(Object1,L1),
    show_vertices(Object2,L2),
    diff_list(L1,L2,L3,L4),
    conc(L3,L4,L).

max(X,Y,X) :-
    X >= Y.

max(X,Y,Y) :-
    X < Y.

maxlist([X],X).

maxlist([X,Y|Rest],Max) :-
    maxlist([Y|Rest],MaxRest),
    max(X,MaxRest,Max).

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
