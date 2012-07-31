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
