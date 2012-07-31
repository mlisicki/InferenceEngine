arc(a,b). 
arc(a,c).
arc(a,d).
arc(b,c). 
arc(b,d). 
arc(c,d).

cv_arc(X,Y,Arc) :-
      ( arc(X,Y),
        Arc = arc(X,Y));
      ( arc(Y,X),
        Arc = arc(Y,X)).

rectangle(rectangle(A,B,C,D)) :-
    cv_arc(A,B,AB),
    cv_arc(A,C,AC),
    compare(<,AB,AC),
    cv_arc(B,D,BD),
    compare(<,AC,BD),
    cv_arc(C,D,CD),
    compare(<,BD,CD).

%compare(<,arc(A,B),arc(C,D)) :-
%    A@<C;
%    A@>C,
%    B@<D.
