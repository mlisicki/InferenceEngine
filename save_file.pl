sum(A,B,C) :-
    write_triple('in.txt', A, B, C),
    C is A + B,
    write_triple('out.txt', A, B, C).

write_triple(File, A, B, C) :-
    open(File, append, Stream),
    write(Stream, (A,B,C)),
    nl(Stream),
    close(Stream).
