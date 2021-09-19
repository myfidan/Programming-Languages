
find_operators(List) :- wrapper(List,LeftSide,RightSide),write_file([LeftSide,RightSide]),fail.
find_operators(_).

write_file(List) :-  open('output.txt',write,Out),
    write(Out,List),
    close(Out).

wrapper(List,LeftSide,RightSide) :- append(L1,L2,List),len(L1,N),len(L2,N2),N>0,N2>0,construct(L1,LeftSide),construct(L2,RightSide),LeftSide =:= RightSide.

len([], Len):- Len is 0.

len([_|Y], Len):-
    len(Y, L),
    Len is L + 1.


construct([X],X).
construct(RL,RLSide) :- append(L2,L3,RL),len(L2,N),len(L3,N2),N>0,N2>0,construct(L2,NewL),construct(L3,NewR),operator(NewL,NewR,RLSide).

%Every possible operator
operator(NewL,NewR,NewL+NewR).
operator(NewL,NewR,NewL-NewR).
operator(NewL,NewR,NewL*NewR).
operator(NewL,NewR,NewL/NewR).