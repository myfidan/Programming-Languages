
check_in_list(X,[X|_]). %base case
check_in_list(X,[_|Tail]) :- check_in_list(X,Tail). %recursive case

element(E,S) :- check_in_list(E,S).

union(S1,S2,S3) :- append(S1,S2,X),sort(X,Y),sort(S3,S4),check_all(Y,S4).

append([],X,X).
append([S|Tail1],List2,[S|List3]) :- append(Tail1,List2,List3).

check_all([],[]).
check_all([S5|Tail],[S6|Tail2]) :- S5=:=S6,check_all(Tail,Tail2).

intersect(S1,S2) :- check_intersects(S1,S2,X),write(X).

check_intersects([],_,[]).
check_intersects([S1|Tail1],S2,[S1|X]) :- check_in_list(S1,S2),check_intersects(Tail1,S2,X).
check_intersects([S1|Tail1],S2,X) :- not(check_in_list(S1,S2)),check_intersects(Tail1,S2,X).

equivalent(S1,S2) :- check_equal(S1,S2),check_equal(S2,S1).

check_equal([],_).
check_equal([S1|Tail],S2) :- check_in_list(S1,S2),check_equal(Tail,S2).
