
%flight knowladge

flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).

flight(izmir,istanbul).
flight(antalya,istanbul).
flight(gaziantep,istanbul).
flight(ankara,istanbul).
flight(van,istanbul).
flight(rize,istanbul).

flight(rize,van).
flight(van,rize).
flight(van,ankara).
flight(ankara,van).
flight(ankara,konya).
flight(konya,ankara).
flight(konya,antalya).
flight(antalya,konya).

flight(edirne,edremit).
flight(edremit,edirne).
flight(edremit,erzincan).
flight(erzincan,edremit).

flight(izmir,ısparta).
flight(ısparta,izmir).
flight(ısparta,burdur).
flight(burdur,ısparta).

flight(antalya,gaziantep).
flight(gaziantep,antalya).

%distance knowladge

distance(istanbul,izmir,328).
distance(istanbul,antalya,482).
distance(istanbul,gaziantep,847).
distance(istanbul,ankara,351).
distance(istanbul,van,1262).
distance(istanbul,rize,967).

distance(izmir,istanbul,328).
distance(antalya,istanbul,482).
distance(gaziantep,istanbul,847).
distance(ankara,istanbul,351).
distance(van,istanbul,1262).
distance(rize,istanbul,967).

distance(rize,van,373).
distance(van,rize,373).
distance(van,ankara,920).
distance(ankara,van,920).
distance(ankara,konya,227).
distance(konya,ankara,227).
distance(konya,antalya,192).
distance(antalya,konya,192).

distance(edirne,edremit,914).
distance(edremit,edirne,914).
distance(edremit,erzincan,736).
distance(erzincan,edremit,736).

distance(izmir,ısparta,308).
distance(ısparta,izmir,308).
distance(ısparta,burdur,24).
distance(burdur,ısparta,24).

distance(antalya,gaziantep,592).
distance(gaziantep,antalya,592).

% Rules

%helper functions check item in list or not work like prolog member function
check_in_list(X,[X|_]). %base case
check_in_list(X,[_|Tail]) :- check_in_list(X,Tail). %recursive case


route(X,Y) :- route_list(X,Y,[]).
route_list(X,Y,RouteList) :- flight(X,Z),not(check_in_list(Z,RouteList)),Y=Z. %base case 
route_list(X,Y,RouteList) :- flight(X,Z),not(check_in_list(Z,RouteList)),route_list(Z,Y,[X|RouteList]). %recursive case


sroute(A,B,Len) :- setof(Lengths,helper(A,B,Lengths),AllPaths), % AllPaths hold all possible paths but sorted because of setof rule
   first_elem(AllPaths,Len).

first_elem([Head|_],Len) :- Len = Head. %just return first element this is smallest path

helper(A,B,Len) :-
       traverse_cities(A,B,[A],Len). % find all possible ways

traverse_cities(A,B,_,Distance) :- distance(A,B,Distance). %base case
traverse_cities(A,B,RouteList,Distance) :- distance(A,C,D),           
       not(C == B),
       not(check_in_list(C,RouteList)),
       traverse_cities(C,B,[C|RouteList],PrevDistance), % recursive case
       Distance is PrevDistance+D.  