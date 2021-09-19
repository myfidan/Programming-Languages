
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

%helper functions check item in list or not work like prolog member function
check_in_list(X,[X|_]). %base case
check_in_list(X,[_|Tail]) :- check_in_list(X,Tail). %recursive case


route(X,Y) :- route_list(X,Y,[]).
route_list(X,Y,RouteList) :- flight(X,Z),not(check_in_list(Z,RouteList)),Y=Z. %base case 
route_list(X,Y,RouteList) :- flight(X,Z),not(check_in_list(Z,RouteList)),route_list(Z,Y,[X|RouteList]). %recursive case