% -*-Prolog-*-
% Reverse a list using the built-in append method
% this does not suffer the recursive looping issue that the schme interpreter
% does
myappend([],Y,Y).
myappend([X|XS],Y,[X|ZS]) :- append(XS,Y,ZS).

myreverse([], []).
myreverse([First|Rest], Result) :- reverse(Rest, RestRev), myappend(RestRev, [First], Result).

%then execute:

%% myreverse([1,2,3], What).

%% What = [3,2,1]

%% yes
%% | ?- myreverse(What, [1,2,3]).

%% What = [3,2,1] ?

%% yes
%% | ?-
