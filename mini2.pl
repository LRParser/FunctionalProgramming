% -*-Prolog-*-
% Part II of assignment 4
% reduction rules for arithmetic expressions.
% Author: Jeremy Johnson

% test cases.
%
% reduce_all(config(times(plus(x,3),minus(5,y)),[value(x,2),value(y,1)]),V).
%    V = config(20,[value(x,2),value(y,1)]) ?
%

% Helpers
% not(P) :- (call(P) -> fail ; true).
car([H|T],H).
cdr([H|T],T).

% Handle replace
% Base case: No matter what is replaced, given an empty input list, output is an empty list
% Adaptation of code from user svick at: http://stackoverflow.com/questions/5850937/prolog-element-in-lists-replacement
replace(_,_,[],[]).
replace(F,R,[F|T],[R|T2]) :- replace(F,R,T,T2).
replace(F,R,[H|T],[H|T2]) :-  H\= F, replace(O,R,T,T2).

lookup([value(I,V)|_],I,V).
lookup([_|Es],I,V) :- lookup(Es,I,V), !.

% Empty envt
update([],value(I,V),[value(I,V)]).
update([value(I,_)|Envs],value(I,V),[value(I,V)|Envs]).
update(Env,value(I,V),R) :- member(value(I,_),Env) -> replace(value(I,_),value(I,V),Env,R); R = [value(I,V),Env].
%(isZero(C) -> S = T; S = F). 

% Documenting previously-failed approaches for updating when variable to update isn't at front of Envt
% Failed: update([_|value(I,_)],value(I,V)) :- update(Es,value(I,V)).
% Failed: update([Head|value(I,_)],value(I,V),[Head,value(I,V)]).
% Failed: update([_|value(I,_)],value(I,V),[_|value(I,V)]).
% Failed: update([Head|value(I,_)],value(I,V),[Head,value(I,V)]).
% Failed: update([Head|Tail],value(I,V),lookup(Env1,I,V)) :- update(Tail,value(I,V),lookup(Env1,I,V)).
% Failed (Close?): update([Head,Tail],value(I,V),lookup(Env1,I,V)) :- update([Tail],value(I,V),T), lookup(T,I,V).
% Failed: update([Head,Tail],value(I,V),[Head,value(I,V)]) :- update([Tail],value(I,V),T), lookup(T,I,V).
% Failed: update([value(_)|Es],value(I,V)) :- update(Es,value(I,V)).
% Failed: update([_|value(I,_)],value(I,V),[_|value(I,V)]).
% Failed: update([_|value(I,_)],value(I,V)) :- update(Es,value(I,V)), !.

% Test cases
% update([],value(x,3),[value(x,3)]).
% update([value(x,4)],value(x,3),[value(x,3)]).

reduce(config(plus(E,E2),Env),config(plus(E1,E2),Env)) :-
     reduce(config(E,Env),config(E1,Env)).
reduce(config(minus(E,E2),Env),config(minus(E1,E2),Env)) :-
     reduce(config(E,Env),config(E1,Env)).
reduce(config(times(E,E2),Env),config(times(E1,E2),Env)) :-
     reduce(config(E,Env),config(E1,Env)).

reduce(config(plus(V,E),Env),config(plus(V,E1),Env)) :-
     reduce(config(E,Env),config(E1,Env)).
reduce(config(minus(V,E),Env),config(minus(V,E1),Env)) :-
     reduce(config(E,Env),config(E1,Env)).
reduce(config(times(V,E),Env),config(times(V,E1),Env)) :-
     reduce(config(E,Env),config(E1,Env)).

reduce(config(plus(V1,V2),Env),config(R,Env)) :- integer(V1), integer(V2), !, R is V1+V2.
reduce(config(minus(V1,V2),Env),config(R,Env)) :- integer(V1), integer(V2), !, R is V1-V2.
reduce(config(times(V1,V2),Env),config(R,Env)) :- integer(V1), integer(V2), !, R is V1*V2.
reduce(config(I,Env),config(V,Env)) :- atom(I), lookup(Env,I,V).

% Handle assign with no pre-existing binding in envt
reduce(config(assign(I,V),[]),[value(I,V)]) :- update([],value(I,V),[value(I,V)]).
% Test case: reduce(config(assign(x,3),[]),[value(x,3)]).

% Handle assign with pre-existing binding
% reduce(config(assign(I,T),[value(I,V)]),[value(I,T)]) :- update([value(I,V)],value(I,T),[value(I,T)]).
% Test case: reduce(config(assign(x,3),[value(x,2)]),[value(x,3)]).

% Handle assign with multiple bindings in envt
reduce(config(assign(I,T),Env),lookup(Env1,I,T)) :- update(Env,value(I,T),Env1).
% Support for if when clauses are both assign, conditional is integer, and condition is true
reduce(config(if(V1,assign(A,B),assign(_,_)),Env),lookup(Env,A,B)) :- integer(V1), V1 =\= 0.
% Test case: reduce(config(if(3,assign(x,3),assign(x,4)),[]),Env).
% Returns: [value(x,3)]

% Support for if when clauses are both assign and conditional is integer, and condition is false
reduce(config(if(V1,assign(_,_),assign(C,D)),[]),lookup(Env,C,D)) :- integer(V1), V1 =:= 0.
% Test case: reduce(config(if(0,assign(x,3),assign(x,4)),[]),Env).
% Returns: [value(x,4)]

% Support for if when clause conditional is variable and evaluates to true
reduce(config(if(E,assign(A,B),assign(_,_)),Env),lookup(Env,A,B)) :- lookup(Env,E,C), C =\= 0.
% Test case: reduce(config(if(n,assign(i,0),assign(i,1)),[value(n,3)]),Env).

% Support for if when clause conditional is variable and evaluates to false
reduce(config(if(E,assign(_,_),assign(C,D)),Env),lookup(Env,C,D)) :- lookup(Env,E,0).
% Test case: reduce(config(if(n,assign(i,0),assign(i,1)),[value(n,0)]),Env).

% Support for if with values passed
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =\= 0, !, R is V2.
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =:= 0, !, R is V3.
% Test case for 'true' (non-zero value passed)
% reduce(config(if(1,2,3),[]),config(R,[])).
% Above test case returns 2

reduce(config(while(E,assign(A,B)),Env),Env2) :- lookup(Env,E,C), C =\= 0, reduce(config(assign(A,B),Env),Env2).
reduce(config(seq([stmt1|Rest]),Env),Env2) :- config(seq(_,Rest),Env2).

%reduce(config(while(E,

% Handle sequence
reduce(config(seq(stmtFirst,Rest),EnvtInitial)) :- config(seq(Rest),EnvtNew).

reduce_all(config(V,Env),config(V,Env)) :- integer(V), !.
reduce_all(config(E,Env),config(E2,Env)) :-
     reduce(config(E,Env),config(E1,Env)), reduce_all(config(E1,Env),config(E2,Env)).

reduce_value(config(E,Env),V) :- reduce_all(config(E,Env),config(V,Env)).

% Math test cases

% reduce_value(config(times(plus(x,3),minus(5,y)),[value(x,2),value(y,1)]),V).
% reduce_exp_all(config(plus(times(2,5),minus(2,5)),[]),V).

% If test case
% reduce(config(if(0,1,2),[]),Env). Returns 1
% reduce(config(if(1,1,2),[]),Env). Returns 2

% Overall test cases (from Lecture Slides):

% Test cases:
% reduce_exp_all(config(plus(times(2,5),minus(2,5)),[]),V).
% V = config(7,[])
% reduce_exp_all(config(plus(times(x,5),minus(2,y)),[value(x,2),value(y,5)]),V).
% V = config(7,[value(x,2),value(y,5)])
% reduce_all(config(seq(assign(x,3),assign(y,4)),[]),Env).
% Env = [value(x,3),value(y,4)]
% reduce(config(if(3,assign(x,3),assign(x,4)),[]),Env).
% Env = [value(x,3)]
% reduce(config(if(0,assign(x,3),assign(x,4)),[]),Env).
% Env = [value(x,4)]
% reduce_all(config(if(n,assign(i,0),assign(i,1)),[value(n,3)]),Env).
% Env = [value(n,3),value(i,0)]
