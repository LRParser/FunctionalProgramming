% -*-Prolog-*-
% Part II of assignment 4
% reduction rules for arithmetic expressions.
% Author: Jeremy Johnson

% test cases.
%
% reduce_all(config(times(plus(x,3),minus(5,y)),[value(x,2),value(y,1)]),V).
%    V = config(20,[value(x,2),value(y,1)]) ?
%

lookup([value(I,V)|_],I,V).
lookup([_|Es],I,V) :- lookup(Es,I,V), !.

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

reduce_all(config(V,Env),config(V,Env)) :- integer(V), !.
reduce_all(config(E,Env),config(E2,Env)) :-
     reduce(config(E,Env),config(E1,Env)), reduce_all(config(E1,Env),config(E2,Env)).

reduce_value(config(E,Env),V) :- reduce_all(config(E,Env),config(V,Env)).

% Support for if
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =:= 0, !, R is V2.
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =\= 0, !, R is V3.

% If test case
% reduce(config(if(0,1,2),[]),Env). Returns 1
% reduce(config(if(1,1,2),[]),Env). Returns 2

% Support for updating values (first step to supporting assign)

% Empty envt
update([],value(I,V),[value(I,V)]).
% Handle case of changing binding
update([value(I,_)|Envs],value(I,V),[value(I,V)|Envs]).
% Test cases
% update([],value(x,3),[value(x,3)]).
% update([x,4],value(x,3),[value(x,3)]).

% Changing binding
update([I,X],value(I,V),[value(I,V)]).


% Handle assign
reduce(config(assign(I,V),[])) :- update([],value(I,V),[value(I,V)]).
% Test case: reduce(config(assign(x,6),[]),Env).
% Handle sequence
reduce(config(seq(stmtFirst,Rest),EnvtInitial)) :- config(seq(_,Rest),EnvtNew).
