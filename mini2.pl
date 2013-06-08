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

% Support for updating values (first step to supporting assign)

% Empty envt
update([],value(I,V),[value(I,V)]).
% Handle case of changing binding
update([value(I,_)|Envs],value(I,V),[value(I,V)|Envs]).
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
% Support for if
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =:= 0, !, R is V2.
reduce(config(if(V1,V2,V3),Env),config(R,Env)) :- integer(V1), integer(V2), integer(V3), V1 =\= 0, !, R is V3.


% Handle assign with no pre-existing binding in envt (not using update approach)
reduce(config(assign(I,V),[]),[value(I,V)]) :- update([],value(I,V),[value(I,V)]).
% Test case: reduce(config(assign(x,3),[]),[value(x,3)]).

% Handle assign with pre-existing binding
reduce(config(assign(I,T),[value(I,V)]),[value(I,T)]) :- update([value(I,V)],value(I,T),[value(I,T)]).
% Test case: reduce(config(assign(x,3),[value(x,2)]),[value(x,3)]).

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
