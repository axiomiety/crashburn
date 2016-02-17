-module(fact).
-export([factorial/1,factorial_tr/1]).

factorial_tr(N) ->
  factorial_tr(1, N, 1).

factorial_tr(Current, N, Result) when Current =< N ->
  NewResult = Result*Current,
  io:format("~w yields ~w!~n", [Current, NewResult]),
  factorial_tr(Current+1, N, NewResult);

factorial_tr(_, _, Result) ->
  io:format("Finished.~n"),
  Result.

factorial(N) when N > 1 ->
  io:format("Calling from ~w.~n", [N]),
  Result = N * factorial(N-1),
  io:format("~w yields ~w.~n", [N, Result]),
  Result;

factorial(N) when N =< 1 ->
  io:format("Calling from ~w.~n", [N]),
  io:format("~w yields ~w.~n", [N, N]),
  1.
