-module(functions).
-compile(export_all). %% replace with -export() later

head([H|_]) -> H.

second([_,X|_]) -> X.

same(X,X) ->
  true;
same(_,_) ->
  false.

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The data tupe (~p) says: ~p/~p/~p,~n", [Date,Y,M,D]),
  io:format("The time tuple(~p) says: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
  io:format("Bad data").
