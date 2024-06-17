%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 09:57
%%%-------------------------------------------------------------------
-module(road).
-author("cjanderson").

%% API
-export([main/1]).

%% main is entry point.

-spec main(FileName) -> list()
  when FileName :: string().

main([FileName]) ->
  {ok, Binary} = file:read_file(FileName),
  Map = parse_map(Binary),
  io:format("~p,~n", [optimal_path(Map)]),
  erlang:halt().

-spec parse_map(Distances) -> list()
  when Distances :: binary() | Distances :: string().

parse_map(Binary) when is_binary(Binary) ->
  parse_map(binary_to_list(Binary));
parse_map(List) when is_list(List) ->
  Values = [list_to_integer(X) || X <- string:tokens(List, "\r\n\t ")],
  group_values(Values, []).

%% group_values constructs {A,B,X} tuples

-spec group_values(List, Acc) -> list()
  when List :: list(), Acc :: list().

group_values([], Acc) -> lists:reverse(Acc);
group_values([A, B, X | Rest], Acc) -> group_values(Rest, [{A, B, X} | Acc]).

%% shortest_step finds the shortest path to a point

shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
  OptA1 = {DistA + A, [{a, A} | PathA]},
  OptA2 = {DistB + B + X, [{x, X}, {b, B} | PathB]},
  OptB1 = {DistB + B, [{b, B} | PathB]},
  OptB2 = {DistA + A + X, [{x, X}, {a, A} | PathA]},
  {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
  {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
  {_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
                    hd(element(2, B)) =/= {x, 0} -> B
                  end,
  lists:reverse(Path).
