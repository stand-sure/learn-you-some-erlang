%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 14:35
%%%-------------------------------------------------------------------
-module(kitchen).
-author("cjanderson").

%% API
-export([fridge1/0, fridge2/1, store/2, take/2, start/1, store2/2, take2/2]).

fridge1() ->
  receive
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge1();
    {From, {take, _Food}} ->
%% uh....
      From ! {self(), not_found},
      fridge1();
    terminate ->
      ok
  end.

fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food | FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate ->
      ok
  end.

-spec store(Pid, Food) -> term()
  when Pid :: pid(),
  Food :: atom().

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

-spec take(Pid, Food) -> term()
  when Pid :: pid(),
  Food :: list().

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

-spec start(FoodList) -> pid()
  when FoodList :: list().

start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

store2(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Message} -> Message
  after 3000 ->
    timeout
  end.

-spec take2(Pid :: pid(), Food :: atom()) -> term().
take2(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Message} -> Message
  after 3000 ->
    timeout
  end.

