%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2024 08:51
%%%-------------------------------------------------------------------
-module(event).
-author("cj anderson").
-include("event.hrl").

%% API
-export([loop/1, start/2, start_link/2, cancel/1, init/3]).

-spec loop(State :: #state{}) -> any().

loop(State = #state{server = Server, to_go = [T | Next]}) ->
  receive {Server, Ref, cancel} -> Server ! {Ref, ok}
  after T * 1000 ->
    if Next =:= [] -> Server ! {done, State#state.name};
      Next =/= [] -> loop(State#state{to_go = Next})
    end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, DateTime) ->
  loop(#state{
    server = Server,
    name = EventName,
    to_go = time_to_go(DateTime)
  }).

cancel(EventPid) ->
  EventRef = erlang:monitor(process, EventPid),
  EventPid ! {self(), EventRef, cancel},
  receive
    {EventRef, ok} ->
      erlang:demonitor(EventRef, [flush]),
      ok;
    {'DOWN', EventRef, process, EventPid, _Reason} ->
      ok
  end.

time_to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
    calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0 -> ToGo;
           ToGo =< 0 -> 0
         end,
  normalize(Secs).