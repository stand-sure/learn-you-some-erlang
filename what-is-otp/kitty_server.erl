%%%%% Naive version
-module(kitty_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color = green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(LoopPid, Name, Color, Description) ->
  MonitorRef = erlang:monitor(process, LoopPid),
  LoopPid ! {self(), MonitorRef, {order, Name, Color, Description}},
  receive
    {MonitorRef, Cat} ->
      erlang:demonitor(MonitorRef, [flush]),
      Cat;

    {'DOWN', MonitorRef, process, LoopPid, Reason} ->
      erlang:error(Reason)

  after 5000 ->
    erlang:error(timeout)
  end.

%% This call is asynchronous
return_cat(LoopPid, Cat = #cat{}) ->
  LoopPid ! {return, Cat},
  ok.

%% Synchronous call
close_shop(LoopPid) ->
  MonitorRef = erlang:monitor(process, LoopPid),
  LoopPid ! {self(), MonitorRef, terminate},
  receive
    {MonitorRef, ok} ->
      erlang:demonitor(MonitorRef, [flush]),
      ok;
    
    {'DOWN', MonitorRef, process, LoopPid, Reason} ->
      erlang:error(Reason)
  
  after 5000 ->
    erlang:error(timeout)
  end.

%%% Server functions
init() -> loop([]).

loop(Cats) ->
  receive
    {LoopPid, MonitorRef, {order, Name, Color, Description}} ->
      if Cats =:= [] ->
        LoopPid ! {MonitorRef, make_cat(Name, Color, Description)},
        loop(Cats);

        Cats =/= [] -> % got to empty the stock
          LoopPid ! {MonitorRef, hd(Cats)},
          loop(tl(Cats))

      end;

    {return, Cat = #cat{}} ->
      loop([Cat | Cats]);

    {LoopPid, MonitorRef, terminate} ->
      LoopPid ! {MonitorRef, ok},
      terminate(Cats);

    Unknown ->
      %% do some logging here too
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(Cats)

  end.

%%% Private functions
make_cat(Name, Color, Description) ->
  #cat{name = Name, color = Color, description = Description}.

terminate(Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  ok.