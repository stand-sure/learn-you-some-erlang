%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2024 14:41
%%%-------------------------------------------------------------------
-module(cat_fsm).
-author("cj anderson").

%% API
-export([start/0, event/2]).

start() ->
  spawn(fun() -> dont_give_a_crap() end).

event(CallerPid, Event) ->
  Ref = make_ref(),
  CallerPid ! {self(), Ref, Event},

  receive
    {Ref, Message} -> {ok, Message}
  after 5000 ->
    {error, timeout}
  end.

dont_give_a_crap() ->
  receive
    {CallerPid, Ref, _Msg} -> CallerPid ! {Ref, meh};
    _ -> ok
  end,
  io:format("Switching to 'dont_give_a_crap' state~n"),
  dont_give_a_crap().