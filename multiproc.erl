%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 15:08
%%%-------------------------------------------------------------------
-module(multiproc).
-author("cjanderson").

%% API
-export([sleep/1, flush/0, important/0, optimized/1]).

-spec sleep(T :: integer()) -> any().
sleep(T) ->
  receive
  after T -> ok
  end.

-spec flush() -> any().
flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.

-spec important() -> any().
important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
    normal()
  end.

-spec normal() -> any().
normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
  after 0 ->
    []
  end.

%% optimized in R14A
optimized(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, hello},
  receive
    {Pid, Ref, Msg} ->
      io:format("~p~n", [Msg])
  end.