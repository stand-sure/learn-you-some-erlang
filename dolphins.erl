%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 13:15
%%%-------------------------------------------------------------------
-module(dolphins).
-author("cj anderson").

%% API
-export([dolphin1/0, dolphin2/0, dolphin3/0]).

-spec dolphin1() -> 'ok'.

dolphin1() ->
  receive
    do_a_flip -> io:format("How about no?~n");
    fish -> io:format("Thanks for the fish!~n");
    _ -> io:format("We're smarter than humans!~n")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} -> From ! "How about no?";
    {From, fish} -> From ! "So long and thanks for all the fish!";
    _ -> io:format("Heh, we're smarter than you humans.~n")
  end.

-spec dolphin3() -> any().
dolphin3() ->
  receive
    {From, do_a_flip} -> From ! "How about no?",
      dolphin3();
    {From, fish} -> From ! "So long and thanks for all the fish!";
    _ -> io:format("Heh, we're smarter than you humans.~n"),
      dolphin3()
  end.