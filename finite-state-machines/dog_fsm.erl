%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2024 14:48
%%%-------------------------------------------------------------------
-module(dog_fsm).
-author("cjanderson").

%% API
-export([start/0, squirrel/1, pet/1]).

-spec start() -> pid().
-spec squirrel(Caller :: pid()) -> no_return().
-spec pet(Caller :: pid()) -> no_return().
-spec bark() -> no_return().
-spec wag_tail() -> no_return().
-spec sit() -> no_return().


start() ->
  spawn(fun() -> bark() end).

squirrel(Caller) -> Caller ! squirrel.

pet(Caller) -> Caller ! pet.

bark() ->
  io:format("Dog: Bark! BARK!~n"),
  receive
    pet -> wag_tail();

    _ ->
      io:format("Dog: confused~n"),
      bark()

  after 2000 ->
    bark()
  end.

wag_tail() ->
  io:format("Dog: wags tail~n"),
  receive
    pet -> sit();

    _ ->
      io:format("Dog: confused~n"),
      wag_tail()

  after 30000 ->
    bark()

  end.

sit() ->
  io:format("Dog: sits~n"),
  receive
    squirrel -> bark();

    _ ->
      io:format("Dog: confused~n"),
      sit()
  end.