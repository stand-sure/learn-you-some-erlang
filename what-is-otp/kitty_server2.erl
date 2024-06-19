%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2024 09:59
%%%-------------------------------------------------------------------
-module(kitty_server2).
-author("cjanderson").

-record(cat, {name :: string(), color = green :: atom(), description :: string()}).
-type cat() :: #cat{}.

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link() -> pid().
start_link() -> my_server:start_link(?MODULE, []).

-spec order_cat(
    LoopPid ::pid(), 
    Name :: string(), 
    Color :: atom(), 
    Description :: string()) -> cat().
order_cat(LoopPid, Name, Color, Description) ->
  my_server:call(LoopPid, {order, Name, Color, Description}).

-spec return_cat(LoopPid :: pid(), Cat :: cat()) -> ok.
return_cat(LoopPid, Cat = #cat{}) ->
  my_server:cast(LoopPid, {return, Cat}),
  ok.

-spec close_shop(LoopPid :: pid()) -> no_return().
close_shop(LoopPid) ->
  my_server:call(LoopPid, terminate).

-spec init([]) -> [].
init([]) -> [].

-type order_message() :: {order,
  Name :: atom() | string(),
  Color :: atom(),
  Description :: string()
}.

-type terminate_message() :: terminate.

-spec handle_call(
    Message :: order_message() | terminate_message(),
    From :: {pid(), reference()},
    Cats :: list(cat())) -> list(cat()).

handle_call({order, Name, Color, Description}, From, Cats) ->
  if
    Cats =:= [] ->
      my_server:reply(From, make_cat(Name, Color, Description)),
      Cats;

    Cats =/= [] ->
      my_server:reply(From, hd(Cats)),
      tl(Cats)

  end;

handle_call(terminate, From, Cats) ->
  my_server:reply(From, ok),
  terminate(Cats).

-spec handle_cast({return, Cat :: cat()}, Cats :: list(cat())) -> list(cat()).

handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat | Cats].

%% private

%% makes a cat
-spec make_cat(Name :: string(), Color :: atom(), Description :: string()) -> cat().
make_cat(Name, Color, Description) ->
  #cat{name = Name, color = Color, description = Description}.

%% frees cats, exits
-spec terminate(Cats :: list(cat())) -> no_return().
terminate(Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  exit(normal).