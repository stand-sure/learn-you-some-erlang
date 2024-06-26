%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2024 13:20
%%%-------------------------------------------------------------------
-module(kitty_gen_server).
-author("cj anderson").

-behavior(gen_server).
%%-behaviour(my_behaviour). %% this doesn't work

-record(cat, {name :: string(), color = green :: atom(), description :: string()}).
-type cat() :: #cat{}.

%% API
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/0, handle_info/2, terminate/2, code_change/3]).
-export([order_cat/4, return_cat/2, close_shop/1]).

start_link() -> gen_server:start_link(?MODULE, [], []).

-spec order_cat(
    ServerRef :: gen_server:server_ref(),
    Name :: string(),
    Color :: atom(),
    Description :: string()
) -> term().

order_cat(ServerRef, Name, Color, Description) ->
  gen_server:call(ServerRef, {order, Name, Color, Description}).

-spec return_cat(
    ServerRef :: gen_server:server_ref(),
    Cat :: cat()
) -> ok.

return_cat(ServerRef, Cat = #cat{}) ->
  gen_server:cast(ServerRef, {return, Cat}).

-spec close_shop(ServerRef :: gen_server:server_ref()) -> term().

close_shop(ServerRef) ->
  gen_server:call(ServerRef, terminate).

-spec init([]) -> {ok, []}.

init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if
    Cats =:= [] ->
      {reply, make_cat(Name, Color, Description), Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat | Cats]}.

handle_info(Message, Cats) ->
  io:format("Unexpected message: ~p~n.", [Message]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

make_cat(Name, Color, Description) ->
  #cat{name = Name, color = Color, description = Description}.