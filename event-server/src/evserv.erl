%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2024 09:30
%%%-------------------------------------------------------------------
-module(evserv).
-author("cj anderson").
-include("evserv.hrl").

%% API
-export([init/0, start/0, start_link/0, terminate/0, subscribe/1, add_event/3, cancel/1, listen/1]).

-spec init() -> fun((state()) -> no_return()).
-spec loop(State :: state()) -> fun((state()) -> no_return()).
-spec start() -> pid().
-spec start_link() -> pid().
-spec terminate() -> shutdown.

-spec add_event(string(), string(), calendar:datetime()) -> any().
-spec cancel(Name :: any()) -> ok | {error, timeout}.
-spec subscribe(pid()) -> any().

start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.

init() ->
  loop(#state{
    events = orddict:new(),
    clients = orddict:new()
  }).

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} -> {ok, Ref};
    
    {'DOWN', Ref, process, _Pid, Reason} -> {error, Reason}
  
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, Timeout) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay * 1000 ->
    []
  end.

loop(State = #state{}) ->
  receive
    {CallerPid, MsgRef, {subscribe, Client}} -> handle_subscribe(State, CallerPid, MsgRef, Client);

    {CallerPid, MsgRef, {add, Name, Description, Timeout}} -> handle_add(State, CallerPid, MsgRef, Name, Description, Timeout);

    {CallerPid, MsgRef, {cancel, Name}} -> handle_cancel(State, Name, CallerPid, MsgRef);

    {done, Name} -> handle_done(State, Name);

    shutdown -> handle_shutdown();

    {'DOWN', Ref, process, _Pid, _Reason} -> handle_down(State, Ref);

    code_change -> ?MODULE:loop(State);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)

  end.

-spec handle_add(
    State :: state(),
    Pid :: pid(),
    MsgRef :: reference(),
    Name :: string(),
    Description :: string(),
    Timeout :: calendar:datetime()) -> fun((state()) -> no_return()).

-spec handle_cancel(
    State :: state(),
    Name :: string(),
    Pid :: pid(),
    MsgRef :: reference()) ->
  fun((state()) -> no_return()).

-spec handle_subscribe(
    State :: state(),
    Pid :: pid(),
    MsgRef :: reference(),
    Client :: pid()) ->
  fun((state()) -> no_return()).

handle_add(State, CallerPid, MsgRef, Name, Description, Timeout) ->
  case valid_datetime(Timeout) of
    true ->
      NewEvents = orddict:store(
        Name,
        #event{
          name = Name,
          description = Description,
          pid = event:start_link(Name, Timeout),
          timeout = Timeout
        },
        State#state.events),
      CallerPid ! {MsgRef, ok},
      loop(State#state{events = NewEvents});

    false ->
      CallerPid ! {MsgRef, {error, bad_timeout}},
      loop(State)
  end.

handle_subscribe(State, CallerPid, MsgRef, Client) ->
  ClientRef = erlang:monitor(process, Client),
  NewClients = orddict:store(ClientRef, Client, State#state.clients),
  CallerPid ! {MsgRef, ok},
  loop(State#state{clients = NewClients}).

handle_cancel(State, Name, Pid, MsgRef) ->
  Events = case orddict:find(name, State#state.events) of
             {ok, Event} ->
               event:cancel(Event#event.pid),
               orddict:erase(Name, State#state.events);

             error -> State#state.events
           end,
  Pid ! {MsgRef, ok},
  loop(State#state{events = Events}).

handle_done(State, Name) ->
  case orddict:find(Name, State#state.events) of
    {ok, Event} ->
      send_to_clients({done, Event#event.name, Event#event.description}, State#state.clients),
      loop(State#state{events = orddict:erase(Name, State#state.events)});
    error -> loop(State)
  end.

handle_shutdown() -> exit(shutdown).

handle_down(State, Ref) ->
  loop(State#state{clients = orddict:erase(Ref, State#state.clients)}).

send_to_clients(Msg, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, Clients).

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> false
  end;
valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).

valid_time(H, M, S) when H >= 0, H < 24,
  M >= 0, M < 60,
  S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

