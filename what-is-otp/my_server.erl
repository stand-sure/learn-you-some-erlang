-module(my_server).
-export([call/2, cast/2, reply/2, start/2, start_link/2]).

%%% Public API

-spec start(Module :: module(), InitialState :: any()) -> pid().

start(Module, InitialState) ->
  spawn(fun() -> init(Module, InitialState) end).

-spec start_link(Module :: module(), InitialState :: any()) -> pid().

start_link(Module, InitialState) ->
  spawn_link(fun() -> init(Module, InitialState) end).

-spec call(CallerPid :: pid(), Message :: any()) -> any().

call(CallerPid, Message) ->
  CallRef = monitor(process, CallerPid),
  CallerPid ! {sync, self(), CallRef, Message},

  receive
    {CallRef, Reply} ->
      demonitor(CallRef, [flush]),
      Reply;

    {'DOWN', CallRef, CallerPid, Reason} ->
      error(Reason)

  after 5000 ->
    error(timeout)
  end.

-spec cast(CallerPid :: pid(), Message :: any()) -> ok.

cast(CallerPid, Message) ->
  CallerPid ! {async, Message},
  ok.

-spec reply({CallerPd :: pid(), ReplyRef :: reference()}, any()) -> no_return().

reply({CallerPid, ReplyRef}, Reply) ->
  CallerPid ! {ReplyRef, Reply}.

%% Private
-spec init(Module :: module(), InitialState :: list(any())) -> no_return().
init(Module, InitialState) ->
  loop(Module, Module:init(InitialState)).

-spec loop(Module :: module(), State :: list(any())) -> no_return().
loop(Module, State) ->
  receive
    {async, Message} ->
      loop(Module, Module:handle_cast(Message, State));
    {sync, CallerPid, ReplyRef, Message} ->
      loop(Module, Module:handle_call(Message, {CallerPid, ReplyRef}, State))
  end.