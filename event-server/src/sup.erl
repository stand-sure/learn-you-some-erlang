%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2024 16:39
%%%-------------------------------------------------------------------
-module(sup).
-author("cj anderson").

%% API
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod,Args}]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod,Args}]).

init({Mod,Args}) ->
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args}).

loop({Module,Function,Args}) ->
  Pid = apply(Module,Function,Args),
  receive
    {'EXIT', _From, shutdown} -> exit(shutdown);
    {'EXIT', Pid, Reason} -> 
      io:format("Process ~p exited. Reason = ~p~n", [Pid,Reason]),
      loop({Module, Function, Args})
  end.
