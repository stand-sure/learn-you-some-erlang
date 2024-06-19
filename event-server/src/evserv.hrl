%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2024 16:24
%%%-------------------------------------------------------------------
-author("cj anderson").

-export_type([event/0, state/0]).

-record(event, {
  name = "" :: string(),
  description = "" :: string(),
  pid :: pid(),
  timeout = {{1970, 1, 1}, {0, 0, 0}} :: calendar:datetime()
}).

-type event() :: #event{}.

-record(state, {
  events :: orddict:orddict(event()),
  clients :: orddict:orddict(pid())
}).

-type state() :: #state{}.

