%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2024 16:17
%%%-------------------------------------------------------------------
-author("cj anderson").

-record(event, {
  name = "" :: string(),
  description = "" :: string(),
  pid :: pid(),
  timeout = {{1970, 1, 1}, {0, 0, 0}} :: calendar:datetime()
}).

-record(state, {
  server :: pid(),
  name = "" :: string(),
  to_go = 0 :: integer()}).

-type state() :: #state{}.

