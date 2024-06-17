%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 11:32
%%%-------------------------------------------------------------------
-author("cjanderson").

-record(included, {
  some_field,
  some_default = "ok",
  unimaginative_name}).
