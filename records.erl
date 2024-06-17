%%%-------------------------------------------------------------------
%%% @author cj anderson
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2024 11:09
%%%-------------------------------------------------------------------
-module(records).
-author("cjanderson").
-include("records.hrl").

%% API
-export([first_robot/0, car_factory/1, admin_panel/1, adult_section/1, repairman/1, included/0]).

-record(robot, {name,
  type = industrial,
  hobbies,
  details = []}).

first_robot() ->
  #robot{
    name = "Mo",
    type = handmade,
    details = ["Look! No strings!"]
  }.

car_factory(CorpName) ->
  #robot{name = CorpName, hobbies = "building cars"}.

-record(user, {id, name, group, age}).

%% admin_panel indicates is the user is allowed.

-spec admin_panel(User) -> string()
  when User :: #user{}.

admin_panel(#user{name = Name, group = admin}) ->
  Name ++ " is allowed";
admin_panel(#user{name = Name}) ->
  Name ++ " is not allowed".

adult_section(U = #user{}) when U#user.age >= 18 ->
  allowed;
adult_section(_) -> forbidden.

%% repairman repairs a robot.
-spec repairman(Robot) -> #robot{}
  when Robot :: #robot{}.

repairman(Robot) ->
  Details = Robot#robot.details,
  NewRobot = Robot#robot{details = ["Repaired by repairman" | Details]},
  {repaired, NewRobot}.

included() -> #included{some_field = "abc"}.