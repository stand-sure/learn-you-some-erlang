-module(my_behaviour).
%%-export([behavior_info/1]).
-callback init(list()) -> any().

%% init/1, some_fun/0 and other/3 are now expected callbacks
%%behavior_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}].