-module(exceptions).
-compile(export_all).

throws(Fn) ->
  try Fn() of
    _ -> ok
  catch
    Throw -> {throw, caught, Throw}
  end.

errors(Fn) ->
  try Fn() of
    _ -> ok
  catch
    error:Error -> {error, caught, Error}
  end.

exits(Fn) ->
  try Fn() of
    _ -> ok
  catch
    exit:Exit -> {exit, caught, Exit}
  end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
  try Attack() of
    _ -> "None shall pass."
  catch
    throw:slice -> "It is but a scratch.";
    error:cut_arm -> "I've had worse.";
    exit:cut_leg -> "Come on you pansy!";
    _:_ -> "Just a flesh wound."
  end.

talk() -> "blah blah".

catcher(X, Y) ->
  case catch X / Y of
    {'EXIT', {badarith, _}} -> "uh oh";
    N -> N
  end.

%%has_value(_, {node, 'nil'}) -> false;
%%has_value(Value, {node, {_, Value, _, _}}) -> true;
%%has_value(Value, {node, {_, _, Left, Right}}) ->
%%  case (has_value(Value, Left)) of
%%    true -> true;
%%    false -> has_value(Value, Right)
%%  end.

has_value(Value, Tree) ->
  try has_value1(Value, Tree) of
    false -> false
  catch
    true -> true
  end.

has_value1(_, {node, 'nil'}) -> false;
has_value1(Value, {node, {_, Value, _, _}}) -> throw(true);
has_value1(Value, {node, {_, _, Left, Right}}) ->
  has_value1(Value, Left),
  has_value1(Value, Right).
