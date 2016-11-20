-define(error1(Expr, Expected, Actual),
  io:format("~s is ~w instead of ~w at ~w:~w~n",
      [??Expr, Actual, Expected, ?MODULE, ?LINE])).

-define(match(Expected, Expr),
        fun() ->
    Actual = (catch (Expr)),
    case Actual of
        Expected ->
      {success, Actual};
        _ ->
      ?error1(Expr, Expected, Actual),
      erlang:error("match failed", Actual)
    end
  end()).