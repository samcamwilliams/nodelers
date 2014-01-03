-module(etest).
-export([run/0]).
-export_type([group/0, test/0]).
-define(PORT, 8086).

-type test() ::
	{test, atom(), [op()], [op()]}
		| {test, atom(), [op()]}.

-type group() :: [test()].

-spec run() -> ok.
run() ->
	error_logger:info_msg("  Testing Started~n~n"),
	timer:sleep(100),
	RawTests =
		lists:foldl(
			fun(X, Acc) ->
				Acc ++ (list_to_atom(hd(string:tokens(X, ".")))):tests()
			end,
			[],
			element(2, {ok, _} = file:list_dir("test"))
		),
	Tests = lists:filter(fun({routine, _, _}) -> false; (_) -> true end, RawTests),
	Res = lists:map(fun({N, X}) -> test(N, X, RawTests) end, lists:zip(lists:seq(1, length(Tests)), Tests)),
	{Passes, Failures} = lists:partition(fun(C) -> C == pass end, Res),
	error_logger:info_report(
		[
			{passed, length(Passes)},
			{failed, length(Failures)}
		]
	),
	timer:sleep(100),
	ok.

-spec test(integer(), test(), [test()]) -> pass | fail.
test(N, {test, Name, Op}, Ts) -> test(N, {test, Name, Op, []}, Ts);
test(N, {test, Name, Op, Post}, Ts) ->
	io:format("   ~3..0B: ~p... ", [N, Name]),
	StartS = etest_browser:start(?PORT),
	{PostS, Res} =
		try exec(StartS, Op, Ts) of
			{S, pass} -> io:format("PASS~n"), {S, pass}
		catch
			throw:Reason ->
				io:format("FAIL~n"),
				io:format("     Reason: ~s~n~n", [Reason]),
				{StartS, fail};
			E:R ->
				io:format("FAIL~n"),
				io:format("     Reason: ~p:~p~n", [E, R]),
				io:format("  ~p~n", [erlang:get_stacktrace()]),
				{StartS, fail}
		end,
	{_, pass} = exec(PostS, Post, Ts),
	Res.

-type op() ::
	{run, atom()}
		| {click, string()}
		| {set, string(), string()}
		| {set, string(), string(), string()}
		| {transform, fun((_) -> _)}
		| {assert, _}
		| {assert, _, _}
		| {assert, _, _, _}
		| {assert, _, _, _, _}.
	
-spec exec(tuple(), [tuple()], [test()]) -> {tuple(), pass}.
exec(S, [], _Ts) -> {S, pass};
exec(S, [{run, Name}|T], Ts) ->
	case lists:keyfind(Name, 2, Ts) of
		false -> throw({routine_not_found, Name});
		X -> exec(element(1, exec(S, element(3, X), Ts)), T, Ts)
	end;
exec(S, [{click, ID}|T], Ts) ->
	exec(etest_browser:action(S, {onclick, ID}), T, Ts);
exec(S, [{set, Name, Value}|T], Ts) ->
	exec(etest_browser:set(S, Name, Value), T, Ts);
exec(S, [{set, Name, Key, Value}|T], Ts) ->
	exec(etest_browser:set(S, Name, Key, Value), T, Ts);
exec(S, [{transform, Fun}|T], Ts) ->
	exec(
		case erlang:fun_info(Fun, arity) of
			{arity, 0} -> Fun(), S;
			{arity, 1} ->
				case Fun(S) of
					ignore -> S;
					X -> X
				end
		end,
		T,
		Ts
	);
exec(S, [{assert, ID, visible}|T], Ts) ->
	exec(S, [{assert, ID, visible, true}|T], Ts);
exec(S, [{assert, ID, visible, Val}|T], Ts) ->
	exec(S, [{assert, ID, style, display, if Val -> "block"; true -> "none" end}|T], Ts);
exec(S, [{assert, ID, style, Key, Val}|T], Ts) ->
	case etest_browser:get_style(S, ID, Key) of
		Val -> exec(S, T, Ts);
		Val2 -> throw({styles_dont_match, Val2, Val})
	end;
exec(S, [{assert, ID, html, X}|T], Ts) ->
	case re:run(Str = etest_browser:to_html(S, ID), X) of
		{match, _} -> exec(S, T, Ts);
		nomatch -> throw({html_doesnt_match, Str, X})
	end;
exec(S, [{assert, cookie, Key}|T], Ts) ->
	case etest_browser:get_cookie(S, Key) of
		undefined -> throw({cookie_undefined, Key});
		_ -> exec(S, T, Ts)
	end;
exec(S, [{assert, cookie, Key, Val}|T], Ts) ->
	case etest_browser:get_cookie(S, Key) of
		Val -> throw({cookie_value_wrong, Key});
		_ -> exec(S, T, Ts)
	end;
exec(S, [{assert, ID, Key, Val}|T], Ts) ->
	case etest_browser:get_attr(S, ID, Key) of
		Val -> exec(S, T, Ts);
		Val2 -> throw({values_dont_match, Val2, Val})
	end;
exec(S, [{assert, Fun}|T], Ts) ->
	case Fun(etest_browser:get_html(S)) of
		true -> exec(S, T, Ts);
		false -> throw(state_transform_failed);
		{false, R} -> throw({state_transform_failed, R})
	end.
