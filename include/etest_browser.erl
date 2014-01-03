-module(etest_browser).
-export([start/1, start/2]).
-export([action/2, set/3, set/4, get_style/3, get_attr/3, get_cookie/2, to_html/2, ask/3, get_html/1]).
-record(state, {
	port :: integer(),
	addr :: string(),
	vars = [] :: [{string(), string()}],
	cookies = [] :: [{string(), string()}],
	html :: ehtml()
}).

-type ehtml() ::
	string()
		| {Tag :: atom()}
		| {ehtml, [string() | ehtml()]}
		| {Tag :: atom(), [attr()]}
		| {Tag :: atom(), [attr()], [ehtml() | string()]}.
-type attr() ::
	atom()
		| {atom() | string(), string()}
		| {style, [{string(), string()}]}.

-type call_opt() :: {var, string()} | {send, string(), string(), string()}.

-spec start(integer()) -> #state{}.
-spec start(integer(), string()) -> #state{}.
start(Port) -> start(Port, "/").
start(Port, Addr) ->
	#state{
		port = Port,
		addr = Addr,
		html = parse_html(request("http://localhost:" ++ integer_to_list(Port) ++ Addr))
	}.

-spec get_html(#state{}) -> ehtml().
get_html(S) -> S#state.html.

-spec action(#state{}, {atom(), string()}) -> #state{}.
action(S, {Trig, ID}) ->
	{_, Attrs, _} = find_id(S#state.html, ID),
	case lists:keyfind(Trig, 1, Attrs) of
		{Trig, JS} -> apply_res(S, ajax_request(S, extract_call(JS)));
		false -> S
	end.

-spec set(#state{}, string(), string()) -> #state{}.
-spec set(#state{}, string(), string(), string()) -> #state{}.
set(S, ID, Value) -> set(S, ID, "value", Value).
set(S, ID, Key, Value) ->
	transform(S, ID,
		fun({Tag, Attrs, Body}) ->
			{
				Tag,
				case lists:keyfind(Key, 1, Attrs) of
					{Key, _} -> lists:keyreplace(Key, 1, Attrs, {Key, Value});
					false -> Attrs ++ [{Key, Value}]
				end,
				Body
			}
		end
	).

-spec get_attr(#state{}, string(), string()) -> string().
get_attr(S, ID, Key) ->
	case lists:keyfind(ID, 1, element(2, (find_id(S, ID)))) of
		{Key, Val} -> Val;
		false -> ""
	end.

-spec get_cookie(#state{}, string()) -> undefined | string().
get_cookie(S, Key) -> proplists:get_value(Key, S#state.cookies).

-spec to_html(#state{}, string()) -> string().
to_html(S, ID) ->
	lists:flatten(
		yaws_api:ehtml_expand(
			transform(
				find_id(S#state.html, ID),
				fun({Tag, Attrs, Body}) ->
					{Tag,
						lists:keyreplace(style, 1, Attrs,
							{style,
								string:join(
									lists:map(
										fun({Key, Val}) ->
											Key ++ ": " ++ Val
										end,
										element(2, lists:keyfind(style, 1, Attrs))
									),
									"; "
								)
							}
						),
						Body
					}
				end
			)
		)
	).

%actions(S, Inst) -> lists:foldl(fun(X, Acc) -> action(Acc, X) end, S, Inst).

-spec ask(#state{}, string(), {style, string()} | innerHTML) -> _.
ask(S, ID, Q) ->
	Obj = find_id(S#state.html, ID),
	case Q of
		{style, X} -> get_style(element(2, Obj), X);
		innerHTML -> element(3, Obj)
	end.

-spec extract_call(string()) -> {string(), [call_opt()]}.
extract_call(Str) ->
	{match, [Res]} = re:run(Str, "ajax_call\\('(fun\\=[0-9]+.*)'", [{capture, all_but_first, list}]),
	case string:tokens(Res, "?&") of
		[X] -> {X, []};
		R -> {hd(R), extract_args(tl(R))}
	end.

-spec extract_args([string()]) -> [call_opt()].
extract_args([]) -> [];
extract_args([H|T]) ->
	case re:run(H, "(.+)='\\+encodeURIComponent\\(document\\.getElementById\\('(.+)'\\).(.+)\\)\\+", [{capture, all_but_first, list}]) of
		{match, [A, ID, Val]} -> [{send, A, ID, Val}];
		nomatch ->
			case re:run(H, "(.+)=", [{capture, all_but_first, list}]) of
				{match, [A]} -> [{var, A}];
				nomatch -> []
			end
	end ++ extract_args(T).

-spec apply_res(#state{}, {ok, [{string(), string()}], string()}) -> #state{}.
apply_res(S, {ok, Hdrs, Lines}) ->
	lists:foldl(
		fun(L, Acc) ->
			execute(Acc, interpret_command(L))
		end,
		case lists:keyfind("set-cookie", 1, Hdrs) of
			{_, C} ->
				S#state {
					cookies =
						lists:foldl(
							fun(X = {Key, _}, XAcc) ->
								case lists:keyfind(X, 1, XAcc) of
									false -> XAcc ++ [X];
									{_, _} -> lists:keyreplace(Key, 1, XAcc, X)
								end
							end,
							S#state.cookies,
							string:tokens(C, ";")
						)
				};
			false -> S
		end,
		string:tokens(Lines, [10])
	).

-spec interpret_command(string()) -> {error, cannot_parse, string()} | string().
interpret_command(Str) ->
	case re:run(Str, "document\\.getElementById\\(\\'(.+)\\'\\)\\.{0,1}(.+) (.{1,2}) (.+);", [{capture, all_but_first, list}]) of
		{match, [ID, Attr, Op, NewVal]} ->
			{
				case Op of "=" -> to; "+=" -> append_to end,
				ID,
				string:tokens(Attr, "."),
				case re:run(NewVal, "decodeURIComponent\\(\\'(.+)\\'\\)", [{capture, all_but_first, list}]) of
					{match, [UrlEnc]} -> yaws_api:url_decode(UrlEnc);
					nomatch ->
						case re:run(NewVal, "\\'(.+)\\'", [{capture, all_but_first, list}]) of
							{match, [Val]} -> Val;
							nomatch -> NewVal
						end
				end
			};
		nomatch -> {error, cannot_parse, Str}
	end.

-spec execute(#state{} | ehtml(), _) -> #state{} | ehtml().
execute(S, {Op, ID, ["innerHTML"], Val}) ->
	transform(S, ID,
		fun({Tag, Attrs, CVal}) ->
			{
				Tag,
				Attrs,
				case Op of
					to -> parse_html(Val);
					append_to -> CVal ++ parse_html(Val)
				end
			}
		end
	);
execute(S, {_, ID, ["style", Key], Val}) ->
	transform(
		S,
		ID,
		fun({Tag, Attrs, Body}) ->
			{
				Tag,
				set_style(Attrs, Key, Val),
				Body
			}
		end
	).

-spec ajax_request(#state{}, {string(), [call_opt()]}) -> {ok, [{string(), string()}], string()}.
ajax_request(S, Req) ->
	case
		httpc:request(
			post,
			{
				"http://localhost:" ++ integer_to_list(S#state.port) ++ "/aj_srv.yaws",
				headers(S),
				"application/x-www-form-urlencoded; charset=UTF-8",
				ajax_body(S, Req)
			}, [], []
		) of
		{ok, {{_, 200, _}, Hdrs, Body}} -> {ok, Hdrs, Body};
		_ -> {error, request_failed, ajax}
	end.

-spec headers(#state{}) -> [{string(), string()}].
headers(S) ->
	[
		{"User-Agent", "etest/0.1"}
	] ++
	[
		{"Cookie",
			string:join(
				[
					Key ++ "=" ++ Val
				||
					{Key, Val} <- S#state.cookies
				],
				"&"
			)
		}
	].

-spec ajax_body(#state{}, {string(), [call_opt()]}) -> string().
ajax_body(_S, {Fun, []}) -> Fun;
ajax_body(S, {Fun, Opts}) ->
	Fun ++ "&" ++
	string:join(
		lists:map(
			fun({var, X}) -> X ++ "=" ++ element(2, lists:keyfind(X, 1, S#state.vars));
			({send, Arg, ID, Prop}) -> Arg ++ "=" ++ element(2, lists:keyfind(Prop, 1, element(2, find_id(S#state.html, ID))))
			end,
			Opts
		),
		"&"
	).

-spec request(string()) -> string().
request(URL) ->
	case httpc:request(URL) of
		{ok, {{_, 200, _}, _, Body}} -> Body;
		_ -> {error, request_failed, URL}
	end.

-spec find_id(ehtml(), string()) -> ehtml().
find_id(S, ID) when is_list(S) ->
	case
		lists:filter(
			fun(X) ->
				X =/= not_found
			end,
			lists:map(fun(Y) -> find_id(Y, ID) end, S)
		) of
		[] -> not_found;
		[X] -> X
	end;
find_id(S = {_, Attrs, Body}, ID) ->
	case lists:keyfind(id, 1, Attrs) of
		{id, ID} -> S;
		_ -> find_id(Body, ID)
	end;
find_id(_, _) -> not_found.

-spec transform(#state{} | ehtml(), fun((ehtml()) -> ehtml())) -> #state{} | ehtml().
-spec transform(#state{} | ehtml(), string() | any, fun((ehtml()) -> ehtml())) -> #state{} | ehtml().
transform(S, Fun) -> transform(S, any, Fun).
transform(S, ID, Fun) when is_record(S, state) -> S#state { html = transform(S#state.html, ID, Fun) };
transform(S, ID, Fun) when is_list(S) -> lists:map(fun(X) -> transform(X, ID, Fun) end, S);
transform(S = {Tag, Attrs, Body}, ID, Fun) ->
	case {ID, lists:keyfind(id, 1, Attrs)} of
		{any, _} ->
			{XTag, XA, XB} = Fun(S),
			{XTag, XA, transform(XB, ID, Fun)};
		{_, {id, ID}} -> Fun(S);
		_ -> {Tag, Attrs, transform(Body, ID, Fun)}
	end;
transform(S, _, _) -> S.

-spec parse_html(string()) -> ehtml().
parse_html("") -> [];
parse_html("\n" ++ Rest) -> parse_html(Rest);
parse_html("<" ++ Rest) ->
	{Tag, Rest2} = read(word, read(whitespace, Rest)),
	{RawAttrs, Rest3} = read(attributes, read(whitespace, Rest2)),
	Attrs = extract_styles(RawAttrs),
	Rest4 = read(whitespace, Rest3),
	case Rest4 of
		"/>" ++ Rest5 -> [{list_to_atom(Tag), Attrs, []}] ++ parse_html(Rest5);
		">" ++ Rest5 ->
			{Body, Rest6} = read(body, Rest5),
			[{list_to_atom(Tag), Attrs, parse_html(Body)}] ++ parse_html(Rest6)
	end;
parse_html(Str) ->
	case string:chr(Str, $<) of
		0 -> [Str];
		X -> [string:substr(Str, 1, X - 1)] ++ parse_html(string:substr(Str, X, length(Str) - X + 1))
	end.

-spec extract_styles([attr()]) -> [attr()].
extract_styles(List) ->
	case lists:keyfind(style, 1, List) of
		{style, Str} ->
			lists:keyreplace(
				style,
				1,
				List,
				{style,
					lists:map(
						fun(X) ->
							{match, [Key, Val]} = re:run(X, "(.+) *: *(.+)", [{capture, all_but_first, list}]),
							{Key, Val}
						end,
						string:tokens(Str, ";")
					)
				}
			);
		false -> List ++ [{style, []}]
	end.

-spec set_style([attr()], string(), string()) -> [attr()].
set_style(Attrs, Key, Val) ->
	case lists:keyfind(style, 1, Attrs) of
		{style, Styles} ->
			lists:keyreplace(
				style, 1,
				Attrs,
				{style,
					case lists:keyfind(Key, 1, Styles) of
						{Key, _} -> lists:keyreplace(Key, 1, Styles, {Key, Val});
						false -> Styles ++ [{Key, Val}]
					end
				}
			);
		false -> Attrs ++ [{style, [{Key, Val}]}]
	end.

-spec get_style(#state{}, string(), string()) -> string().
get_style(S, ID, Key) ->
	get_style(
		element(2, (find_id(S, ID))),
		Key
	).

-spec get_style([attr()], string()) -> string().
get_style(Attrs, Key) ->
	case lists:keyfind(style, 1, Attrs) of
		{style, Styles} ->
			case lists:keyfind(Key, 1, Styles) of
				{Key, Val} -> Val;
				false -> undefined
			end;
		false -> undefined
	end.

-spec read(whitespace | word | string | body | until | attributes, _) -> string() | {string(), string()} | {[{atom(), string()}], string()}.
read(whitespace, [X|Rest]) when X == $ ; X == $\t; X == $\n -> read(whitespace, Rest);
read(whitespace, Rest) -> Rest;
read(word, {Done, [Y|[X|Rest]]}) when not (((X >= 65) and (X =< 90)) or ((X >= 97) and (X =< 122))) -> {Done ++ [Y], [X] ++ Rest};
read(word, {Done, [Y|Rest]}) -> read(word, {Done ++ [Y], Rest});
read(word, Str) -> read(word, {"", Str});
read(string, [$"|Rest]) -> read(string, {"", Rest});
read(string, {Done, "\\\"" ++ Rest}) -> read(string, {Done ++ "\\\"", Rest});
read(string, {Done, [$"|Rest]}) -> {Done, Rest};
read(string, {Done, [X|Rest]}) -> read(string, {Done ++ [X], Rest});
read(body, {1, Done, "</"++ Rest}) -> {Done, read(until, {$>, Rest})};
read(body, {_, Done, ""}) -> {Done, ""};
read(body, {Lvl, Done, "</" ++ Rest}) -> read(body, {Lvl - 1, Done ++ "</", Rest});
read(body, {Lvl, Done, "/>" ++ Rest}) -> read(body, {Lvl - 1, Done ++ "/>", Rest});
read(body, {Lvl, Done, "<" ++ Rest}) -> read(body, {Lvl + 1, Done ++ "<", Rest});
read(body, {Lvl, Done, [X|Rest]}) -> read(body, {Lvl, Done ++ [X], Rest});
read(body, Rest) -> read(body, {1, "", Rest});
read(until, {X, [X|Rest]}) -> Rest;
read(until, {X, [_|Rest]}) -> read(until, {X, Rest});
read(attributes, Str) when is_list(Str) -> read(attributes, {[], Str});
read(attributes, {Attrs, Str}) ->
	try
		begin
			{Key, Rest} = read(word, read(whitespace, Str)),
			[$=|Rest2] = read(whitespace, Rest),
			{Val, Rest3} = read(string, Rest2),
			{Attrs ++ [{list_to_atom(string:to_lower(Key)), Val}], Rest3}
		end
	of
		X -> read(attributes, X)
	catch _:_ -> {Attrs, Str}
	end.
