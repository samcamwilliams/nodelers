-module(web_ajax).
-export([request/1, request/2, base/0]).
-export_type([opt/0]).
-define(default_url, "/aj_srv.yaws")
-define(funID_arg_name, "fun").

%% ACCEPTABLE OPTIONS
%%    ttl = Time To Live
%%    ready = Execute this JS after the result is recieved, response body avialable in 'response'
%%    result_to = Send the whole response to this ID
%%    post_call = Executed after the call is made
%%    send_js = JS variables to send, sent as first arguments
%%    send = Input form fields to send, sent after js

-type opt() ::
	{ttl, integer()}
		| {ready, string() | op:ejs()}
		| {result_to, string(), string() | op:ehtml()}
		| {post_call, string() | op:ejs()}
		| {send_js, [string()]}
		| {send, [string()]}.

-spec base() -> op:ejs().
base() ->
	{function, ajax_call, [post, exec], 
		lists:flatten([
			{var, xhr},
			{'if', {'==', {["navigator", "appName"]}, {quotes, "Microsoft.XMLHTTP"}},
				[
					{'=', xhr, {new, "ActiveXObject", [{quotes, "Microsoft.XMLHTTP"}]}}
				],
				[
					{'=', xhr, {new, "XMLHttpRequest"}}
				]
			},
			{[xhr, "open"], 
				[
					{quotes, "POST"},
					{quotes, "/aj_srv.yaws"},
					"true"
				]
			},
			{[xhr, "setRequestHeader"], [{quotes, "Content-type"}, {quotes, "application/x-www-form-urlencoded"}]},
			{'=', {[xhr, "onreadystatechange"]},
				{function, [],
					[
						{'if', {'==', {[xhr, "readyState"]}, 4},
							[
								{[eval], [{[xhr, "responseText"]}]}
							]
						}
					]
				}
			},
			{[xhr, "send"], [post]},
			{'if', {'!=', exec, 0},
				[
					{[exec], []}
				]
			}
		])
	}.

-spec request(fun()) -> string().
-spec request(fun(), [opt()]) -> string().
request(Fun) -> request(Fun, []).
request(Fun, Opts) ->
	FunID = web_ajax_server:add(Fun, 
		case lists:keyfind(ttl, 1, Opts) of
			{ttl, Val} -> Val;
			false -> infinity
		end, Opts),
	ejs:eval(
		{[ajax_call],
			[
				post_from_opts(FunID, Opts),
				case lists:keyfind(post_call, 1, Opts) of
					{post_call, Statements} ->
						{function, [],
							[
								{statements, Statements}
							]
						};
					false -> 0
				end
			]
		}
	).

-spec post_from_opts(integer(), [opt()]) -> op:ejs().
post_from_opts(FunID, Opts) ->
	{quotes, lists:flatten([
		"fun=" ++ integer_to_list(FunID),
		lists:map(
			fun({Atom, Fun}) ->
				case lists:keyfind(Atom, 1, Opts) of
					{Atom, Vars} ->
						lists:map(
							fun({ID, _}) -> Fun(ID);
							(ID) -> Fun(ID)
							end,
							Vars
						);
					false -> ""
				end
			end,
			[
				{send_js, fun(JSName) -> "&" ++ JSName ++ "='+" ++ JSName ++ "+'" end},
				{send, fun(Name) -> "&" ++ Name ++ "='+encodeURIComponent(document.getElementById('" ++ Name ++ "').value)+'" end}
			]
		)
	])}.
