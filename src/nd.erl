-module(nd).
-export([start/0, stop/0, rebuild/0, d/1, d/0, report/2, log/2]).
-export([rand/1, rand/2, tm/0]).
-export([minute/0, hour/0, day/0]).
-include("nd_recs.hrl").
-define(SERVER, {127,0,0,1}).
-define(PORT, 8089).

start() ->
	{S1, S2, S3} = now(),
	random:seed(S1, S2, S3),
	{ok, WD} = file:get_cwd(),
	yaws:start_embedded(io_lib:format("~s/site/", [WD]),
		[
			{id, "nodelers"},
			{servername, "nodelers"},
			{listen, ?SERVER},
			{port, ?PORT}
		],
		[
			{cache_refresh_secs, 0}
		],
		"nodelers"
	),
	inets:start(),
	ssl:start(),
	web_ajax_server:start(),
	nd_world:start(),
	ok.

log(S, Action) ->
	kbk_lib:add(S#pstate.logid, Action).

d() -> d("BREAKPOINT REACHED").
d(A) -> io:format("DEBUG: ~p~n", [A]), A.

report(Str, Args) ->
	error_logger:info_msg(Str ++ "~n", Args).

rebuild() ->
	case make:all([load]) of
		up_to_date ->
			web_ajax_server:code_change(),
			ws_server ! code_change,
			ws_reminder ! code_change,
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			ok;
		Else ->
			Else
	end.

stop() ->
	yaws:stop().

rand(X) -> rand(0, X).
rand(X, Y) -> crypto:rand_uniform(X, Y + 1).

tm() -> 1000.

minute() -> erlang:round(60000 / tm()).
hour() -> erlang:round(3600000 / tm()).
day() -> erlang:round(86400000 / tm()).
