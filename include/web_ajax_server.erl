-module(web_ajax_server).
-export([start/0, code_change/0, stop/0, is_started/0, add/1, add/2, add/3, remove/1, clear/0, server/1]).
-export([in_progress_count/0, completed_count/0]).
-define(TIMEOUT_H, 6).

% TODO: This /really/ needs upgrading to a record based system.

-spec start() -> ok.
start() -> register(web_ajax_server, spawn(web_ajax_server, server, [{0, 0, dict:new()}])), ok.

-spec stop() -> stopped.
stop() ->
	web_ajax_server ! stop,
	stopped.

-spec is_started() -> boolean().
is_started() -> whereis(?MODULE) =/= undefined.

-spec clear() -> ok.
clear() ->
	web_ajax_server ! clear,
	ok.

-spec in_progress_count() -> integer().
in_progress_count() ->
	web_ajax_server ! {in_progress, self()},
	receive
		{in_progress_count, Count} -> Count
	end.

-spec completed_count() -> integer().
completed_count() ->
	web_ajax_server ! {completed, self()},
	receive
		{completed_count, Count} -> Count
	end.

-spec add(fun()) -> integer().
-spec add(fun(), infinity | integer()) -> integer().
-spec add(fun(), infinity | integer(), [web_ajax:opt()]) -> integer().
add(Fun) -> web_ajax_server:add(Fun, infinity).
add(Fun, TTL) ->
	add(Fun, TTL, []).
add(Fun, TTL, Opts) ->
	web_ajax_server ! {add, ID = get_unused(), Fun, TTL, Opts, unix_ts() + (60 *60 * ?TIMEOUT_H)},
	ID.

-spec remove(integer()) -> {fun(), [web_ajax:opt()]}.
remove(ID) ->
	web_ajax_server ! {remove, self(), ID},
	receive
		{func, ID, Fun, Opts} -> {Fun, Opts}
	end.

-spec get_unused() -> integer().
get_unused() ->
	web_ajax_server ! {unused, self(), X = crypto:rand_uniform(1, 10000000000000000)},
	receive
		ok -> X;
		false -> io:format("web_ajax_server: ~w in use!~n", [X]), get_unused()
	end.

-spec unix_ts() -> integer().
unix_ts() ->
	{M, S, _} = now(),
	M*1000000 + S.

-spec code_change() -> ok.
code_change() ->
	web_ajax_server ! code_change,
	ok.

-spec server({integer(), integer(), _}) -> stopped.
server(State = {Completed, In_progress, FDict}) ->
	FunDict = 
		case (Completed rem 250) of
			0 -> dict:filter(fun(_, {_, _, _, TO}) -> TS = unix_ts(), if TS > TO -> false; true -> true end end, FDict);
			_ -> FDict
		end,
	receive
		{unused, PID, Num} -> PID ! case dict:is_key(Num, FunDict) of true -> false; false -> ok end, server(State);
		code_change -> web_ajax_server:server(State);
		stop -> stopped;
		clear -> server({Completed, 0, dict:new()});
		{in_progress, PID} -> PID ! {in_progress_count, In_progress}, server(State);
		{completed, PID} -> PID ! {completed_count, Completed}, server(State);
		{add, ID, Fun, TTL, Opts, Timeout} -> server({Completed, In_progress + 1, dict:store(ID, {Fun, TTL, Opts, Timeout}, FunDict)});
		{remove, PID, ID} ->
			case dict:find(ID, FunDict) of
				{ok, {Fun, TTL, Opts, Timeout}} ->
					PID ! {func, ID, Fun, Opts},
					case TTL of
						infinity -> server({Completed+1, In_progress, FunDict});
						1 -> server({Completed+1, In_progress-1, dict:erase(ID, FunDict)});
						Num -> server({Completed+1, In_progress, dict:store(ID, {Fun, Num-1, Opts, Timeout}, FunDict)})
					end;
				error ->
					PID ! {func, ID, fun() -> op_error:ajax({fun_not_found, ID}) end, []},
					server({Completed+1, In_progress, FunDict})
 			end
	end.
