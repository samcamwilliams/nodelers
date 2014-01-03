-module(nd_reminder).
-export([start/1, start/2, loop/1]).
-record(state, {
	tick = 1,
	pid,
	ops = []
}).

start(PID) -> start(PID, []).
start(PID, List) ->
	spawn(
		nd_reminder,
		loop,
		[
			#state {
				pid = PID,
				ops = List
			}
		]
	).

loop(RawS) ->
	S =
		RawS#state {
			tick = RawS#state.tick + 1,
			ops = 
				lists:filter(
					fun(X) ->
						RawNum = element(1, X),
						Unit = element(2, X),
						Msg = element(3, X),
						Num =
							erlang:round(RawNum *
							case Unit of
								minute -> 1;
								hour -> 60;
								day -> 60 * 24
							end),
						if RawS#state.tick rem Num == 0 ->
							(RawS#state.pid) ! Msg,
							case length(tuple_to_list(X)) of
								4 -> true;
								_ -> false
							end;
						true -> true
						end
					end,
					RawS#state.ops
				)
		},
	receive
		stop -> ok;
		{add, X} -> loop(S#state { ops = S#state.ops ++ [X] })
	after nd:minute() -> loop(S)
	end.


