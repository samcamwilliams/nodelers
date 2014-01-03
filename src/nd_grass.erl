-module(nd_grass).
-export([start/0, start/1, loop/1]).
-record(state, {
	volume
}).

start() -> start(nd:rand(1, 1000) / 10).
start(Vol) ->
	PID = spawn(
		nd_grass,
		loop,
		[
			#state {
				volume = Vol
			}
		]
	),
	nd_reminder:start(PID, [{5, minute, grow, repeat}]),
	PID.


loop(S) ->
	receive
		{ident, PID} -> PID ! {ident, self(), grass}, loop(S);
		{info, PID} -> PID ! {info, S}, loop(S);
		grow ->
			loop(
				S#state {
					volume =
						case S#state.volume of
							0 -> nd:rand(30, 45) / 10;
							Vol -> Vol + ((1 / Vol) * (nd:rand(30, 45) / 10))
						end
				}
			);
		{volume, PID} ->
			PID ! {volume, self(), S#state.volume},
			loop(S);
		{reduce, Amount} ->
			loop(
				S#state {
					volume = S#state.volume - Amount
				}
			)
	end.
