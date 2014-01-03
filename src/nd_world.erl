-module(nd_world).
-export([start/0, loop/1]).
-export([ident_look/1, ident_look_filter/2]).
-define(WIDTH, 10).
-define(HEIGHT, 5).
-define(NODELER_CHANCE, 5).
-define(GRASS_CHANCE, 2).
-record(state, {
	dict = setup()
}).

start() ->
	register(nd_world, spawn(nd_world, loop, [#state{}])).

ident_look(PID) ->
	nd_world ! {look, PID},
	receive
		{look, RawItems} ->
			Items = [ X || X <- RawItems, X =/= PID ], %% TODO: Such a hack - we don't even.
			lists:foreach(
				fun(X) ->
					X ! {ident, self()}
				end,
				Items
			),
			lists:map(
				fun(X) ->
					receive
						{ident, X, Type} -> {X, Type}
					end
				end,
				Items
			)
	end.

ident_look_filter(PID, Type) ->
	lists:map(
		fun({Y, _}) -> Y end,
		lists:filter(
			fun({_, X}) -> X == Type end,
			ident_look(PID)
		)
	).

loop(S) ->
	%nd:report("Dupes: ~w~n", [dupes(S)]),
	receive
		stop -> ok;
		code_change -> nd_world:loop(S);
		{world, PID} -> PID ! {world, S#state.dict}, loop(S);
		{all, PID} -> PID ! {all, S#state.dict}, loop(S);
		{look, PID} -> PID ! {look, get_location(PID, S) -- [PID]}, loop(S);
		{neighbours, PID} ->
			{X, Y} = locate(PID, S),
			PID !
				{neighbours,
					[
						{X + X2, Y + Y2}
					||
						X2 <- lists:seq(-1, 1),
						Y2 <- lists:seq(-1, 1),
						get_location({X + X2, Y + Y2}, S) =/= not_found
					] -- [{X, Y}]
				},
			loop(S);
		{move, PID, Co} ->
			OldCo = locate(PID, S#state.dict),
			loop(
				S#state {
					dict =
						lists:keyreplace(
							Co,
							1,
							lists:keyreplace(
								OldCo,
								1,
								S#state.dict,
								{OldCo, get_location(PID, S) -- [PID]}
							),
							{Co, get_location(Co, S) ++ [PID]}
						)
				}
			);
		{add, PID, CoPID} ->
			CoOrd = locate(CoPID, S),
			loop(
				S#state {
					dict =
						lists:keyreplace(
							CoOrd,
							1,
							S#state.dict,
							{CoOrd, get_location(CoPID, S) ++ [PID]}
						)
				}
			);
		{remove, PID} ->
			CoOrd = locate(PID, S),
			loop(
				S#state {
					dict =
						lists:keyreplace(
							CoOrd,
							1,
							S#state.dict,
							{CoOrd, get_location(PID, S) -- [PID]}
						)
				}
			)
	end.

locate(PID, S) when is_tuple(S) -> locate(PID, S#state.dict);
locate(_, []) -> not_found;
locate(PID, [{Co, List}|Rest]) ->
	case lists:member(PID, List) of
		true -> Co;
		false -> locate(PID, Rest)
	end.

get_location({X, Y}, _) when X < 0; Y < 0; X > ?WIDTH; Y > ?HEIGHT -> not_found;
get_location(Co = {_, _}, S) ->
	case lists:keyfind(Co, 1, S#state.dict) of
		{_, Data} -> Data;
		false -> not_found
	end;
get_location(PID, S) ->
	element(2, lists:keyfind(locate(PID, S#state.dict), 1, S#state.dict)).

setup() ->
	[
		{{X, Y},
			[] ++
			case nd:rand(?GRASS_CHANCE) of
				0 -> [nd_grass:start()];
				_ -> []
			end ++
			case nd:rand(?NODELER_CHANCE) of
				0 -> [nd_nodeler:start()];
				_ -> []
			end
		}
	||
		X <- lists:seq(1, ?WIDTH), Y <- lists:seq(1, ?HEIGHT)
	].

dupes(#state { dict = D }) ->
	lists:foldl(
		fun(_, true) -> true;
			(X, Acc) ->
				case lists:member(X, Acc) of
					true -> true;
					false -> Acc ++ [X]
				end
		end,
		[],
		lists:flatten([ X || {_, X} <- D ])
	) == true.
