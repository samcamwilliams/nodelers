-module(nd_page).
-export([main/1, display/0]).

main(_) ->
	[
		{'div', [{id, "displayarea"}], [display()]},
		{'div', [{id, "infoarea"}], [""]}
	].

display() ->
	nd_world ! {world, self()},
	Table =
		receive
			{world, Dict} -> partition(Dict)
		end,
	{table, [],
		[
			{tr, [],
				[
					{td, [],
						[
							{'div', [],
								[
									{'div',
										[
											{onClick,
												web_ajax:request(
													fun() ->
														PID ! {info, self()},
														receive
															{info, S} -> {to, "infoarea", lists:flatten(io_lib:format("~w", [S]))}
														end
													end
												)
											}
										],
										[lists:flatten(io_lib:format("~w", [PID]))]
									}
								||
									PID <- Cell
								]
							}
						]
					}
				||
					Cell <- Row
				]
			}
		||
			Row <- Table
		]
	}.

partition(Dict) -> partition([], [], element(1, element(1, hd(Dict))), Dict).

partition(D, Do, _, []) -> D ++ [Do];
partition(D, Do, X, [{{X, _}, C}|R]) -> partition(D, Do ++ [C], X, R);
partition(D, Do, _, [{{X, _}, C}|R]) -> partition(D ++ [Do], [C], X, R).
