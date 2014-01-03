-module(kbk_gcharts).
-export([chart/3, chart/4, chart/5, chart/6, out/1, appmod/0]).
-define(CHARTURL, "gcharts").
-define(DEFAULTWIDTH, 400).
-define(DEFAULTHEIGHT, 250).
-define(BORDER, 20).

-spec out(_) -> op:ehtml().
out(A) ->
	{ok, Str} = yaws_api:queryvar(A, "gc"),
	{ok, SW} = yaws_api:queryvar(A, "w"),
	{ok, SH} = yaws_api:queryvar(A, "h"),
	{ehtml,
		[
			{head, [],
				[
					{script, [{type, "text/javascript"}, {src, "https://www.google.com/jsapi"}], []},
					{script, [{type, "text/javascript"}],
						[
							ejs:eval(
								{[google, load], [{quotes, "visualization"}, "1"]}
							)
						]
					},
					{script, [{type, "text/javascript"}],
						[
							ejs:eval(
								{statements,
									[
										{var, json, Str},
										{var, rows, "json[\"rows\"]"},
										{var, cols, "json[\"cols\"]"},
										{var, dt, "new google.visualization.DataTable()"},
										{for,
											{'=', i, 0},
											{'<', i, {[cols, length]}},
											{'++', i},
											[
												{[dt, addColumn], ["cols[i][\"type\"]", "cols[i][\"name\"]"]}
											]
										},
										{for,
											{'=', i, 0},
											{'<', i, {[rows, length]}},
											{'++', i},
											[
												{'if', {[{'//', "new Date"}, test], ["rows[i][0]"]},
													[
														{'=', "rows[i][0]", {[eval], ["rows[i][0]"]}}
													]
												},
												{[dt, addRow], ["rows[i]"]}
											]
										},
										{'=', "json[\"dataTable\"]", dt},
										{function, drawChart, [],
											[
												{[google, visualization, "drawChart"],
													[
														json
													]
												}
											]
										},
										{[google, setOnLoadCallback], [drawChart]}
									]
								}
							)
						]
					}
				]
			},
			{body, [],
				[
					{'div',
						[
							{id, "chart"},
							{style,
								"width: " ++ SW ++ "px;" ++ 
								"height: " ++ SH ++ "px;"}
						]
					}
				]
			}
		]
	}.

-type column() :: {string(), string() | atom()}.

-spec chart(atom() | string(), [column()], [[op:data()]]) -> op:ehtml().
-spec chart(atom() | string(), [column()], [[op:data()]], [{_, _}]) -> op:ehtml().
-spec chart(atom() | string(), [column()], [[op:data()]], pos_integer(), pos_integer()) -> op:ehtml().
-spec chart(
		atom() | string(),
		[{string(), string() | atom()}],
		[[op:data()]],
		[{_, _}],
		pos_integer(),
		pos_integer()
	) -> op:ehtml().
chart(Type, Columns, Data) -> chart(Type, Columns, Data, []).
chart(Type, Columns, Data, Opts) ->
	chart(Type, Columns, Data, Opts, ?DEFAULTWIDTH, ?DEFAULTHEIGHT).
chart(Type, Columns, Data, W, H) ->
	chart(Type, Columns, Data, [], W, H).
chart(Type, Columns, Data, Opts, W, H) when is_atom(Type) ->
	chart(lookup_type(Type), Columns, Data, Opts, W, H);
chart(Type, Columns, Data, Opts, W, H) ->
	{iframe,
		[
			{frameBorder, "0"},
			{width, integer_to_list(W)},
			{height, integer_to_list(H)},
			{src, url(Type, Columns, Data, Opts, W, H)}
		]
	}.

-spec url(
		atom() | string(),
		[{string(), string() | atom()}],
		[[op:data()]],
		[{_, _}],
		pos_integer(),
		pos_integer()
	) -> string().
url(Type, Columns, Data, Opts, W, H) ->
	?CHARTURL ++
		"?w=" ++ integer_to_list(W-?BORDER) ++ "&" ++
		"h=" ++ integer_to_list(H-?BORDER) ++ "&" ++
		"gc=" ++
			yaws_api:url_encode(
				lists:flatten(
					json2:encode(
						{struct,
							[
								{chartType, Type},
								{rows,
									{array,
										[
											{array, format_timestamps(CurrentData)}
										||
											CurrentData <- Data
										]
									}
								},
								{cols,
									{array,
										lists:map(
											fun({Name, ColType}) ->
												{struct,
													[
														{name, Name},
														{type, ColType}
													]
												}
											end,
											Columns
										)
									}
								},
								{options,
									{struct,
										Opts
									}
								},
								{containerId, "chart"}
							]
						}
					)
				)
			).

-spec format_timestamps([op:data()]) -> [op:data()].
format_timestamps(Data) ->
	lists:map(
		fun({{Yr, Mo, Da}, {Hr, Mi, Se}}) ->
			lists:flatten(io_lib:format("new Date(~w, ~w, ~w, ~w, ~w, ~w)", [Yr, Mo, Da, Hr, Mi, Se]));
		   (X) -> X
		end,
		Data
	).

-spec appmod() -> {string(), atom()}.
appmod() -> {"/" ++ ?CHARTURL, ?MODULE}.

-spec lookup_type(atom()) -> string().
lookup_type(T) ->
	case T of
		annotatedtimeline -> "AnnotatedTimeLine";
		area -> "AreaChart";
		bar -> "BarChart";
		bubble -> "BubbleChart";
		candlestick -> "CandlestickChart";
		column -> "ColumnChart";
		combo -> "ComboChart";
		gauge -> "Gauge";
		geo -> "GeoChart";
		geomap -> "GeoMap";
		image -> "ImageChart";
		intensitymap -> "IntensityMap";
		line -> "LineChart";
		motion -> "MotionChart";
		org -> "OrgChart";
		pie -> "PieChart";
		scatter -> "ScatterChart";
		sparkline -> "ImageSparkLine";
		steppedarea -> "SteppedAreaChart";
		table -> "Table";
		timeline -> "AnnotatedTimeLine";
		treemap -> "TreeMap"
	end.
