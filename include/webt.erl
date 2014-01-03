-module(webt).
-export([radio_change/1, radio_change/2, checkbox_change/0, checkbox_change/1]).

-spec radio_change(string()) -> {onchange, string()}.
-spec radio_change(string(), no_fun | op:ejs() | [op:ejs()]) -> {onchange, string()}.
radio_change(Parent) -> radio_change(Parent, no_fun).
radio_change(Parent, Fun) ->
	{onchange,
		ejs:eval(
			{
				[
					{function, [el],
						[
							ejst:setValueById({quotes, Parent}, {[el, value]})
						] ++
						exec(Fun)
					}
				],
				[this]
			}
		)
	}.

-spec checkbox_change() -> {onchange, string()}.
-spec checkbox_change(no_fun | op:ejs() | [op:ejs()]) -> {onchange, string()}.
checkbox_change() -> checkbox_change(no_fun).
checkbox_change(Fun) ->
	{onchange,
		ejs:eval(
			{
				[
					{function, [el],
						[
							{'=', {[el, value]}, {[el, checked]}}
						] ++
						exec(Fun)
					}
				],
				[this]
			}
		)
	}.

-spec exec(no_fun | op:ejs() | [op:ejs()]) -> [op:ejs()].
exec(no_fun) -> [];
exec(X) when is_list(X) -> X;
exec(X) -> [X].
