-module(nd_nodeler).
-export([start/0, start/1, loop/1]).
-record(state, {
	appearance = nd:rand(1, 100),
	gender = case nd:rand(1) of 0 -> male; 1 -> female end,
	eat = nd:rand(10),
	sleep = nd:rand(10),
	sex = nd:rand(10),
	age = 0,
	pregnant = false,
	reminder,
	asleep = false
}).

-spec start() -> pid().
-spec start(#state{}) -> pid().
start() -> start(#state{}).
start(S) ->
	PID = spawn(nd_nodeler, loop, [S]),
	nd:report("~w: Born. Gender: ~w.", [PID, S#state.gender]),
	PID !
		{
			reminder,
			nd_reminder:start(
				PID,
				[
					{vary(30), minute, eat, repeat},
					{vary(1), hour, sleep, repeat},
					{vary(6), hour, sex, repeat},
					{1, day, age, repeat},
					{vary(5, 10), minute, action}
				]
			)
		},
	PID.

vary(Num) -> vary(Num, 15).
vary(Num, Pc) ->
	Low = (100 - (Pc / 2)) * 1000,
	High = (100 + (Pc / 2)) * 1000,
	(Num / 100) * (nd:rand(erlang:trunc(Low), erlang:trunc(High)) / 1000).

-spec loop(#state{}) -> dead | _.
loop(S) ->
	case is_dead(S) of
		true -> nd:report("~w: died.", [self()]), nd_world ! {remove, self()}, dead;
		false -> handle(S)
	end.

handle(S) ->
	receive
		{ident, PID} -> PID ! {ident, self(), nodeler}, loop(S);
		{info, PID} -> PID ! {info, S}, loop(S);
		stop -> ok;
		{reminder, PID} ->
			loop(S#state { reminder = PID });
		eat ->
			loop(
				S#state {
					eat = S#state.eat + (nd:rand(36, 46) / 100)
				}
			);
		sleep ->
			loop(
				S#state {
					sleep = S#state.sleep + (nd:rand(15, 20) / 100)
				}
			);
		sex ->
			loop(
				S#state {
					sex = S#state.sex + (nd:rand(60, 120) / 100)
				}
			);
		wake_up ->
			loop(
				S#state {
					asleep = false
				}
			);
		age ->
			loop(
				S#state {
					age = S#state.age + 1
				}
			);
		{appearance, PID} -> PID ! {appearance, self(), S#state.appearance}, loop(S);
		{trimester, 3} ->
			nd_world ! {add, nd_nodeler:start(S#state.pregnant), self()},
			loop(S#state { pregnant = false });
		{trimester, X} ->
			case nd:rand(1, 10) of
				1 -> loop(S#state { pregnant = false });
				_ ->
					trimester_reminder(S, X + 1),
					loop(S)
			end;
		{sex_request, PID, Appearance} when not S#state.asleep ->
			PID !
				{sex_request,
					self(),
					compatible(S, Appearance),
					S
				},
			loop(S);
		{sex, Partner} ->
			loop(
				have_sex(S, Partner)
			);
		action -> loop(perform_action(S))
	end.

compatible(S, A) -> (abs(S#state.appearance - A) + nd:rand(25)) > S#state.sex.

perform_action(S) ->
	NewS = 
		case Action = choose_action(S) of
			{sleep, Hrs} -> sleep(S, Hrs);
			{eat, Type, PID} -> eat(S, Type, PID);
			{sex, PID} -> sex(S, PID);
			{move, Co} -> move(S, Co)
		end,
	nd:report("~w chose to ~w", [self(), Action]),
	set_reminder(NewS),
	NewS.

choose_action(S) ->
	Opts = lists:filter(
		fun(sleep) -> S#state.sleep > 1;
		   (eat) -> nd_world:ident_look_filter(self(), grass) =/= [];
		   (sex) -> nd_world:ident_look_filter(self(), nodeler) =/= [];
		   (move) -> true
		end,
		[sleep, eat, sex, move]
	),
	case lists:nth(nd:rand(1, length(Opts)), Opts) of
		sleep -> {sleep, (nd:rand(15, 25) / 10) * (S#state.sleep / 100)};
		eat -> {eat, grass, hd(nd_world:ident_look_filter(self(), grass))};
		sex ->
			{sex,
				hd(
					lists:filter(
						fun(X) ->
							X ! {appearance, self()},
							receive
								{appearance, X, A} -> compatible(S, A)
							end
						end,
						nd_world:ident_look_filter(self(), nodeler)
					)
				)
			};
		move ->
			nd_world ! {neighbours, self()},
			receive
				{neighbours, Ns} ->
					{move, lists:nth(nd:rand(1, length(Ns)), Ns)}
			end
	end.

set_reminder(S) -> (S#state.reminder) ! {add, {max(0, 100 - frantic(S)), minute, action}}.

frantic(S) -> (S#state.sex + S#state.eat + S#state.sleep) / 3.

sleep(S, Hrs) ->
	(S#state.reminder) ! {add, {Hrs, hour, wake_up}},
	S#state { asleep = true }.

eat(S, grass, PID) ->
	PID ! {volume, self()},
	receive
		{volume, PID, Vol} ->
			Reduction = erlang:min(S#state.eat * (2/3), Vol),
			PID ! {reduce, Reduction},
			S#state {
				eat = S#state.eat + Reduction
			}
	end.

move(S, Co) ->
	nd_world ! {move, self(), Co},
	S.

sex(S, PID) ->
	PID ! {sex_request, self(), S#state.appearance},
	receive
		{sex_request, PID, true, Partner} ->
			nd:report("~w: Sex request accepted by ~w.", [self(), PID]),
			PID ! {sex, S},
			have_sex(S, Partner);
		{sex_request, PID, false, _} ->
			nd:report("~w: Sex request rejected by ~w.", [self(), PID]),
			S
	after nd:minute() * nd:rand(1,5) -> S
	end.

have_sex(S, Partner) ->
	S#state {
		sex = max(S#state.sex - abs(S#state.appearance - Partner#state.appearance), 0),
		eat = S#state.eat + (nd:rand(0, 100) / 100),
		pregnant =
			if
				S#state.gender == male;
				S#state.pregnant =/= false -> S#state.pregnant;
			true ->
				case {S#state.gender, Partner#state.gender, nd:rand(100) > 50} of
					{female, male, true} -> impregnate(S, Partner);
					_ -> false
				end
			end
	}.

impregnate(S, Partner) ->
	nd:report("~w: Impregnated.", [self()]),
	trimester_reminder(S, 1),
	Min = max(0, (min(S#state.appearance, Partner#state.appearance) - 10)),
	Max = min(100, (max(S#state.appearance, Partner#state.appearance) + 10)),
	#state {
		appearance = nd:rand(Min, Max)
	}.

trimester_reminder(S, Num) ->
	(S#state.reminder) ! {add, {8, hour, {trimester, Num}}}.

is_dead(S) ->
	lists:any(
		fun(X) -> X end,
		[
			S#state.eat > 100,
			S#state.sleep > 100
		]
	).
