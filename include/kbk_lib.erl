-module(kbk_lib).
-export([extract_ip/1, embed_logid/2]).
-export([connect/0, rpc/3]).
-export([log/3, log/5, log/6, add/2]).
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-define(DS, list_to_atom("head-DS@" ++ net_adm:localhost())).

%%% kbk_lib Version 1 (17/7/12)
%%% Theoretically containing all of the cross-site functions that we use in the Koboko network.
%%% Should only be updated once in a blue moon.

-type ip() :: {integer(), integer(), integer(), integer()}.

-spec connect() -> true.
connect() -> true = net_kernel:connect(?DS).

-spec extract_ip(#arg{}) -> ip().
extract_ip(Arg) ->
	Extras = (Arg#arg.headers)#headers.other,
	case lists:keyfind("X-Real-Ip", 3, Extras) of
		{http_header,_,"X-Real-Ip",_,IP} ->
			list_to_tuple([ list_to_integer(X) || X <- string:tokens(IP, ".") ]);
		false -> element(1, Arg#arg.client_ip_port)
	end.

-spec log(atom(), _, ip()) -> integer().
-spec log(atom(), _, ip(), _, #arg{}) -> integer().
-spec log(atom(), _, ip(), _, #arg{}, string() | undefined) -> integer().
log(Site, Action, IP) -> log(Site, Action, IP, [], #arg{ headers = #headers {}}).
log(Site, Action, IP, Details, Arg) ->
	log(Site, Action, IP, Details, Arg, (Arg#arg.headers)#headers.referer).
log(Site, Action, IP, Details, Arg, Ref) ->
	rpc:call(?DS, ds_analytics, log, [Site, erlang:localtime(), IP, Action, Details, Arg, Ref]).

-spec add(integer(), _) -> ok.
add(ID, Details) ->
	rpc:call(?DS, ds_analytics, add, [ID, Details]).

-spec rpc(atom(), atom(), [_]) -> _.
rpc(Module, Func, Args) ->
	rpc:call(?DS, Module, Func, Args).

-spec embed_logid(integer(), fun()) -> fun().
embed_logid(LogID, Fun) ->
	fun(X) ->
		Fun([LogID] ++ X)
	end.
