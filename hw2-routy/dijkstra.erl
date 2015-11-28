-module(dijkstra).
-export([update/4, iterate/3, table/2, route/2]).

update(Node, N, Gateway, Sorted) ->
	case entry(Node, Sorted) > N of
		true ->
			replace(Node, N, Gateway, Sorted);
		false ->
			Sorted
	end.

iterate(Sorted, Map, Table) ->
	case Sorted of
		[] ->
			Table;
		[{_, inf, _}|_] ->
			Table;
		[{Node, N, Gateway}|Tail] ->
			Reachable = map:reachable(Node, Map),
			Updated = lists:foldl(fun(E, Acc) ->
				update(E, N+1, Gateway, Acc) end, Tail, Reachable),
        	iterate(Updated, Map, [{Node, Gateway}|Table])
	end.

table(Gateways, Map) ->
	Direct = lists:foldl(fun(E, Acc) ->
		[{E, 0, E}|Acc] end, [], Gateways),
	Indirect = lists:foldl(fun(E, Acc) ->
		add_indirect(E, Acc, Gateways) end, [], map:all_nodes(Map)),
	iterate(Direct ++ Indirect, Map, []).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{Node, Gateway} ->
			{ok, Gateway};
		false ->
			notfound
	end.


%% private functions

entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		{Node, N, _} ->
			N;
		false ->
			0
	end.

replace(Node, N, Gateway, Sorted) ->
	lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})).

add_indirect(Node, List, Gateways) ->
	case lists:member(Node, Gateways) of
		true ->
			List;
		false ->
			[{Node, inf, unknown}|List]
	end.