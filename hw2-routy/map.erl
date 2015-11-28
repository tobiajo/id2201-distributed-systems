-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
	[].

update(Node, Links, Map) ->
	lists:keystore(Node, 1, Map, {Node, Links}).

reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		{Node, Links} ->
			Links;
		false ->
			[]
	end.

all_nodes(Map) ->
	lists:foldl(fun(E, Acc) -> add_entry(E, Acc) end, [], Map).


%% private functions

add_entry({Node, Links}, List) ->
	lists:foldl(fun(E, Acc) -> add_node(E, Acc) end, List, [Node|Links]).

add_node(Node, List) ->
	case lists:member(Node, List) of
		true ->
			List;
		false ->
			[Node|List]
	end.