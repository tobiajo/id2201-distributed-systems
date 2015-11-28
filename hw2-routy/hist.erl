-module(hist).
-export([new/1, update/3]).

new(Name) ->
	[{Name, inf}].

update(Node, N, History) ->
	case lists:keyfind(Node, 1, History) of
		{Node, M} ->
			case N > M of
				true ->
					{new, lists:keyreplace(Node, 1, History, {Node, N})};
				false ->
					old
			end;
		false ->
			{new, [{Node, N}|History]}
	end.