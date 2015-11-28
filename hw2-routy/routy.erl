-module(routy).
-export([start/2, stop/1, init/1, status/1]).

start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
	Intf = intf:new(),
	Map = map:new(),
	Table = dijkstra:table(Intf, Map),
	Hist = hist:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
	receive
		% adding interfaces
		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = intf:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		{remove, Node} ->
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = intf:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = intf:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = intf:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);
		stop ->
			ok;

		% link-state messages
		{links, Node, R, Links} ->
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					intf:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					router(Name, N, Hist1, Intf, Table, Map1);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;
		update ->
			Table1 = dijkstra:table(intf:list(Intf), Map),
			router(Name, N, Hist, Intf, Table1, Map);
		broadcast ->
			Message = {links, Name, N, intf:list(Intf)},
			intf:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);

		% routing a message
		{route, Name, From, Message} ->
			io:format("~w: received message from ~w: \"~s\"~n",
				[Name, From, Message]),
			router(Name, N, Hist, Intf, Table, Map);
		{route, To, From, Message} ->
			io:format("~w: routing message to ~w from ~w: \"~s\"~n",
				[Name, To, From, Message]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case intf:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! {route, To, From, Message};
						notfound ->
							ok
					end;
				notfound ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);
		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map)
	end.

status(Router) ->
    Router ! {status, self()},
    receive
		{status, {Name, N, Hist, Intf, Table, Map}} ->
	    	io:format("┌────────────────────────────────────────"),
	    	io:format( "──────────────────────────────────────~n"),
			io:format("│ Status for ~w~n", [Name]),
			io:format("├───────┬────────────────────────────────"),
			io:format( "──────────────────────────────────────~n"),
			io:format("│ n     │ ~w~n", [N]),
			io:format("│ hist  │ ~w~n", [Hist]),
    		io:format("│ intf  │ ~w~n", [Intf]),
    		io:format("│ table │ ~w~n", [Table]),
    		io:format("│ map   │ ~w~n", [Map]),
    		io:format("└───────┴────────────────────────────────"),
    		io:format( "──────────────────────────────────────~n")
    after 5000 ->
    		io:format("Status request timed out~n")
    end.
