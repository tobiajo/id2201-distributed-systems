-module(test).
-export([main/1, bench/2]).

-define(REQUESTS, 100). % my code

%% My main function
main([A,B]) ->
	Host = atom_to_list(A),
	Port = list_to_integer(atom_to_list(B)),
	Seconds = bench(Host,Port)/1000000,
	io:format("Bench: ~.1f request/s~n", [?REQUESTS/Seconds]),
	init:stop().

bench(Host, Port) ->
	Start = now(),
	run(?REQUESTS, Host, Port),
	Finish = now(),
	timer:now_diff(Finish, Start).

run(N, Host, Port) ->
	if
		N == 0 ->
			ok;
		true ->
			request(Host, Port),
			run(N-1, Host, Port)
	end.

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, http:get("foo")),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} ->
			ok;
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end,
	gen_tcp:close(Server).