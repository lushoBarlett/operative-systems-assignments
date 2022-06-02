-module(turnos).

-export([server/0]).

counter(N) ->
	receive
		Receiver ->
			Receiver ! N,
			counter(N + 1)
	end.

receive_id_from(Counter) ->
	Counter ! self(),
	receive
		N -> N
	end.

server() ->
	{ok, ListenSocket} = gen_tcp:listen(8000, [{reuseaddr, true}]),
	Counter = spawn(fun() -> counter(0) end),
	wait_connect(ListenSocket, Counter).

wait_connect(ListenSocket, Counter) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> wait_connect(ListenSocket, Counter) end),
	get_request(Socket, Counter).

get_request(Socket, Counter) ->
	io:fwrite("Esperando mensajes de ~p~n", [Socket]),
	receive
		{tcp, Client, "NUEVO\n"} ->
			Id = receive_id_from(Counter),
			StringId = integer_to_list(Id),
			gen_tcp:send(Client, StringId),
			get_request(Socket, Counter);
		{tcp, _Client, "CHAU\n"} ->
			gen_tcp:shutdown(Socket, read_write)
	end.
