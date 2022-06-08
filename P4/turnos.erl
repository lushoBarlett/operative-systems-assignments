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
	{ok, ListenSocket} = gen_tcp:listen(8000, [{reuseaddr, true}, {active, false}]),
	Counter = spawn(fun() -> counter(0) end),
	wait_connect(ListenSocket, Counter).

wait_connect(ListenSocket, Counter) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> wait_connect(ListenSocket, Counter) end),
	get_request(Socket, Counter),
	ok.

get_request(Socket, Counter) ->
	io:fwrite("Esperando mensajes de ~p~n", [Socket]),
	case gen_tcp:recv(Socket, 0) of
		{ok, "NUEVO\n"} -> io:fwrite("received: NUEVO\n~n"),
				Id = receive_id_from(Counter),
				StringId = integer_to_list(Id),
				gen_tcp:send(Socket, StringId),
				get_request(Socket, Counter);
		{ok, "CHAU\n"} ->
			io:fwrite("Closing a socket~n"),
			gen_tcp:shutdown(Socket, read_write);
		{error, closed} ->
			io:fwrite("Closing a socket~n"),
			gen_tcp:shutdown(Socket, read_write);
		{error, Reason} ->
			io:fwrite("an error occurred~s~n", [Reason])
	end.
