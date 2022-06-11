-module(turnos).

-export([server/0]).

% encuentra la primera ocurrencia de NUEVO\n o CHAU\n
firstOccurrence(Rec) ->
	N = string:str(Rec,"NUEVO\n"),
	C = string:str(Rec,"CHAU\n"),
	if
	    (N > 0) and (C == 0) -> {nuevo,N};
	    (N == 0) and (C > 0) -> chau;
	    (N == 0) and (C == 0) -> nada;
	    N < C -> {nuevo,N};
	    true -> chau
	end.

discardPrefix(Rec, N, Word) ->
	string:slice(Rec,N + string:length(Word) - 1).

% Mantiene un contador para devolver IDs unicos
counter(N) ->
	receive
		Receiver ->
			Receiver ! N,
			counter(N + 1)
	end.

% Pide un ID al contador
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
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} -> 
				spawn(fun() -> wait_connect(ListenSocket, Counter) end),
				get_request(Socket, Counter, "");
		{error, Reason} -> io:fwrite("error, reason: ~s~n", [Reason])
	end,
	ok.

get_request(Socket, Counter, Partial) ->
	io:fwrite("Esperando mensajes de ~p~n", [Socket]),
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			String = (string:concat(Partial,Packet)),
			decode(Socket, String, Counter);
		{error, closed} ->
			gen_tcp:shutdown(Socket, read_write);
		{error, Reason} ->
			io:fwrite("an error occurred~s~n", [Reason]),
			gen_tcp:shutdown(Socket, read_write)
	end.

% Decodifica lo recibido hasta que no haya nada interesante
decode(Socket, Rec, Counter) ->
	case firstOccurrence(Rec) of
		{nuevo, N} -> 
			sendCounter(Counter, Socket),
			Remaining = discardPrefix(Rec, N, "NUEVO\n"),
			decode(Socket, Remaining, Counter);
		chau ->
			gen_tcp:shutdown(Socket, read_write);
		nada -> get_request(Socket, Counter, Rec)
	end.

sendCounter(Counter, Socket) ->
	Id = receive_id_from(Counter),
	StringId = string:concat(integer_to_list(Id),"\n"),
	gen_tcp:send(Socket, StringId).
