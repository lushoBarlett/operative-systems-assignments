-module(turnos).

-export([server/0]).

firstOccurrence(Rec) ->
	N = string:str(Rec,"NUEVO\n"),
	C = string:str(Rec,"CHAU\n"),
	if
	    (N > 0) and (C == 0) -> {nuevo,N};
	    (N == 0) and (C > 0) -> chau;
	    (N == 0) and (C == 0) -> nada;
	    true -> if
			N < C -> {nuevo,N};
			true -> chau
		    end
	end.

discardPrefix(Rec, N, Word) ->
	string:slice(Rec,N + string:length(Word) - 1).

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
	get_request(Socket, Counter, ""),
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
			io:fwrite("an error occurred~s~n", [Reason])
	end.

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
