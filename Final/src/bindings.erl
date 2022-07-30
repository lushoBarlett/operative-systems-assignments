-module(bindings).
-import('sync', [createLock/0, lock/1, unlock/1, destroyLock/1]).
-export([start/0, put/3, get/2, take/2, del/2, stats/1, stop/1]).

start() ->
	{ok, Sock} = gen_tcp:connect("localhost", 8001, [{active, false}, binary]),
	Lock = createLock(),
	{Sock, Lock}.

integerTo4Bytes(Int) ->
	Bin = binary:encode_unsigned(Int),
	case byte_size(Bin) of
		1 -> [<<0,0,0>>, Bin];
		2 -> [<<0,0>>, Bin];
		3 -> [<<0>>, Bin];
		4 -> [Bin];
		_N -> throw("integer is too big")
	end.

readMore(Socket, Bytes, BytesToRead) ->
	case gen_tcp:recv(Socket, BytesToRead) of
		{ok, Packet} ->
			<<Bytes, Packet>>;
		true ->
			error
	end.

getValue(Socket, Bytes, BytesToRead) ->
	case BytesToRead of
		0 ->
			Bytes;
		true ->
			readMore(Socket, Bytes, BytesToRead)
	end.

parseArgument(Socket, Bytes) ->
	BytesLength = byte_size(Bytes),
	if
		BytesLength < 4 ->
			case readMore(Socket, Bytes, 0) of
				error -> error;
				NewBytes -> parseArgument(Socket, NewBytes)
			end;
		true ->
			Len = binary:decode_unsigned(binary:part(Bytes, 0, 4)),
			RemainingBytes = binary:part(Bytes, 4, BytesLength - 4),
			BytesToRead = Len - (BytesLength - 4),
			case getValue(Socket, RemainingBytes, BytesToRead) of
				error -> error;
				BinValue -> {ok, BinValue}
			end
	end.

decode(Socket, Bytes) ->
	case Bytes of
		<<101, Rest/binary>> -> 
			parseArgument(Socket, Rest);
		<<112>> ->
			enotfound;
		true ->
			error
	end.

recvAnswer(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			decode(Socket, Packet);
		{error, _Reason} ->
			gen_tcp:shutdown(Socket, read_write),
			error
	end.

recvCode(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, <<101>>} -> ok;
		{ok, <<112>>} -> enotfound;
		true -> error
	end.

put({Sock, Lock}, Key, Value) ->
	BinKey = term_to_binary(Key),
	BinValue = term_to_binary(Value),

	KeyLen = integerTo4Bytes(byte_size(BinKey)),
	ValLen = integerTo4Bytes(byte_size(BinValue)),

	lock(Lock),
	gen_tcp:send(Sock, [11, KeyLen, BinKey, ValLen, BinValue]),

	R = recvCode(Sock),
	unlock(Lock),

	R.

get({Sock, Lock}, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integerTo4Bytes(byte_size(BinKey)),

	lock(Lock),
	gen_tcp:send(Sock, [13, KeyLen, BinKey]),
	
	R = case recvAnswer(Sock) of
			{ok, BinValue} -> {ok, binary_to_term(BinValue)};
			Other -> Other
		end,
	unlock(Lock),
	
	R.	

take({Sock, Lock}, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integerTo4Bytes(byte_size(BinKey)),

	lock(Lock),
	gen_tcp:send(Sock, [14, KeyLen, BinKey]),
	
	R = case recvAnswer(Sock) of
			{ok, BinValue} -> {ok, binary_to_term(BinValue)};
			Other -> Other
		end,
	unlock(Lock),

	R.

del({Sock, Lock}, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integerTo4Bytes(byte_size(BinKey)),

	lock(Lock),
	gen_tcp:send(Sock, [12, KeyLen, BinKey]),

	R = recvCode(Sock),
	unlock(Lock),

	R.

stats({Sock, Lock}) ->
	lock(Lock),
	gen_tcp:send(Sock, [21]),

	R = recvAnswer(Sock),
	unlock(Lock),

	R.

stop({Port, Lock}) ->
	gen_tcp:close(Port),
	destroyLock(Lock),
	ok.