-module(bindings).
-import('sync', [createLock/0, lock/1, unlock/1, destroyLock/1]).
-export([start/0, put/3, get/2, take/2, del/2, stats/1, integer_to_4bytes/1]).

start() ->
	{ok, Sock} = gen_tcp:connect("localhost", 8000, [{active, false}]),
	Lock = createLock(),
	{Sock, Lock}.

integer_to_4bytes(Int) ->
	Bin = binary:encode_unsigned(Int),
	case byte_size(Bin) of
		1 -> [<<0,0,0>>, Bin];
		2 -> [<<0,0>>, Bin];
		3 -> [<<0>>, Bin];
		4 -> [Bin];
		_N -> throw("integer is too big")
	end.

put({Sock, Lock}, Key, Value) ->
	BinKey = term_to_binary(Key),
	BinValue = term_to_binary(Value),
	KeyLen = integer_to_4bytes(byte_size(BinKey)),
	ValLen = integer_to_4bytes(byte_size(BinValue)),
	lock(Lock),
	gen_tcp:send(Sock, [11, KeyLen, BinKey, ValLen, BinValue]),
	unlock(Lock).

get({Sock, Lock}, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integer_to_4bytes(byte_size(BinKey)),
	lock(Lock),
	gen_tcp:send(Sock, [13, KeyLen, BinKey]),
	unlock(Lock).

take(Sock, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integer_to_4bytes(byte_size(BinKey)),
	gen_tcp:send(Sock, [14, KeyLen, BinKey]).

del(Sock, Key) ->
	BinKey = term_to_binary(Key),
	KeyLen = integer_to_4bytes(byte_size(BinKey)),
	gen_tcp:send(Sock, [12, KeyLen, BinKey]).

stats(Sock) ->
	gen_tcp:send(Sock, [21]).
