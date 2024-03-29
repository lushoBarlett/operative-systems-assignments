-module(bindings).
-import('sync', [createLock/0, lock/1, unlock/1, destroyLock/1]).
-export([start/0, put/3, get/2, take/2, del/2, stats/1, stop/1]).

%% Conecta al puerto 889 en modo pasivo y binario
%% 
%% Retorna un socket y un lock para que los mensajes
%% que se envian y reciben no se mezclen porque puede
%% haber varios threads ejecutando operaciones
start() ->
	{ok, Sock} = gen_tcp:connect("localhost", 889, [{active, false}, binary]),
	Lock = createLock(),
	{Sock, Lock}.

%%
%% Cierra el puerto y destruye el lock
%%
stop({Port, Lock}) ->
	gen_tcp:close(Port),
	destroyLock(Lock),
	ok.

%%
%% Convierte un entero en su representacion binaria big endian
%% de cuatro bytes
%%
integerTo4Bytes(Int) ->
	Bin = binary:encode_unsigned(Int),
	case byte_size(Bin) of
		1 -> [<<0,0,0>>, Bin];
		2 -> [<<0,0>>, Bin];
		3 -> [<<0>>, Bin];
		4 -> [Bin];
		_N -> throw("integer is too big")
	end.

%%
%% Dados un socket, un binario y una cantidad de bytes
%% por leer, lee del socket esa cantidad o menos y los
%% agrega al binario. En caso de error retorna error
%%
readMore(Socket, Bytes, BytesToRead) ->
	case gen_tcp:recv(Socket, BytesToRead) of
		{ok, Packet} ->
			<<Bytes/binary, Packet/binary>>;
		_N ->
			error
	end.

%%
%% Recibe una cantidad de bytes para leer y los agrega a los bytes
%% Si esa cantidad es 0 simplemente retorna los bytes
%%
getValue(Socket, Bytes, BytesToRead) ->
	case BytesToRead of
		0 ->
			Bytes;
		_N ->
			readMore(Socket, Bytes, BytesToRead)
	end.

%%
%% Parsea un argumento que tiene la longitud y el valor en binario
%%
%% Si los bytes no alcanzan para armar un entero de 4 bytes, lee mas
%%
%% Si no, interpreta los primeros 4 bytes como la longitud, e intenta
%% leer y retornar esa cantidad de bytes
%%
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

%%
%% Dados bytes de respuesta del servidor los interpreta
%%
decode(Socket, Bytes) ->
	case Bytes of
		<<101, Rest/binary>> -> 
			parseArgument(Socket, Rest);
		<<112>> ->
			enotfound;
		true ->
			error
	end.

%%
%% Lee del socket para recibir la respuesta del servidor
%% en caso de error, cierra la conexion.
%% Retorna {ok, valor en binario}
%%
recvAnswer(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			decode(Socket, Packet);
		{error, _Reason} ->
			gen_tcp:shutdown(Socket, read_write),
			error
	end.

%%
%% Lee un byte del socket e interpreta el codigo
%%
recvCode(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, <<101>>} -> ok;
		{ok, <<112>>} -> enotfound;
		true -> error
	end.

%%
%% Transforma los términos a binarios y guarda esos valores en la cache.
%% Luego para obtenerlos se hace el proceso inverso.
%%
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