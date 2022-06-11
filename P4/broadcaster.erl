-module(broadcaster).

-export([start/0, stop/0, spawn_clients/1, message/1, client/0, broadcaster/1]).

broadcaster(Clients) ->
	receive
		{subscribe, Client} ->
			broadcaster([Client | Clients]);
		{message, Message} ->
			lists:foreach(fun(Client) -> Client ! {normal, Message} end, Clients),
			broadcaster(Clients);
		{unsubscribe, Client} ->
			NewList = lists:filter(fun(C) -> C /= Client end, Clients),
			broadcaster(NewList);
		{stop} ->
			ok
	end.

client_subscribe() ->
	broadcaster ! {subscribe, self()}.

client_unsubscribe() ->
	broadcaster ! {unsubscribe, self()}.

message(Message) ->
	broadcaster ! {message, Message}.

client_receive_message() ->
	receive
		{normal, Body} ->
			io:fwrite("Recieved, ~s~n", [Body]),
			client_receive_message()
	end.

client() ->
	client_subscribe(),
	client_receive_message(),
	client_unsubscribe().

spawn_clients(0) ->
	ok;
spawn_clients(N) ->
	spawn(?MODULE, client, []),
	spawn_clients(N - 1).

start() ->
	Broadcaster = spawn(?MODULE, broadcaster, [[]]),
	register(broadcaster, Broadcaster),
	ok.

stop() ->
	broadcaster ! {stop},
	unregister(broadcaster),
	ok.
