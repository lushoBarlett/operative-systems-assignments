-module(broadcaster).

-export([start/0, stop/1, spawn_clients/2, message/2, broadcaster/1, client/1]).

broadcaster(Clients) ->
	receive
		{subscribe, Client} ->
			broadcaster([Client | Clients]);
		{message, Message} ->
			lists:foreach(fun(Client) -> Client ! Message end, Clients),
			broadcaster(Clients);
		{unsubscribe, Client} ->
			NewList = lists:filter(fun(C) -> C /= Client end, Clients),
			broadcaster(NewList);
		{stop} ->
			ok
	end.

client_subscribe(Broadcaster) ->
	Broadcaster ! {subscribe, self()}.

client_unsubscribe(Broadcaster) ->
	Broadcaster ! {unsubscribe, self()}.

message(Broadcaster, Message) ->
	Broadcaster ! {message, Message}.

client_receive_message() ->
	receive
		{normal, Body} ->
			io:fwrite("Recieved, ~s~n", [Body]),
			client_receive_message();
		{quit} ->
			ok
	end.

client(Broadcaster) ->
	client_subscribe(Broadcaster),
	client_receive_message(),
	client_unsubscribe(Broadcaster).

spawn_clients(_Broadcaster, 0) ->
	ok;
spawn_clients(Broadcaster, N) ->
	spawn(?MODULE, client, [Broadcaster]),
	spawn_clients(Broadcaster, N - 1).

start() ->
	spawn(?MODULE, broadcaster, [[]]).

stop(Broadcaster) ->
	Broadcaster ! {stop}.
