-module(hello).

-export([init/0, hello/0]).

wait(Miliseconds) ->
	receive
		after Miliseconds -> ok
	end.

randfail() ->
	case rand:uniform(10) of
		10 -> 1 / uno;
		_ -> self()
	end.

hello() ->
	wait(1000),
	io:fwrite("Hello ~p~n", [randfail()]),
	?MODULE:hello().

parent() ->
	spawn_link(fun () -> hello() end),
	process_flag(trap_exit, true),
	receive
		{'EXIT', _Pid, _Reason} ->
			parent()
	end.

init() ->
	spawn(fun () -> parent() end).