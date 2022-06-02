-module(tempo).
-export([wait/1, contarAvisar/2, cronometro/2, cronometro_iniciar/3, f/0]).

wait(N) ->
	receive after N -> ok end.

contarAvisar(Hasta, PID) ->
	wait(Hasta),
	PID ! cortar.

f() -> io:fwrite("f~n").

cronometro(Fun, Periodo) ->
	receive
		cortar -> ok
	after Periodo ->
		Fun(),
		cronometro(Fun, Periodo)
	end.

cronometro_iniciar(Fun, Hasta, Periodo) ->
	Id = spawn(?MODULE, cronometro, [Fun, Periodo]),
	spawn(?MODULE, contarAvisar, [Hasta, Id]),
	ok.
