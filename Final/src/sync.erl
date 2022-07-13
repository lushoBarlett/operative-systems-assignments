-module(sync).
-export([createLock/0, lock/1, unlock/1, destroyLock/1, waitForUnlock/0, lockingLoop/0]).

waitForUnlock() ->
	receive
		unlock -> ok
	end.

% representa la estructura de un lock
lockingLoop() ->
	receive
		{lock, PID} -> 
			PID ! run,	
			waitForUnlock(),
			lockingLoop();
		destroy -> ok
	end.

createLock () ->
	spawn(?MODULE, lockingLoop, []).

% avisa al lock que esta en espera, y espera a que le manden el mensaje run
lock (L) ->
	L ! {lock, self()},
	receive
		run -> ok
	end.

unlock (L) ->
	L ! unlock.

destroyLock (L) ->
	L ! destroy.
