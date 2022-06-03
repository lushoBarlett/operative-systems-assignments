-module(sync).
-export([createLock/0, lock/1, unlock/1, destroyLock/1, lockingLoop/0]).
-export([createSem/1, semP/1, semV/1, destroySem/1, semLoop/1]).
-export([testLock/0, testSem/0]).

waitForUnlock() ->
	receive
		unlock -> ok
	end.

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

lock (L) ->
	L ! {lock, self()},
	receive
		run -> ok
	end.

unlock (L) ->
	L ! unlock.

destroyLock (L) ->
	L ! destroy.

waitForPost() ->
	receive
		post -> ok
	end.

waitingProcedure(Counter) ->
	if
		(Counter == 0) ->
			waitForPost(),
			NewCounter = Counter;
		true -> 
			NewCounter = Counter - 1
	end,
	NewCounter.

semLoop(Counter) ->
	receive
		post -> semLoop(Counter + 1);
		{wait, PID} ->
			NewCounter = waitingProcedure(Counter),
			PID ! run,
			semLoop(NewCounter);
		destroy -> ok
	end.

createSem (N) ->
	spawn(?MODULE, semLoop, [N]).

destroySem (S) ->
	S ! destroy.

semP (S) ->
	S ! {wait, self()},
	receive
		run -> ok
	end.

semV (S) ->
	S ! post.

f (L, W) ->
	lock(L),
	% regioncritica(),
	io:format("uno ~p~n", [self()]),
	io:format("dos ~p~n", [self()]),
	io:format("tre ~p~n", [self()]),
	io:format("cua ~p~n", [self()]),
	unlock(L),
	W ! finished.


waiter (L, 0) -> destroyLock(L);
waiter (L, N) -> receive finished -> waiter(L, N-1) end.
waiter_sem (S, 0) -> destroySem(S);
waiter_sem (S, N) -> receive finished -> waiter_sem(S, N-1) end.

testLock () ->
	L = createLock(),
	W = spawn(fun () -> waiter(L, 3) end),
	spawn (fun () -> f(L, W) end),
	spawn (fun () -> f(L, W) end),
	spawn (fun () -> f(L, W) end),
	ok.

sem (S, W) ->
	semP(S),
	%regioncritica(), bueno, casi....
	io:format("uno ~p~n", [self()]),
	io:format("dos ~p~n", [self()]),
	io:format("tre ~p~n", [self()]),
	io:format("cua ~p~n", [self()]),
	io:format("cin ~p~n", [self()]),
	io:format("sei ~p~n", [self()]),
	semV(S),
	W ! finished.

testSem () ->
	S = createSem(2), % a lo sumo dos usando io al mismo tiempo
	W = spawn (fun () -> waiter_sem (S, 5) end),
	spawn (fun () -> sem (S, W) end),
	spawn (fun () -> sem (S, W) end),
	spawn (fun () -> sem (S, W) end),
	spawn (fun () -> sem (S, W) end),
	spawn (fun () -> sem (S, W) end),
	ok.
