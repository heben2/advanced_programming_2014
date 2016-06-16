-module(spawner2).
-export([start/1]).


start(Int) ->
	Pid = spawn(fun() -> loop(Int) end),
	try link(Pid) of
	 	_ ->
	 		start(Int + 1)
	 catch
	 	_ -> stopped
	 end.


loop(Int) ->
    io:format("~p is number ~p.~n", [self(),Int]),
	receive	
		_ -> 
			loop(Int)
	end.