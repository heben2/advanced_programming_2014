-module(spawner).
-export([start/0, test/1, exit/1]).


start() ->
	MyPid = self(),
	spawn(fun() -> init(1, MyPid) end).

test(Pid) ->
	rpc(Pid, test).

exit(Pid) ->
	Pid ! exit.

init(Int, Prev_Pid) when Int < 250000 ->	
    		io:format("~p is number ~p.~n", [self(),Int]),
	MyPid = self(),
	Next_Pid = spawn(fun() -> init(Int+1, MyPid) end),
	loop(Int, Prev_Pid, Next_Pid);
init(Int, Prev_Pid) ->
	io:format("~p is number ~p.~n", [self(),Int]),
	loop(Int, Prev_Pid, none).
		

loop(Int, Prev_Pid, none) ->
	receive	
		{From, Message} -> 
            io:format("End: ~p : ~p ~n",[self(), Message]),
			reply(From, {ok, Message}),
			loop(Int, Prev_Pid, none);
		exit -> ok
	end;
loop(Int, Prev_Pid, Next_Pid) when Int rem 10000 =:= 0 ->
	receive	
		{From, Message} -> 
           io:format("Got: ~p : ~p ~n",[self(), Message]),
			Reply = rpc(Next_Pid, Message),
            io:format("Reply: ~p : ~p ~n",[self(), Reply]),
			reply(From, Reply),
			loop(Int, Prev_Pid, Next_Pid);
		exit -> 
			Next_Pid ! exit,
			ok
	end;
loop(Int, Prev_Pid, Next_Pid) ->
	receive	
		{From, Message} -> 
			Reply = rpc(Next_Pid, Message),
			reply(From, Reply),
			loop(Int, Prev_Pid, Next_Pid);
		exit -> 
			Next_Pid ! exit,
			ok
	end.



rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(To, Response) ->
    To ! {self(), Response}.