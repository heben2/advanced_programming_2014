%%
%% A simple move server
%%

-module(buffer_lq).
-export([start/0,put/2,get/1]).


%%% API

% ordinary function definition, just here for reference

start() -> 
	spawn(fun() -> bufferloop([],[]) end).


put(Pid, Value) -> info(Pid, {put, Value}).

get(Pid) -> spawn(fun() -> 
                    Res = rpc(Pid, get),
                    io:format("Got: ~p~n",[Res])
                  end).

%%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
		{Pid, Response} -> Response
    end.

reply(To, Response) ->
    To ! {self(), Response}.

info(Pid, Request) ->
    Pid ! Request.

%%% Internal implementation
 bufferloop(Buffer, Gets) ->
    receive
        {put, Value} ->
            case Gets of
                [] -> 
                    bufferloop([Value | Buffer],[]);
                [Head | Tail] ->
                    reply(Head, Value),
                    bufferloop(Buffer, Tail)
            end;
        {From, get} ->
            case Buffer of
                [] -> 
                    bufferloop([], [From | Gets]);
                [Head | Tail] ->
                    reply(From, Head),
                    bufferloop(Tail, Gets)
            end
    end.
