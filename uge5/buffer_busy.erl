%%
%% A simple move server
%%

-module(buffer_busy).
-export([start/0,put/2,get/1]).

%%% API

% Start and empty buffer
start() -> 
	spawn(fun() -> bufferloop([]) end).

% put(BufferID, Value)
put(Pid, Value) -> info(Pid, {put, Value}).

% get(Buffer, ID)
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

bufferloop(Buffer) ->
    receive
        {put, Value} ->
            bufferloop([Value | Buffer]);
    	{From, get} ->
            case Buffer of
                [] -> 
                    self() ! {From, get},
                    bufferloop([]);
                [Head | Tail] ->
                    reply(From, Head),
                    bufferloop(Tail)
            end            
	end.



















