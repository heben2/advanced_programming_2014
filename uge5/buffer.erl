%%
%% A simple move server
%%

-module(buffer).
-export([start/0,put/2,get/1]).


%%% API

% Start and empty buffer
start() -> 
	spawn(fun buffer_init/0).

% put(BufferID, Value)
put(Pid, Value) -> info(Pid, {put, Value}).

% get(BufferID)
get(Pid) -> rpc(Pid, get).

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
buffer_init() ->
    Initial_buffer = [], 
    bufferloop(Initial_buffer).

bufferloop(Buffer) ->
    receive
        {put, Value} ->
            bufferloop([Value | Buffer]);
        {From, get} ->
            [Head | Tail] = Buffer,
            reply(From, Head),
            bufferloop(Tail)
    end.


















