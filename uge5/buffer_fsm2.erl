%%
%% A simple move server
%%

-module(buffer_fsm2).
-export([start/0,put/2,get/1]).


%%% API

% ordinary function definition, just here for reference

start() -> 
    spawn(fun() -> bufferloop([]) end).


put(Pid, Value) -> info(Pid, {put, Value}).

get(Pid) -> spawn(fun() -> 
                    Res = rpc(Pid, get),
                    io:format("Got: ~p~n",[Res])
                  end).

% get(Pid) -> 
%     rpc(Pid, get).


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

bufferloop([]) ->
    receive
        {put, Value} ->
            bufferloop([Value])
    end;
bufferloop(Buffer) ->
    receive
        {put, Value} ->
            bufferloop([Value | Buffer]);
        {From, get} ->
            [Head | Tail] = Buffer,
            reply(From, Head),
            bufferloop(Tail)
    end.







