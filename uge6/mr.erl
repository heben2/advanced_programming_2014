%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Ken Friis Larsen, Michael Kirkedal Thomsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%% Added   : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------

%% SORRY ABOUT THE UGLY CODE...
%% YOU SHOULD DO BETTER THAN THIS.
%% Only for inspiration.
%% If you copy any code remember to write this explicitly.

-module(mr).

-export([start/1, stop/1, job/5]).

%%%% Interface

start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.

stop(Pid) -> rpc(Pid, stop).

job(CPid, MapFun, RedFun, RedInit, Data) -> 
    rpc(CPid, {MapFun, RedFun, RedInit, Data}).


%%%% Internal implementation

init(N) -> 
    Reducer = spawn(fun reducer_loop/0),
    Mappers = lists:map(
        fun(_) -> spawn(
            fun() -> mapper_loop(Reducer,fun(X)->X end) end) 
        end, lists:duplicate(N, 1)),
    {Reducer, Mappers}.


%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    info(Pid, stop).

data_async(Pid, D) ->
    info(Pid, {data, D}).

setup_async(Pid, D) ->
    info(Pid, {setup, D}).

setup_async_red(Pid, Fun, Acc, N) ->
    info(Pid, {self(), {Fun, Acc, N}}).

%%% Coordinator


coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);
    {From, {_MapFun, _RedFun, RedInit, []}} ->
        reply_ok(From, RedInit),
        coordinator_loop(Reducer, Mappers);
	{From, {MapFun, RedFun, RedInit, Data}} ->
        io:format("~p starting job~n", [self()]),
        lists:map(fun(X) -> setup_async(X,MapFun) end, Mappers),
        setup_async_red(Reducer, RedFun, RedInit, length(Data)),
        send_data(Mappers, Data),
        receive
            {Reducer, Result} -> Result
        end,
        reply_ok(From, Result),
        coordinator_loop(Reducer, Mappers)
    end.


send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).

%%% Mapper

mapper_loop(Reducer,MapFun) ->
    receive
    stop -> 
        io:format("Mapper ~p stopping~n", [self()]),
        ok;
    {setup, NewFun} ->
        mapper_loop(Reducer,NewFun);
    {data, Data} ->
        Result = MapFun(Data),
        data_async(Reducer, Result),
        mapper_loop(Reducer, MapFun);
    Unknown ->
        io:format("unknown message: ~p~n",[Unknown]), 
        mapper_loop(Reducer,MapFun)
    end.

%%% Reducer

reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
    {From, {RedFun, RedInit, Count}} ->
        io:format("Reducer ~p receive data~n", [self()]),
        Result = gather_data_from_mappers(RedFun,RedInit,Count),
        reply(From, Result),
        reducer_loop()
    end.

gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
        {data, Data} ->
            Result = Fun(Data, Acc),
            case Missing =:= 1 of
                true  -> Result;
                false -> gather_data_from_mappers(Fun, Result, Missing- 1)
            end
    end.
