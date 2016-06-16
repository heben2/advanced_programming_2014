%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Henrik Bendt
%%% Student KU-id: gwk553
%%%-------------------------------------------------------------------

-module(p).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    io:format("start_server\n"),
    {ok, spawn(fun() -> process_flag(trap_exit, true), stateloop(State, dict:new(), dict:new()) end)}. %Returns {ok, AT}

stop(AT) -> rpc(AT, stop). %Returns {ok, State}

doquery(AT, Fun) -> %Do a querry, not depending on a transaction (no update inteded)
    rpc(AT, {query, Fun}). %Synchronous to block

% Returns a reference
begin_t(AT) ->
    rpc(AT, makeRef).

query_t(AT, Ref, Fun) -> %Do a non-updating query on the current state of transaction ref.
    rpc(AT, {query, Fun, Ref}). %Synchronous to block

update_t(AT, Ref, Fun) -> 
    io:format("update_t\n"),
    info(AT, {updateState, Fun, Ref}). %Asynchronous to not block

commit_t(AT, Ref) -> 
    rpc(AT, {commitState, Ref}).


%Server loop for the current state of the server, along with the current transactions

stateloop(State, Trans, Waiting) ->
    io:format("Stateloop\n"),
    Self = self(),
    receive
        {From, makeRef} -> %make intermediate copy of state with new ref.
            io:format("makeRef\n"),
            Ref = make_ref(),
            Pid = spawn_link(fun() -> transactionHelper(Self, State, Ref) end),
            io:format("Pid of spawned: ~w\n",[Pid]),
            reply_ok(From, Ref),
            stateloop(State, dict:store(Ref, Pid, Trans), Waiting);
        {From, {query, Fun}} -> %query on state of server
            io:format("query1\n"),
            case calcQuery(State, Fun) of
                {ok, Result} -> reply_ok(From, Result);
                _ -> reply_error(From)
            end;
        {From, {query, Fun, Ref}} -> %query on state of transaction
            io:format("query2, Ref: ~w\n", [Ref]),
            io:format("currentdict: ~w\n",[Trans]),
            case dict:find(Ref, Trans) of
                {ok, Pid} -> 
                    io:format("Found Pid: ~p with Ref: ~w\n",[Pid, Ref]),
                    info(Pid, {Self, query, Fun}),
                    stateloop(State, Trans, dict:store(Ref, From, Waiting));
                _ -> 
                io:format("REF NOT FOUND IN DICT\n"),
                reply_abort(From)
            end;
        {queryAns, Result, Ref} ->
            io:format("queryAns\n"),
            reply_ok(dict:fetch(Ref, Waiting), Result),
            stateloop(State, Trans, dict:erase(Ref,Waiting));
        {updateState, Fun, Ref} -> %Update a current state of a transaction
            io:format("updateState\n"),
            case dict:find(Ref, Trans) of
                {ok, Pid} -> 
                    io:format("Found Pid: ~p with Ref: ~w\n",[Pid, Ref]),
                    info(Pid, {Self, update, Fun});
                _ -> 
                    io:format("errorUpdateState, Ref not found."),
                    stateloop(State, Trans, Waiting)
            end;
        {From, {commitState, Ref}} -> %Commit a transaction and abort all other
            io:format("commitState\n"),
            case dict:find(Ref, Trans) of
                {ok, Pid} ->
                    case rpcE(Pid, getState) of
                        {ok, NewState} -> 
                            reply_ok(From),
                            dict:map(fun(_,P) -> reply_abort(P) end,
                                dict:erase(Ref,Waiting)), %Abort all blocking processes
                            dict:map(fun(_,P) -> exit(P, aborted) end, Trans), %Kill all transaction subprocesses
                            stateloop(NewState, dict:new(), dict:new()); %Set new state and abort all other transactions.;
                        _ -> 
                            reply_abort(From), %Something went wrong. Remove transaction.
                            stateloop(State, dict:erase(Ref, Trans), Waiting)
                    end;
                _ -> 
                    reply_abort(From),
                    stateloop(State, Trans, Waiting)
            end;
        {From, stop} ->
            io:format("Stop\n"),
            reply_ok(From, State),
            dict:map(fun(_,P) -> reply_abort(P) end, Waiting), %Abort all waiting processes
            exit(terminated); %Kill self and all (linked) subprocesses, that is all transaction processes
        {From, _} -> 
            io:format("badArg\n"),
            reply_error(From);
        {'EXIT', _, {aborted, Ref}} -> %Subprocess has aborted. Tell waiting processes
            io:format("subprocessaborted\n"),
            case dict:find(Ref, Waiting) of
                {ok, Pid} -> 
                    reply_abort(Pid),
                    stateloop(State, dict:erase(Ref, Trans), dict:erase(Ref, Waiting));
                _ -> stateloop(State, dict:erase(Ref, Trans), Waiting)
            end;
        _ -> %bad call, keep looping. Also catches normal exits of transactionHelpers
            io:format("Bad Call\n"),
            stateloop(State, Trans, Waiting)
    end,
    stateloop(State, Trans, Waiting). %Needed because not all recieve-cases loops themselves

% Subprocesses for each transaction of stateloop. 
% Can only be manipulated by its parrent, the server.
transactionHelper(Server, State, Ref) -> 
    io:format("transactionHelper, pId: ~p\n", [self()]),
    receive
        {Server, getState} -> 
            io:format("TgetState\n"),
            reply_ok(Server, State);
        {Server, query, Fun} ->
            io:format("Tquery\n"),
            case calcQuery(State, Fun) of
                {ok, Result} -> 
                    io:format("TqueryOk\n"),
                    info(Server, {queryAns, Result, Ref}),
                    transactionHelper(Server, State, Ref);
                _ -> 
                    io:format("TqueryAborted\n"),
                    exit({aborted, Ref})
            end;
        {Server, update, Fun} ->
            io:format("Tupdate\n"),
            try Fun(State) of 
                NewState -> 
                    io:format("Tupdate Ok\n"),
                    transactionHelper(Server, NewState, Ref)
            catch
                _ : _ -> 
                    io:format("TupdateAborted\n"),
                    exit({aborted, Ref}) %Case of error, abort and tell server                    
            end;
        {Server, aborted} ->
            io:format("Taborted\n"),
            exit(normal); %No need for server to catch this as server aborted me
        _ -> 
            io:format("TBadCall\n"),
            transactionHelper(Server, State, Ref)
    end.
    

calcQuery(State, Fun) ->
    io:format("calcQuery\n"),
    try 
        Result = Fun(State),
        {ok, Result}
    catch
        _ : _ -> 
            io:format("Cerror\n"),
            error
    end.


%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

%To also recieve exit messages
rpcE(Pid, Request) ->
io:format("rpcE, Pid: ~p Request:~w\n",[Pid, Request]),
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response;
        {'EXIT', Pid, _} -> aborted
    end.

rpc(Pid, Request) ->
    io:format("rpc, Pid: ~p Request:~w\n",[Pid, Request]),
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

reply(From,  Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

reply_error(From) ->
    reply(From, error).

reply_abort(From) -> %Changed this to only take From
    reply(From, aborted).

%% asynchronous communication

info(Pid, Msg) ->
    io:format("info, Pid: ~p Request:~w\n",[Pid, Msg]),
    Pid ! Msg.


%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Your implementation of the atomic transaction server.

