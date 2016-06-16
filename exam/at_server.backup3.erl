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

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    {ok, spawn(fun() -> process_flag(trap_exit, true), stateloop(State, dict:new()) end)}. %Returns {ok, AT}

stop(AT) -> rpc(AT, stop). %Returns {ok, State}

doquery(AT, Fun) -> %Do a querry, not depending on a transaction (no update inteded)
    rpc(AT, {query, Fun}). %Synchronous to block

% Returns a reference
begin_t(AT) ->
    rpc(AT, makeRef).

query_t(AT, Ref, Fun) -> %Do a non-updating query on the current state of transaction ref.
    rpc(AT, {query, Fun, Ref}). %Synchronous to block

update_t(AT, Ref, Fun) -> 
    info(AT, {updateState, Fun, Ref}). %Asynchronous to not block

commit_t(AT, Ref) -> 
    rpc(AT, {commitState, Ref}).


%Server loop for the current state of the server, along with the current transactions

stateloop(State, Trans) ->
    Self = self(),
    receive
        {From, makeRef} -> %make intermediate copy of state with new ref.
            Ref = make_ref(),
            Pid = spawn_link(transactionHelper(Self, State)),
            reply_ok(From, Ref),
            stateloop(State, dict:append(Ref, Pid,Trans));
        {From, {query, Fun}} -> %query on state of server
            case calcQuery(State, Fun) of
                {ok, Result} -> reply_ok(From, Result);
                _ -> reply_error(From)
            end;
        {From, {query, Fun, Ref}} -> %query on state of transaction
            case dict:find(Ref, Trans) of
                {ok, Pid} -> info(Pid, {query, Fun, From}); %Asynchronous
                _ -> reply_abort(From)
            end;
        {updateState, Fun, Ref} -> %Update a current state of a transaction
            case dict:find(Ref, Trans) of
                {ok, Pid} -> info(Pid, {update, Fun});
                _ -> stateloop(State, Trans)
            end;
        {From, {commitState, Ref}} -> %Commit a transaction and abort all other
            case dict:find(Ref, Trans) of
                {ok, Pid} ->
                    case rpc(Pid, getState) of
                        {_, {ok, NewState}} -> 
                            reply_ok(From),
                            dict:map(fun(_,P) -> exit(P, aborted) end, Trans), %Kill all transaction subprocesses
                            stateloop(NewState, []); %Set new state and abort all other transactions.;
                        _ -> 
                            reply_abort(From), %Something went wrong. Remove transaction.
                            stateloop(State, dict:erase(Ref, Trans))
                    end;
                _ -> 
                    reply_abort(From),
                    stateloop(State, Trans)
            end;
        {From, stop} ->
            reply_ok(From, State),
            exit(terminated); %Kill self and all (linked) subprocesses, that is all transaction processes
        {From, _} -> 
            reply_error(From);
        {'EXIT', From, _} -> %Subprocess has died (from crash, abort, etc.). Remove it from active transactions
            stateloop(State, dict:erase(From, Trans));
        _ -> %bad call, keep looping
            stateloop(State, Trans)
    end,
    stateloop(State, Trans). %Needed because not all recieve-cases loops themselves

% Subprocesses for each transaction of stateloop. 
% Can only be manipulated by its parrent.
transactionHelper(Server, State) -> 
    receive
        {Server, getState} -> reply_ok(Server, State);
        {query, Fun, From} ->
            case calcQuery(State, Fun) of
                {ok, Result} -> reply_ok(From, Result);
                _ -> 
                    reply_abort(From),
                    exit(aborted)
            end;
        {update, Fun} ->
            try Fun(State) of 
                NewState -> transactionHelper(Server, NewState)
            catch
                _ : _ -> exit(aborted) %Case of error, abort and tell server                    
            end;
        {Server, aborted} -> exit(normal); %No need for server to catch this as server aborted me
        _ -> transactionHelper(Server, State)
    end.
    

calcQuery(State, Fun) ->
    try 
        Result = Fun(State),
        {ok, Result}
    catch
        _ : _ -> error
    end.


%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
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
    Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Your implementation of the atomic transaction server.

