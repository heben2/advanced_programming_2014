%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name:
%%% Student KU-id:
%%%-------------------------------------------------------------------

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    {ok, spawn(fun() -> stateloop(State, []) end)}. %Returns {ok, AT}

stop(AT) -> rpc(AT, stop). %Returns {ok, State}

doquery(AT, Fun) -> %Do a querry, not depending on a transaction (no update inteded)
    case rpc(AT, getState) of %Synchronous to not block
        {ok, State} ->
            try 
                Result = Fun(State),
                {ok, Result}
            catch
                _ : _ -> error
            end;
        _ -> error
    end.

% Returns a reference
begin_t(AT) ->
    rpc(AT, makeRef).

query_t(AT, Ref, Fun) -> %Do a non-updating query on the current state of transaction ref.
    case rpc(AT, {getState, Ref}) of %Synchronous to block
        {ok, State} ->
            try 
                Result = Fun(State),
                {ok, Result}
            catch
                _ : _ -> aborted
            end;
        _ -> aborted
    end.

update_t(AT, Ref, Fun) -> 
    info(AT, {updateState, Fun, Ref}). %Asynchronous to not block

commit_t(AT, Ref) -> 
    rpc(AT, {commitState, Ref}).


%Server loop for the current state of the server, along with the current transactions

stateloop(State, Trans) ->
    receive
        {From, makeRef} -> %make intermediate copy of state with new ref.
            Ref = make_ref(),
            reply_ok(From, Ref),
            NewTrans = {Ref, State},
            stateloop(State, [ NewTrans | Trans]);
        {From, {getState, Ref}} -> %Get current state of transaction
            case lists:keyfind(Ref, 1, Trans) of
                false -> reply_abort(From);
                {Ref, CurrState} -> reply_ok(From, CurrState)
            end;
        {From, getState} -> %Get the current state of server
            reply_ok(From, State);
        {updateState, Fun, Ref} -> %Update a current state of a transaction
            case lists:keyfind(Ref, 1, Trans) of
                false -> stateloop(State, Trans);
                {Ref, CurrState} ->
                    try Fun(CurrState) of 
                        NewState -> stateloop(State, lists:keyreplace(Ref, 1, Trans, {Ref, NewState}))
                    catch
                        _ : _ -> stateloop(State, lists:keydelete(Ref, 1, Trans)) %Else abort the transaction by deleting it.
                    end
            end;
        {From, {commitState, Ref}} -> %Commit a transaction and abort all other
            case lists:keyfind(Ref, 1, Trans) of
                false -> 
                    reply_abort(From),
                    stateloop(State, Trans);
                {Ref, CurrState} ->
                    reply_ok(From),
                    stateloop(CurrState, []) %Set new state and abort all other transactions.
            end;
        {From, stop} -> %Automatically aborts all transactions by exiting
            reply_ok(From, State),
            exit(terminated); %Needed because of loop at end of function.
        {From, _} -> 
            reply_error(From);
        _ -> %bad call, keep looping
            stateloop(State, Trans)
    end,
    stateloop(State, Trans). %Needed because not all recieve-cases loops themselves

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

reply_abort(From) -> %Changed this to not take arguments... It don't use it
    reply(From, aborted).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Your implementation of the atomic transaction server.

