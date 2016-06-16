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

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

-import(at_server, [start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

%call update_t/query_t with a errorness function, that is, a function throwing an error.
abort(AT, Ref) -> 
    Fun = fun() -> throw(error) end,
    update_t(AT, Ref, Fun). 

tryUpdate(AT, Fun) -> 
    Ref = begin_t(AT),
    case query_t(AT, Ref, Fun) of
        {ok, _} ->
            update_t(AT, Ref, Fun),
            commit_t(AT, Ref); %Returns ok if successfull, else aborted
        _ -> error %Return error if function fails
    end.

ensureUpdate(AT, Fun) ->
    case tryUpdate(AT, Fun) of
        aborted -> ensureUpdate(AT, Fun); %If the update is aborted, try again.
        Msg -> Msg %Can be only ok or error.
    end.

choiceUpdate(AT, Fun, Val_list) -> 
    Fun_list = lists:map(fun(e) -> (fun(s) -> Fun(s, e) end) end, Val_list),
    Self = self(),
    %Start all functions parallel
    lists:map(
        fun(e) -> 
            spawn(fun() -> choiceUpdateSpawn(Self, AT, e)) end) 
        end, Fun_list),
    choiceUpdateCheck(AT, Fun, Val_list, 0).

% Waits for all spawned functions of choiceUpdate to 
choiceUpdateCheck(AT, Fun, Val_list, Count)
    if 
        Count >= lenght(Val_list) -> 
            error;
        true -> % else branch
            receive
                {ok, Ref} -> commit_t(AT, Ref);
                _ -> choiceUpdateCheck(AT, Fun, Val_list, Count+1)
            end
    end.

% For subprocesses of choiceUpdate
% Begins a new 
choiceUpdateSpawn(Parrent, AT, Fun) ->
    Ref = begin_t(AT),
    update_t(AT, Ref, Fun), %Try to do the update
    DummyFun = fun(a) -> a end, %Dummy function
    case query_t(AT, Ref, DummyFun) of %Check if update is successfull
        {ok, _} -> info(Parrent, {ok, Ref});
        _ -> info(Parrent, error) %Return error if function fails
    end.


%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

info(Pid, Msg) ->
    Pid ! Msg.