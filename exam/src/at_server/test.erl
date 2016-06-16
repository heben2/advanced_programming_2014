-module(test).

-include_lib("eunit/include/eunit.hrl").
-import(at_server,[start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).
-import(at_extapi,[abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).


%%% Test start and stop %%%
start_stop1_test() -> ?assertEqual(prop_start_stop(0), {ok,0}).
start_stop2_test() -> ?assertEqual(prop_start_stop(atom), {ok,atom}).

%
prop_start_stop(State) -> 
    {ok, Pid} = start(State),
    stop(Pid).

%%% Test doquery %%%
doquery1_test() -> ?assertEqual(prop_query1(0), {ok,1}).
doquery2_test() -> ?assertEqual(prop_query2(a), {ok,a}).
doquery_error_test() -> ?assertEqual(prop_query_error1(0), error).

%
prop_query1(State) -> 
    {ok, Pid} = start(State),
    doquery(Pid, fun(X) -> X+1 end).

prop_query2(State) -> 
    {ok, Pid} = start(State),
    doquery(Pid, fun(X) -> X end).

prop_query_error1(State) -> 
    {ok, Pid} = start(State),
    doquery(Pid, fun() -> throw(error) end).

%%% Test doquery and stop (not touching state) %%%
doquery_stop1_test_() -> %Does not crash after multiple calls
        {ok, AT} = start(0),
        doquery_stop1_gen(10000, AT).
doquery_stop2_test() -> %Not touching state
        {ok, AT} = start(0),
        ?assertEqual(doquery_stop2_loop(10000, AT),{ok, 0}).
doquery_stop3_test() -> %Errorfull queries do not touch the state
        {ok, AT} = start(0),
        ?assertEqual(doquery_stop3_loop(10000, AT), {ok, 0}). 

%
doquery_stop1_gen(N, AT) ->
   {generator,
    fun () ->
        if N > 0 ->
               Fun = doquery(AT, fun(X) -> X+1 end),
               [?_assertEqual(Fun, {ok,1})
                | doquery_stop1_gen(N-1, AT)];
           true ->
               []
        end
    end}.

doquery_stop2_loop(N, AT) ->
    if N > 0 ->
           doquery(AT, fun(X) -> X+1 end),
           doquery_stop2_loop(N-1, AT);
       true ->
           stop(AT)
    end.

doquery_stop3_loop(N, AT) ->
    if N > 0 ->
           doquery(AT, fun() -> throw(error) end),
           doquery_stop2_loop(N-1, AT);
       true ->
           stop(AT)
    end.

%%% Test begin_t and uniqueness of returned references %%%
begin_t2_test_() ->
    {ok, AT} = start(0),
    begin_t2_gen(10000, AT, []).

%
begin_t2_gen(N, AT, Refs) ->
       {generator,
        fun () ->
            if N > 0 ->
                    {ok, Ref} = begin_t(AT),
                    NewRefs = [Ref | Refs],
                    [?_assertNot(lists:member(Ref,Refs))
                     | begin_t2_gen(N-1, AT, NewRefs)];
               true ->
                   []
            end
        end}.


%%% Test query_t. Assumes begin_t works for these cases. %%%
query_t_test_() -> 
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    fun(AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assert(prop_query_t1(AT) =:= {ok,1}), %Test return {ok, result}
     ?_assertEqual(prop_query_t_error(AT), aborted), %Test return aborted on function error
     ?_assertEqual(prop_query_t2(AT), {ok,0}), %Test server state is not touched
     ?_assertEqual(prop_query_t_aborted(AT), aborted)] %Test if error function aborts transaction
    end}.

query_t2_test_() -> %Tests if transaction state is touched by query_t
    {ok, AT} = start(0),
    {ok, Ref} = begin_t(AT),
    prop_query_t2_gen(10000, AT, Ref).

%
prop_query_t1(AT) -> 
    {ok, Ref} = begin_t(AT),
    query_t(AT, Ref, fun(X) -> X+1 end).

prop_query_t_error(AT) -> 
    {ok, Ref} = begin_t(AT),
    query_t(AT, Ref, fun() -> throw(error) end).

prop_query_t2(AT) -> 
    {ok, Ref} = begin_t(AT),
    prop_query_t_loop(10000, AT, Ref).

prop_query_t_loop(N, AT, Ref) -> %Should return same result as always
    if N > 0 ->
           query_t(AT, Ref, fun(X) -> X+1 end),
           prop_query_t_loop(N-1, AT, Ref);
       true ->
           doquery(AT, fun(X) -> X end) 
    end.

prop_query_t2_gen(N, AT, Ref) -> %Server state should not be touched by query_t
   {generator,
    fun () ->
        if N > 0 ->
               Fun = query_t(AT, Ref, fun(X) -> X+1 end),
               [?_assertEqual(Fun, {ok,1})
                | prop_query_t2_gen(N-1, AT, Ref)];
           true ->
                exit(AT, kill),
                []
        end
    end}.

prop_query_t_aborted(AT) ->
    {ok, Ref} = begin_t(AT),
    query_t(AT, Ref, fun() -> throw(error) end),
    query_t(AT, Ref, fun(X) -> X+1 end).


%%% Test update_t %%%
update_t_test_() -> 
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    %fun () -> {ok, AT} = start(0) end,
    fun (AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assertEqual(prop_update_t(AT), {ok,1}), %Test transaction state is updated
     ?_assertEqual(prop_update_t_aborted(AT), aborted)] %Test transaction is aborted with failed update.
    end}.

update_t2_test_() -> %Test state is continous updated
    {ok, AT} = start(0),
    {ok, Ref} = begin_t(AT),
    prop_update_t_gen(1, AT, Ref).    

update_t3_test() -> %Server state should not be touched by update_t
    {ok, AT} = start(0),
    {ok, Ref} = begin_t(AT),
    prop_update_t_loop(100, AT, Ref),
    ?_assertEqual(stop(AT), {ok,0}).

%
prop_update_t(AT) ->
    {ok, Ref} = begin_t(AT),
    update_t(AT, Ref, fun(X) -> X+1 end),
    query_t(AT, Ref, fun(X) -> X end).


prop_update_t_aborted(AT) ->
    {ok, Ref} = begin_t(AT),
    update_t(AT, Ref, fun() -> throw(error) end),
    query_t(AT, Ref, fun(X) -> X+1 end).


prop_update_t_gen(N, AT, Ref) -> 
   {generator,
    fun () ->
        if N < 10000 ->
                update_t(AT, Ref, fun(X) -> X+1 end),
                Fun = query_t(AT, Ref, fun(X) -> X end), %Dummy function
                [?_assertEqual(Fun, {ok, N})
                 | prop_update_t_gen(N+1, AT, Ref)];
           true ->
                exit(AT, kill),
                []
        end
    end}.

prop_update_t_loop(N, AT, Ref) ->
    if N > 0 ->
           update_t(AT, Ref, fun(X) -> X+1 end),
           prop_update_t_loop(N-1, AT, Ref);
       true -> ok
    end.


%%% Test commit_t. Assumes update_t works %%%
commit_t_test_() -> 
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    %fun () -> {ok, AT} = start(0) end,
    fun (AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assertEqual(prop_commit_t1(AT), ok), %Test commit result
     ?_assertEqual(prop_commit_t2(AT), aborted), %Test commit result
     ?_assertEqual(prop_commit_t3(AT, a), {ok, a}) %Test if state is commited
    ]
    end}.

commit_t2_test_() -> %Tests all trans are aborted when commit happens
    {ok, AT} = start(0),
    Refs = prop_commit_t4(AT),
    prop_commit_t4_gen(AT, Refs).

%

prop_commit_t1(AT) ->
    {ok, Ref} = begin_t(AT),
    update_t(AT, Ref, fun(X) -> X+1 end),
    commit_t(AT,Ref).

prop_commit_t2(AT) ->
    {ok, Ref} = begin_t(AT),
    update_t(AT, Ref, fun() -> throw(error) end),
    commit_t(AT, Ref).

prop_commit_t3(AT, Val) ->
    {ok, Ref} = begin_t(AT),
    update_t(AT, Ref, fun(_) -> Val end),
    commit_t(AT,Ref),
    doquery(AT, fun(X) -> X end). %Read current server state

prop_commit_t4(AT) ->
    {ok, Ref} =  begin_t(AT),
    update_t(AT, Ref, fun(X) -> X+1 end),
    Refs = prop_commit_t4_loop(30, AT, []),
    [Ref| Refs].

prop_commit_t4_loop(N, AT, Refs) -> %Creates a lot of transactions
    if N > 0 ->
           {ok, Ref} =  begin_t(AT),
           prop_query_t_loop(N-1, AT, [Ref | Refs]);
       true ->
           Refs
    end.

prop_commit_t4_gen(AT, References) -> %Server state should not be touched by query_t
   {generator,
    fun () ->
        if length(References) > 0 ->
               [Ref | Refs] = References,
               Fun = query_t(AT, Ref, fun(X) -> X end), %Dummy function
               [?_assertEqual(Fun, aborted)
                | prop_commit_t4_gen(AT, Refs)];
           true ->
                []
        end
    end}.


%%%%%%%%%%%%% at_extrapi %%%%%%%%%%

%%% Test abort %%%
abort_test() -> %Test if a given transaction is aborted
    {ok, AT} = start(0),
    {ok, Ref} =  begin_t(AT),
    abort(AT, Ref),
    Fun = query_t(AT, Ref, fun(X) -> X end), %Dummy function
    ?assertEqual(Fun, aborted).

%%% Test tryUpdate %%%
tryUpdate_test_() ->
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    %fun () -> {ok, AT} = start(0) end,
    fun (AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assertEqual(prop_tryUpdate(AT), ok), %Test result if successfull
     ?_assertEqual(prop_tryUpdate_error(AT), error), %Test result if error function
     {timeout, 10,?_assertEqual(prop_tryUpdate_aborted(AT), aborted)}, %Test aborted
     {timeout, 10,?_assertEqual(prop_tryUpdate_aborted2(AT), {ok, a})}, %Test that the server state was actually changed
     ?_assertEqual(prop_tryUpdate_val(AT), {ok,1}) %Test if value is updated
    ]
    end}.

%
prop_tryUpdate(AT) ->
    tryUpdate(AT, fun(_) -> 1 end).

prop_tryUpdate_error(AT) ->
    tryUpdate(AT, fun() -> throw(error) end).

prop_tryUpdate_aborted(AT) ->
    spawn(fun() -> prop_tryUpdate_aborted_spawn(AT) end), %spawn competitor
    tryUpdate(AT, fun(_) -> timer:sleep(2000), b end). %sleep 2 seconds
    

prop_tryUpdate_aborted_spawn(AT) ->
    {ok, Ref} =  begin_t(AT),
    timer:sleep(2000), %Sleep a little so tryUpdate gets ahead (but has to run 2 times with function).
    update_t(AT, Ref, fun(_) -> a end),
    commit_t(AT, Ref),
    exit(normal). %kill self

prop_tryUpdate_aborted2(AT) ->
    prop_tryUpdate_aborted(AT),
    doquery(AT, fun(X) -> X end).

prop_tryUpdate_val(AT) ->
    prop_tryUpdate(AT),
    doquery(AT, fun(X) -> X end).


%%% Test ensureUpdate %%%
ensureUpdate_test_() ->
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    %fun () -> {ok, AT} = start(0) end,
    fun (AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assertEqual(prop_ensureUpdate(AT), ok), %Test result if successfull
     ?_assertEqual(prop_ensureUpdate_error(AT), error), %Test result on error function
     ?_assertEqual(prop_ensureUpdate_val(AT), {ok, b}), %Test update actually happens
     {timeout, 10,?_assertEqual(prop_ensureUpdate_aborted(AT), ok)}, %Test update is ensure by trying again first time another commit happens.
     ?_assertEqual(prop_ensureUpdate_read(AT), {ok,c}) %Test if value is updated
    ]
    end}.

%
prop_ensureUpdate(AT) ->
    ensureUpdate(AT, fun(_) -> a end).

prop_ensureUpdate_error(AT) ->
    ensureUpdate(AT, fun() -> throw(error) end).

prop_ensureUpdate_val(AT) ->
    ensureUpdate(AT, fun(_) -> b end),
    doquery(AT, fun(X) -> X end).

prop_ensureUpdate_aborted(AT) ->
    spawn(fun() -> prop_ensureUpdate_aborted_spawn(AT) end), %spawn competitor
    ensureUpdate(AT, fun(_) -> timer:sleep(2000), c end). %sleep 2 seconds
    
prop_ensureUpdate_aborted_spawn(AT) ->
    {ok, Ref} =  begin_t(AT),
    timer:sleep(2000), %Sleep a little so tryUpdate gets ahead.
    update_t(AT, Ref, fun(_) -> 1 end),
    commit_t(AT, Ref),
    exit(normal). %kill self

prop_ensureUpdate_read(AT) ->
    doquery(AT, fun(X) -> X end).


%%% Test choiceUpdate %%%
choiceUpdate_test_() ->
    {setup,
    fun() -> {ok, AT} = start(0), AT end,
    %fun () -> {ok, AT} = start(0) end,
    fun (AT) -> exit(AT, kill) end,
    fun(AT) ->
    [?_assertEqual(prop_choiceUpdate_multi(AT), ok), %Return value on success
     ?_assertEqual(prop_choiceUpdate_read(AT), {ok, c}), %Is the value actually updated to the fastest one?
     ?_assertEqual(prop_choiceUpdate_multi2(AT), ok), %Try again with another list
     ?_assertEqual(prop_choiceUpdate_read(AT), {ok, b})
    ]
    end}.

%

prop_choiceUpdate_multi(AT) ->
    List = [{a,1000}, {b,2000}, {c,0}, {d, 900}],
    choiceUpdate(AT, fun(_, {Y,T}) -> timer:sleep(T), Y end, List).

prop_choiceUpdate_multi2(AT) ->
    List = [{a,3000}, {b,100}, {c,4000}, {d, 900}],
    choiceUpdate(AT, fun(_, {Y,T}) -> timer:sleep(T), Y end, List).

prop_choiceUpdate_read(AT) ->
    doquery(AT, fun(X) -> X end).
