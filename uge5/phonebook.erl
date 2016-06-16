%%
%% Basic concurrent phone-book
%%

-module(phonebook).
-export([start/0, add/2, list_all/1, update/2, fetch/2, pbupdate/2, loop/1, emptymq/0]).

%% Interface

start() -> 
    Pid = spawn(fun() -> loop(dict:new()) end),
    add(Pid, {joe, 167}),
    add(Pid, {robert, 168}),
    add(Pid, {mike, 165}),
    Pid.

add(Pid, Contact) -> rpc(Pid,{add, Contact}).

fetch(Pid, Name) -> rpc(Pid,{fetch, Name}).

list_all(Pid) -> rpc(Pid, list_all).

update(Pid, Contact) -> rpc(Pid, {update, Contact}).

%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    after 1000 ->
        {fail, no_answer}
    end.

loop(Contacts) ->
    receive
        {From, {add, Contact}} ->
            {Name, _} = Contact,
            case dict:is_key(Name, Contacts) of
                true ->
                    From ! {self(), {error, Name, is_in_dict}},
                    loop(Contacts);
                false -> 
                    From ! {self(), {Name, is_added}},
                    loop(dict:store(Name, Contact, Contacts))
            end;
% I have added a fetch
        {From, {fetch, Name}} ->
            try dict:fetch(Name, Contacts) of
                Element ->
                    From ! {self(), Element}
            catch
                _:_ ->
                    From ! {self(), {not_in_phonebook, Name}}
            after
                loop(Contacts)
            end;
        {From, list_all} ->
            List = dict:to_list(Contacts),
            From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
            loop(Contacts);
        {From, {update, Contact}} ->
            {Name, _} = Contact,
            case dict:is_key(Name, Contacts) of
                false ->
                    From ! {self(), {error, Name, is_not_in_dict}},
                    loop(Contacts);
                true -> 
                    From ! {self(), {Name, is_added}},
                    loop(dict:store(Name, Contact, Contacts))
            end;


        {From, {updFun,Fun}} ->
            From ! {self(), phonebook_version_updated},
            Fun(Contacts)
        % {From, Other} ->
        %     From ! {self(), {error,Other}},
        %     loop(Contacts)
    end.

pbupdate(Pid, Fun) -> 
    Pid ! {self(), {updFun,Fun}},
    receive
        {Pid, phonebook_version_updated} ->
            phonebook_version_updated
    end.

emptymq() ->
    receive
        Message -> 
            io:format("~p ~n", [Message]),
        emptymq()
    after 100 ->
        {no_more_messages}
    end.











        % {From, {fetch, Name}} ->
        %     try dict:fetch(Name, Contacts) of
        %         Element ->
        %             From ! {self(), Element}
        %     catch
        %         _:_ ->
        %             From ! {self(), {Name, not_in_phonebook}}
        %     end,            
        %     loop(Contacts);
        % {From, list_all} ->
        %     List = dict:to_list(Contacts),
        %     From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
        %     loop(Contacts);
        % {From, {update, Contact}} ->
        %     {Name, _} = Contact,
        %     case dict:is_key(Name, Contacts) of
        %         true ->
        %             From ! {self(), {Name, is_updated}},
        %             loop(dict:store(Name, Contact, Contacts));
        %         false -> 
        %             From ! {self(), {error, Name, is_not_in_dict}},
        %             loop(Contacts)
        %     end;
