%%
%% More advanced data container
%%

%% The problem I was experiencing was that loop/1 was not added to the exported files. 
%% (I had even added as a note to myself.)
%% Only by exporting the loop function I can call it in the recompiled module.

-module(datacontainer).
-export([start/1, appFun/2, print/1, loop/1]).


%% Interface

start(Data) -> 
    spawn(fun() -> loop(Data) end).

appFun(Pid, Fun) -> rpc(Pid,{app, Fun}).

print(Pid) -> rpc(Pid, print).
% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

loop(Data) ->
	receive
		{From, {app, Fun}} ->
			try Fun(Data) of
				NewData ->
					From ! {self(), {ok, NewData}},
					loop(NewData)
			catch
				_:_ ->
					From ! {self(), {fail, Data}},
					loop(Data)		
			end;
		 {From, print} ->
			From ! {self(), Data},
			loop(Data);
		newcode ->
		 	?MODULE:loop(Data)
	end.


