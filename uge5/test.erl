-module(test).

-compile(export_all).

-include_lib("eqc.hrl").

test() ->
	eqc:quickcheck(prop_data()).

prop_reverse() ->
  ?FORALL(Xs,list(int()),
    lists:reverse(lists:reverse(Xs)) == Xs).

prop_data() ->
  ?FORALL(Xs,list(int()),?FORALL(Fs,function1(list(int())),
    test_dc(Xs,Fs) == Fs(Fs(Xs)))).

test_dc(Xs, Fs) ->
	Pid = datacontainer:start(Xs),
	_ = datacontainer:appFun(Pid, Fs),
	Ys = datacontainer:appFun(Pid, Fs),
	{_, NewData} = Ys,
	NewData.
