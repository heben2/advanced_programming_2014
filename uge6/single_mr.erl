%% Single threadded map-reduce
%% 
%% Author: Michael Kirkedal Thomsen <shapper@diku.dk>

-module(single_mr).
-export([mr/4, test/1]).

mr(Datafun, Mapfun, Reducefun, Accu) ->
    Data  = Datafun(),
    Data2 = lists:map(Mapfun, Data),
    Data3 = lists:foldl(Reducefun, Accu, Data2),
    Data3.

test(N) ->
    Data = fun() -> lists:seq(1,N) end,
    Mapfun = fun(X) -> X * X end,
    Reducefun = fun(X,Acc) -> X + Acc end, % Symmetric function
    Acc = 0,
    mr(Data, Mapfun, Reducefun, Acc).
