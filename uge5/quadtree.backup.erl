%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013
%%% Updated : Aug 2013
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : Henrik Bendt
%%% KU-ID   : gwk553
%%%---------------------------------------------------------------------

-module(quadtree).

-export([start/2, stop/1, addElement/3, mapFunction/3, mapTreeFunction/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts a quadtree with 
%   Bound: Eucledean size of tree. Will not accept bounds of empty or universe.
%   Limit: Maximum elements in each leaf
start(Bound, Limit) ->
    case Bound of
        universe -> error;
        empty -> error;
        _     -> 
            {ok, spawn(fun() -> quadtreeCoordinatorInit(Bound, Limit) end)}.

%Stops all processes in the quadtree
stop(Qtree) -> send_stop(Qtree).

% Adds and element 
addElement(Qtree, Pos, Property) ->
    send_add(Qtree, {element, Pos, Property}).

% Maps the function MapFun over all elements within bound.
mapFunction(Qtree, MapFun, Bound) ->
    send_mapFun(Qtree, MapFun, Bound).

% Maps the function MapTreeFun over the bound of each node and leaf in Qtree.
% Not expected to return anything and will not update the Qtree.
mapTreeFunction(Qtree, MapTreeFun) ->
    send_mapTree(Qtree, MapTreeFun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Asynchronous communication
info(Pid, Msg) ->
    Pid ! Msg.

send_add(Pid, Element) ->
    info(Pid, {addElement,Element}).

send_mapFun(Pid, Fun, MapBound) ->
    info(Pid, {mapFun, Fun, MapBound}).

send_mapTree(Pid, MapTreeFun) ->
    info(Pid, {mapTreeFun, MapTreeFun}).

%send_stop(Pid) -> %Well this is not a pretty way to do it, as it does not verify parent process as commander.
%    info(Pid, stop). 
send_stop(Pid) ->
    exit(Pid, normal).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Quadtree Coordinator

%Initial qtree is a leaf, with an empty list of elements, and the coordinator process as parent.
quadtreeCoordinatorInit(Bound, Limit) ->
    Self = self(),
    QtreeTop = spawn(fun() -> qtreeLeaf(Bound, Limit, Self, []) end),
    quadtreeCoordinator(QtreeTop, Bound).


quadtreeCoordinator (QtreeTop, Bound) ->
    receive
        {addElement, Element} -> 
            case outOfBound(Bound, Element) of
                true ->
                    quadtreeCoordinator(QtreeTop, Bound);
                false ->
                    send_add(QtreeTop, Element),
                    quadtreeCoordinator(QtreeTop, Bound)
            end;
        {mapFun, MapFun, MapBound} ->
            case intersectBounds(Bound,MapBound) of %Given bound must be within bound of qtree.
                empty -> send_mapFun(QtreeTop, MapFun, MapBound);
                _ -> quadtreeCoordinator(QtreeTop, Bound)
            end;
        {mapTreeFun, MapTreeFun} ->
            catch MapTreeFun(Bound),
            send_mapTree(QtreeTop, MapTreeFun),
            quadtreeCoordinator(QtreeTop, Bound);
        {'EXIT', _, normal} -> 
            send_stop(QtreeTop),        %kill tree processes.
            exit(normal)                %kill self.
    end.

%% Quadtree node 
% TODO: Is Limit necessary, when initing in leaf?
qtreeNode(Bound, Parent, Children) ->
    receive
        {addElement, Element} -> 
            {element, Pos, _} = Element,
            case outOfBound(Bound, Element) of %Check if element is out of bounds.
                true ->
                    send_add(Parent, Element);
                false ->
                    case Bound of
                        universe -> 
                            qtreeNode(Bound, Parent, Children); %Do nothing
                        empty -> 
                            qtreeNode(Bound, Parent, Children); %Do nothing
                        _ -> 
                            Quarter = findQuarter(Pos, Bound),
                            send_add(dict:fetc(Quarter, Children), Element),
                            qtreeNode(Bound, Parent, Children)
                    end;
            end;
        {mapFun, MapFun, MapBound} ->
            case intersectBounds(Bound, MapBound) of
                empty -> qtreeNode(Bound, Parent, Children);
                _ -> 
                    %dict:map(fun(C) -> send_mapFun(C, MapFun, MapBound) end, Children),
                    lists:foreach(fun({_,Child}) -> send_mapFun(Child, MapFun, MapBound) end, dict:to_list(Children)),
                    qtreeNode(Bound, Parent, Children)
            end;
        {mapTreeFun, MapTreeFun} ->
            catch MapTreeFun(Bound),
            lists:foreach(fun({_,Child}) -> send_mapTree(Child, MapTreeFun) end, dict:to_list(Children)),
            qtreeNode(Bound, Parent, Children);
        {'EXIT', Parent, normal} ->
            lists:foreach(fun({_,Child}) -> send_stop(Child) end, dict:to_list(Children)), %killingspree on children (processes)
            exit(normal)                                       %kill self.
    end,
    qtreeNode(Bound, Parent, Children).


%% Quadtree leaf. Assumes position of element is within bounds.
qtreeLeaf(Bound, Limit, Parent, Data) ->
    receive
        {addElement, Element} -> 
            DataNew = [Element|Data],
            case length(DataNew) =< Limit of
                true ->
                    qtreeLeaf(Bound, Limit, Parent, DataNew);
                false ->
                    % create new leaves
                    
                    %Leafnw = newQtreeLeaf(nw, Bound, Limit, self()),
                    %Leafne = newQtreeLeaf(ne, Bound, Limit, self()),
                    %Leafsw = newQtreeLeaf(sw, Bound, Limit, self()),
                    %Leafse = newQtreeLeaf(se, Bound, Limit, self()),
                    %Children = dict:from_list([{nw, Leafnw},{ne,Leafne},{sw,Leafsw},{se,Leafse}]),
                    Self = self(),
                    Leafs = lists:map(fun(Quarter) -> newQtreeLeaf(Quarter, Bound, Limit, Self) end, qTreeQuarters()),
                    Children = dict:from_list(lists:zip(qTreeQuarters(), Leafs)),

                    % reposition data elements by sending them to self, as self is soon a node (they will only be handled when self=node).
                    lists:foreach(fun(E) -> send_add(Self,E) end, DataNew),
                    % Turn into a node by running qtreeNode.
                    qtreeNode(Bound, Parent, Children)
            end;
        {mapFun, MapFun, MapBound} ->
            case intersectBounds(Bound, MapBound) of
                empty -> qtreeLeaf(Bound, Limit, Parent, Data);
                BoundTmp -> %This also includes universe
                    try
                        DataNew = lists:map(fun(E) -> 
                                                case outOfBound(BoundTmp, E) of
                                                    false -> MapFun(E);
                                                    _ -> E
                                                end
                                            end,
                                            Data),
                        qtreeLeaf(Bound, Limit, Parent, DataNew)
                    catch % If non-successful, do nothing.
                        _ -> qtreeLeaf(Bound, Limit, Parent, Data)
                    end
            end;
        {mapTreeFun, MapTreeFun} ->
            catch MapTreeFun(Bound),
            qtreeLeaf(Bound, Limit, Parent, Data);
        {'EXIT', Parent, normal} -> exit(normal)    %kill self.
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a new (empty) leaf in the given quarter of the given bound.
% Returns Pid.
newQtreeLeaf(Quarter, Bound, Limit, Parent) ->
    BoundNew = createBound(Quarter, Bound),
    spawn(fun() -> qtreeLeaf(BoundNew, Limit, Parent, []) end).

% Find quarter of given position and bound. Assumes position is within bound.
% TODO: Default quater for universe and empty????
% Returns quarter.
findQuarter(_, universe) -> universe;
findQuarter(_, empty) -> empty;
findQuarter({X,Y}, {X1, Y1, X2, Y2}) ->
    Xmid = X1 + ((X2 - X1) / 2),
    Ymid = Y1 + ((Y2 - Y1) / 2),
    if
        X =< Xmid , Y =< Ymid -> nw;
        X > Xmid , Y =< Ymid -> ne;
        X =< Xmid , Y > Ymid -> sw;
        X > Xmid , Y > Ymid -> se
    end.

% The following is suggestions for useful helpler function,
% which can make your loops easier to follow.

% Organization of quadtree:
% +---------+-- -> X
% | NW | NE |
% |----+----|
% | SW | SE |
% +---------+
% |
% v
% Y

% List with quarters in the quadtree.
qTreeQuarters() -> [nw,ne,sw,se].

% Returns the 1/4 of Bound which is defined by Quarter from the list above.
createBound(_, universe) -> universe;
createBound(_, empty) -> empty;
createBound(Quarter,Bound) -> 
    {X1, Y1, X2, Y2} = Bound,
    Xmid = X1 + ((X2 - X1) / 2),
    Ymid = Y1 + ((Y2 - Y1) / 2),
    case Quarter of
        nw -> {X1,   Y1,   Xmid, Ymid};
        ne -> {Xmid, Y1,   X2,   Ymid};
        sw -> {X1,   Ymid, Xmid, Y2  };
        se -> {Xmid, Ymid, X2,   Y2  }
    end.

% Predicate that returns true if an Element is outside Bound.
outOfBound(universe, _) -> false;
outOfBound(empty, _) -> true;
outOfBound(Bound, Element) ->
    {X1, Y1, X2, Y2} = Bound,
    {element, Pos, _} = Element,
    {X,Y} = Pos,
    (X < X1) or (X >= X2) or (Y < Y1) or (Y >= Y2).

% Finds the intersection between Bound1 and Bound2.
intersectBounds(universe, Bound)    -> Bound;
intersectBounds(Bound,    universe) -> Bound;
intersectBounds(empty,    _    )    -> empty;
intersectBounds(_,        empty)    -> empty;
intersectBounds(Bound,    Bound)    -> Bound; 
intersectBounds(Bound1,   Bound2)   ->
    {X1_1, Y1_1, X1_2, Y1_2} = Bound1,
    {X2_1, Y2_1, X2_2, Y2_2} = Bound2,
    X1 = lists:max([X1_1, X2_1]),
    Y1 = lists:max([Y1_1, Y2_1]),
    X2 = lists:min([X1_2, X2_2]),
    Y2 = lists:min([Y1_2, Y2_2]),
    case (X1 >= X2) or (Y1 >= Y2) of
        true  -> empty;
        false -> {X1, Y1, X2, Y2}
    end.
