%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013 
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : Henrik Bendt
%%% KU-ID   : gwk553
%%%---------------------------------------------------------------------

-module(swarm).
-export([start/2, stop/1, addFish/1]).

%Bound is size of fish school, and Limit is number of fish at a point.
%Returns pid of fish school.
start(Bound, Limit) ->
    case quadtree:start(Bound,Limit) of
        error ->    error;
        {_, Qid} -> %Time interval should be 50 = 50 ms
                    Tref = timer:apply_interval(50, ?MODULE, updateFishSchool, [Qid]),
                    {ok, spawn(fun() -> handleFish(0, Qid, Tref) end)}
    end.

% apply_interval(50, quadtree, mapFunction/3, [Qid, moveFish/1, universe]),


% Stop a fish school.
stop(Pid) -> send_stop(Pid).

% Add a fish the given fish school (pid)
addFish(Pid) ->
    info(Pid, addFish).

% A fish property contains an unique id, a velocity vector, 
% a velocity change vector and the number of times this has been updated.
handleFish(Id, Qid, Tref) ->
    receive
        {addFish} ->
            NewId = Id+1,
            Property = {NewId, {0,0}, {0,0},0},
            quadtree:addElement(Qid, {0,0}, Property), % position=(0,0)
            handleFish(NewId, Qid, Tref);
        {'EXIT', _, normal} ->  
            timer:cancel(Tref),
            quadtree:stop(Qid),         %kill quadtree processes.
            exit(normal)                %kill self.
    end.

% Update the fish school every 50 ms.
updateFishSchool(Qid) ->
    % Update the whole tree, that is, universe. Do not care about apply_interval returns
    quadtree:mapFunction(Qid, moveFish, universe),
    % Attraction / repulsion phase
    % Map on all fish the velocity change vector calculations
    quadtree:mapFunction(Qid, fun(Fish) -> calculateVelocityChange(Qid, Fish) end, universe).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for calculating velocity change vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculate and update velocity change vector of all fish.
% First calculates the repulsion phase, then the attraction.
calculateVelocityChange(Qid, Fish) -> 
    RepulsionBound = calculateRepulsionZone(Fish),
    quadtree:mapFunction(
        Qid, 
        fun(RFish) -> 
            updateChangeVector(Fish, calculateRepulsion(Fish, RFish)) 
        end, 
        RepulsionBound
    ),
    AttractionBound = calculateAttractionZone(Fish),
    quadtree:mapFunction(
        Qid, 
        fun(AFish) -> 
            updateChangeVector(Fish, calculateAttraction1(Fish, AFish)) 
        end, 
        AttractionBound
    ).

calculateAttraction1(Fish, AFish) ->
    {_, _, Prop} = Fish,
    {_, _, FishVCV, _} = Prop,
    case distanceFish(Fish, AFish) < 7 of % upper bound limited by Bound (zone)
        true -> FishVCV; % out of attraction zone
        false -> calculateAttraction(Fish, AFish) %Within attraction zone
    end.

calculateRepulsionZone(Fish) ->
    {element, Pos, _} = Fish,
    {X,Y} = Pos,
    {X-3,Y-3,X+3,Y+3}.

calculateAttractionZone(Fish) ->
    {element, Pos, _} = Fish,
    {X,Y} = Pos,
    {X-10,Y-10,X+10,Y+10}.

%% Asynchronous communication
info(Pid, Msg) ->
    Pid ! Msg.

send_stop(Pid) ->
    exit(Pid, normal).

%USE pman:start(). TO SEE RUNNING PROCESSES.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for vector handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Update velocity change vector of given fish 
% by adding given vector. Also update number of times the change vector
% has been updated.
updateChangeVector(Fish, VelocityCV) ->
	{element, Pos, Prop} = Fish,
	{ID, Velocity, FishVCV, Nd} = Prop,
	{element, Pos, {ID, Velocity, addVectors(FishVCV, VelocityCV), Nd + 1}}.

% Calculates the attraction velocity between the fish at the centre and
% a fish that is expected to be in the attraction zone.
% There is no check for attraction zone in this function.
calculateAttraction(FishCentre, FishInAttractionZone) ->
	{element, PosCF, _} = FishCentre,
	{element, PosAF, _} = FishInAttractionZone,
	toUnitVector(subtractVectors(PosCF, PosAF)).

% Calculates the repulsion velocity between the fish at the centre and
% a fish that is expected to be in the repulsion zone.
% There is no check for repulsion zone in this function.
calculateRepulsion(FishCentre, FishInRepulsionZone) ->
	{element, PosCF, _} = FishCentre,
	{element, PosRF, _} = FishInRepulsionZone,
	toUnitVector(subtractVectors(PosRF, PosCF)).

% Updates the velocity vector of a fish with the velocity difference vector.
% The moves the fish it according to the new velocity vector,
% and finally returns the updated fish with the reset difference vector.
% Maximum speed is 3.
moveFish(Fish) ->
	{element, Pos, Prop} = Fish,
	{ID, {Vx, Vy}, {Dvx, Dvy}, Nd} = Prop,
	NewV = toMaxLenVector({Vx + Dvx/Nd, Vy + Dvy/Nd},3),
	{element, addVectors(Pos, NewV), {ID, NewV, {0,0}, 0}}.

% Returns the distance between two fish
distanceFish(Fish1, Fish2) ->
	{element, Pos1, _} = Fish1,
	{element, Pos2, _} = Fish2,
	distance(Pos1, Pos2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper vector-functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adds two vectors and returns their sum
addVectors({X1,Y1},{X2,Y2}) ->
	{X1+X2, Y1+Y2}.

% Adds two vectors and returns their sum
subtractVectors({X1,Y1},{X2,Y2}) ->
	{X1-X2, Y1-Y2}.

% Transforms a vector to a unit length (vector of length = 1)
toUnitVector(Vector) ->
	Len = vectorLength(Vector),
	{X,Y} = Vector,
	{X/Len, Y/Len}.

% Returns the length of a vector
vectorLength(Vector) ->
	{X,Y} = Vector,
	math:sqrt(math:pow(X,2)+math:pow(Y,2)).

% Returns a vector of maximum MaxLen length.
toMaxLenVector(Vector,MaxLen) ->
	Len = vectorLength(Vector),
	{X,Y} = Vector,
	case Len > MaxLen of
		true  -> {X*MaxLen/Len, Y*MaxLen/Len};
		false -> Vector
	end.

% Returns the distance between two positions
distance(Pos1, Pos2) ->
	{X1,Y1} = Pos1,
	{X2,Y2} = Pos2,
	math:sqrt(math:pow(X1-X2,2)+math:pow(Y1-Y2,2)).
