-define(RtoM(Name, Record), lists:foldl(fun({I, E}, Acc) -> Acc#{E => element(I, Record) } end, #{}, lists:zip(lists:seq(2, (record_info(size, Name))), (record_info(fields, Name))))).

-record(vec3, {
    x :: float(),
    y :: float(),
    z :: float()
}).

-record(entity, {
    %% List of network listeners
    listeners:: list(),

    %% Owner
    owner    :: binary(),

    %% Velocity
    velocity :: integer(),

    %% Position vector in space
    pos      :: #vec3{},
    %% Direction vector
    dir      :: #vec3{},
    %% Destination vector
    dst      :: #vec3{},

    %% Lifetime of an entity
    epoch    :: integer(),

    %% Waypoints
    waypoints:: list(),
    waypoints_idx :: integer()
}).
