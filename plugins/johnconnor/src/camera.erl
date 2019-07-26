-module(camera).

-export([request/2, request_done/2, reinit/2]).

-include("common.hrl").

reinit([{<<"pid">>, BinPid}], _State) ->
    Dst = list_to_pid(binary_to_list(BinPid)),
    case is_pid(Dst) of
        true ->
            Self = list_to_binary(pid_to_list(self())),
            utils:send(Dst, #proto_camera_reinit{ src = Self });
        false ->
            error
    end.

request([{<<"pid">>, BinPid},
         {<<"snapshot">>, Snapshot}], _State) ->
    Dst = list_to_pid(binary_to_list(BinPid)),
    case is_pid(Dst) of
        true ->
            Self = list_to_binary(pid_to_list(self())),
            utils:send(Dst, #proto_camera_request{ src = Self,
                                                   dst = BinPid,
                                                   snapshot = Snapshot });
        false ->
            error
    end.

request_done([{<<"dst">>,       DstPid},
              {<<"error">>,     Error},
              {<<"image">>,     Image},
              {<<"objects">>,   Objects},
              {<<"snapshots">>, Snapshots},
              {<<"src">>,       SrcPid},
              {<<"temp">>,      Temp},
              {<<"timestamp">>, Timestamp}], _State) ->
    Pid = list_to_pid(binary_to_list(SrcPid)),
    utils:send(Pid, #proto_camera_response{ pid       = DstPid,
                                            image     = Image,
                                            objects   = Objects,
                                            timestamp = Timestamp,
                                            error     = Error,
                                            snapshots = Snapshots,
                                            temp      = Temp }).
