-module(notify).
-export([login/2, logout/2, message/2]).

-include("common.hrl").

login(ListDevices, Pid) ->
    % first, notify all devices when new device comes online
    Fun = fun({LPid, _Params}) ->
            utils:send(LPid, #proto_devices_online{ list = [{Pid}] })
          end,
    lists:map(Fun, ListDevices),

    % notify a freshly logged device about all online devices
    utils:send(Pid, #proto_devices_online{ list = ListDevices }).

logout(Pid, ListDevices) ->
    Fun = fun({LPid, _Params}) ->
            utils:send(LPid, #proto_device_offline{ pid = Pid })
          end,
    lists:map(Fun, ListDevices).

message(Devices, Data) ->
    Fun = fun({LPid, _Params}) ->
            utils:send(LPid, #proto_message{ data = list_to_binary(Data) })
          end,
    lists:map(Fun, Devices).
