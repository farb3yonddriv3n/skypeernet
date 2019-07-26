-module(devices).

-export([start/0]).

start() ->
    loop([]).

loop(Devices) ->
    lager:info("Loop Devices: ~p", [Devices]),
    receive
        {login, {Pid}} ->
            notify:login(Devices, Pid),
            loop(lists:keystore(Pid, 1, Devices, {Pid, []}));
        {logout, {Pid, true}} ->
            case lists:keysearch(Pid, 1, Devices) of
                {value, {_Pid, _Params}} ->
                    NewDevices = loop(lists:keydelete(Pid, 1, Devices)),
                    notify:logout(Pid, NewDevices),
                    loop(NewDevices);
                false ->
                    loop(Devices)
            end;
        {logout, {_Pid, false, _Email}} ->
            loop(Devices);
        {message, {Data}} ->
            notify:message(Devices, Data),
            loop(Devices);
        Unknown ->
            lager:info("Unknown msg received ~p", [Unknown]),
            loop(Devices)
    end.
