-module(utils).
-export([send/1, send/2, sendpipe/1]).

-include("proto.hrl").
-include("common.hrl").

encode(Command, Payload) ->
    jiffy:encode({[{<<"command">>, Command}, {<<"payload">>, Payload}]}).

encodepipe(Cmd) ->
    jiffy:encode({Cmd}).

reply(#proto_account_signup_reply{error = Error}) ->
    Payload = {[{<<"error">>, Error}]},
    encode(<<"account_signup_reply">>, Payload);
reply(#proto_account_login_reply{error = Error}) ->
    Payload = {[{<<"error">>, Error}]},
    encode(<<"account_login_reply">>, Payload);
reply(#proto_account_logout_reply{error = Error}) ->
    Payload = {[{<<"error">>, Error}]},
    encode(<<"account_logout_reply">>, Payload);
reply(#proto_broadcast{pid = Pid, msg = Message}) ->
    Payload = {[{<<"pid">>, Pid},
                {<<"msg">>, Message}]},
    encode(<<"broadcast">>, Payload);
reply(#proto_device_offline{ pid = Pid }) ->
    Payload = {[{<<"pid">>, list_to_binary(pid_to_list(Pid)) }]},
    encode(<<"device_offline">>, Payload);
reply(#proto_devices_online{ list = List }) ->
    PidDevList = [ {[{<<"pid">>,    list_to_binary(pid_to_list(Pid)) }]}
                   || {Pid, _Params} <- List ],
    Payload = {[{<<"devices">>, PidDevList}]},
    encode(<<"devices_online">>, Payload);
reply(#proto_camera_request{ src = Src, dst = Dst, snapshot = Snapshot }) ->
    Payload = {[{<<"src">>, Src},
                {<<"dst">>, Dst},
                {<<"snapshot">>, Snapshot}]},
    encode(<<"camera_request">>, Payload);
reply(#proto_camera_reinit{ src = Src }) ->
    Payload = {[{<<"src">>, Src}]},
    encode(<<"camera_reinit">>, Payload);
reply(#proto_camera_response{ error = Error, pid = Pid, image = Image,
                              objects = Objects, timestamp = Timestamp,
                              snapshots = Snapshots, temp = Temp }) ->
    Payload = {[{<<"pid">>,       Pid},
                {<<"image">>,     Image},
                {<<"objects">>,   Objects},
                {<<"timestamp">>, Timestamp},
                {<<"error">>,     Error},
                {<<"snapshots">>, Snapshots},
                {<<"temp">>     , Temp}]},
    encode(<<"camera_response">>, Payload);
reply(#proto_message{ data = Data }) ->
    Payload = Data,
    encode(<<"message">>, Payload);
reply(#proto_files_get{ src = <<"local">> }) ->
    encode(2, {[{<<"test">>, 1}]});
reply(#proto_files_get{ src = <<"remote">> }) ->
    encode(3, {[{<<"test">>, 1}]});
reply(#proto_job_add{ name = Name }) ->
    encodepipe([{<<"command">>, 8}, {<<"name">>, Name}]);
reply(#proto_job_finalize{ name = Name }) ->
    encodepipe([{<<"command">>, 9}, {<<"name">>, Name}]);
reply(_) ->
    error.

sendpipe(Proto) ->
    Reply = reply(Proto),
    pipe:write(Reply).
send(Pid, Proto) ->
    Reply = reply(Proto),
    Pid ! {dispatch, Reply},
    ok.
send(Proto) ->
    send(self(), Proto).
