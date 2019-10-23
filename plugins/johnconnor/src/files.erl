-module(files).
-export([get/3, job_add/3, job_finalize/3]).

-include("proto.hrl").
-include("common.hrl").

get([{ <<"src">>, Src }], State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_files_get{ src = Src, request_id = RequestId }).

job_add([{ <<"name">>, Name }], State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_job_add{ name = Name, request_id = RequestId }).

job_finalize([{ <<"name">>, Name }], State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_job_finalize{ name = Name, request_id = RequestId }).
