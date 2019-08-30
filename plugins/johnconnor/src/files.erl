-module(files).
-export([get/3, job_add/3, job_finalize/3]).

-include("proto.hrl").
-include("common.hrl").

get([{ <<"src">>, Src }], _State, RequestId) ->
    utils:sendpipe(#proto_files_get{ src = Src, request_id = RequestId }).

job_add([{ <<"name">>, Name }], _State, RequestId) ->
    utils:sendpipe(#proto_job_add{ name = Name, request_id = RequestId }).

job_finalize([{ <<"name">>, Name }], _State, RequestId) ->
    utils:sendpipe(#proto_job_finalize{ name = Name, request_id = RequestId }).
