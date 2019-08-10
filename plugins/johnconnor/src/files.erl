-module(files).
-export([get/2, job_add/2, job_finalize/2]).

-include("proto.hrl").
-include("common.hrl").

get([{ <<"src">>, Src }], _State) ->
    utils:sendpipe(#proto_files_get{ src = Src }).

job_add([{ <<"name">>, Name }], _State) ->
    utils:sendpipe(#proto_job_add{ name = Name }).
job_finalize([{ <<"name">>, Name }], _State) ->
    utils:sendpipe(#proto_job_finalize{ name = Name }).
