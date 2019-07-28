-module(files).
-export([get/2]).

-include("proto.hrl").
-include("common.hrl").

get(_Payload, _State) ->
    utils:sendpipe(#proto_files_get{}).
