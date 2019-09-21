-module(peers).
-export([get/3]).

-include("proto.hrl").
-include("common.hrl").

get(_Payload, _State, RequestId) ->
    utils:sendpipe(#proto_peers_get{ request_id = RequestId }).
