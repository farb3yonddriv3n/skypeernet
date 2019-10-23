-module(peers).
-export([get/3]).

-include("proto.hrl").
-include("common.hrl").

get(_Payload, State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_peers_get{ request_id = RequestId }).
