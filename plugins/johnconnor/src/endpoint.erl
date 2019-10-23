-module(endpoint).
-export([dump/3]).

-include("proto.hrl").
-include("common.hrl").

dump(_Payload, State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_endpoint_dump{ request_id = RequestId }).
