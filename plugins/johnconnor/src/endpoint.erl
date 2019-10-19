-module(endpoint).
-export([dump/3]).

-include("proto.hrl").
-include("common.hrl").

dump(_Payload, _State, RequestId) ->
    utils:sendpipe(#proto_endpoint_dump{ request_id = RequestId }).
