-module(tunnel).
-export([open/3]).

-include("proto.hrl").
-include("common.hrl").

open([{ <<"pubkeyhash">>, PubKeyHash }], _State, RequestId) ->
    utils:sendpipe(#proto_tunnel_open{ pubkeyhash = PubKeyHash, request_id = RequestId }).
