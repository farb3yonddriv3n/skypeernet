-module(tunnel).
-export([open/3]).

-include("proto.hrl").
-include("common.hrl").

open([{ <<"pubkeyhash">>, PubKeyHash }, { <<"port">>, Port }], _State, RequestId) ->
    utils:sendpipe(#proto_tunnel_open{ pubkeyhash = PubKeyHash,
                                       port       = Port,
                                       request_id = RequestId }).
