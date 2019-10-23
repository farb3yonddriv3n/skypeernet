-module(tunnel).
-export([open/3, dump/3]).

-include("proto.hrl").
-include("common.hrl").

open([{ <<"pubkeyhash">>, PubKeyHash }, { <<"port">>, Port }], State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_tunnel_open{ pubkeyhash = PubKeyHash,
                                       port       = Port,
                                       request_id = RequestId }).

dump(_Payload, State, RequestId) ->
    utils:sendpipe(State#state.fifowrite, #proto_tunnel_dump{ request_id = RequestId }).
