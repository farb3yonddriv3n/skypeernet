-module(parser).
-export([do/2]).

-include("proto.hrl").

parse({[{<<"command">>, Cmd}, {<<"payload">>, {Payload}}, {<<"version">>, Version}, {<<"request_id">>, RequestId}]}) ->
    {Cmd, Payload, Version, RequestId};
parse(_Fail) ->
    error.

do(Data, State) ->
    try
        Json = jiffy:decode(Data),
        Parsed = parse(Json),
        case Parsed of
            {<<"peers_get">>, Payload, _Version, RequestId} ->
                peers:get(Payload, State, RequestId),
                {reply, nochange};
            {<<"files_get">>, Payload, _Version, RequestId} ->
                files:get(Payload, State, RequestId),
                {reply, nochange};
            {<<"job_add">>, Payload, _Version, RequestId} ->
                files:job_add(Payload, State, RequestId),
                {reply, nochange};
            {<<"job_finalize">>, Payload, _Version, RequestId} ->
                files:job_finalize(Payload, State, RequestId),
                {reply, nochange};
            {<<"tunnel_open">>, Payload, _Version, RequestId} ->
                tunnel:open(Payload, State, RequestId),
                {reply, nochange};
            {<<"tunnel_dump">>, Payload, _Version, RequestId} ->
                tunnel:dump(Payload, State, RequestId),
                {reply, nochange};
            {<<"endpoint_dump">>, Payload, _Version, RequestId} ->
                endpoint:dump(Payload, State, RequestId),
                {reply, nochange};
            {<<"account_login">>, Payload, _Version, _RequestId} ->
                NewState = account:login(Payload, State),
                {reply, NewState};
            {<<"account_logout">>, Payload, _Version, _RequestId} ->
                NewState = account:logout(Payload, State),
                {reply, NewState};
            error ->
                throw(metadata)
        end
    catch
        E1:E2 ->
            lager:info("parser error ~p ~p ~p", [E1, E2, Data]),
            {error, {parsing, E1, E2}}
    end.
