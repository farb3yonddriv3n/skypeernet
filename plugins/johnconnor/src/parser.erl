-module(parser).
-export([do/2]).

-include("proto.hrl").

parse({[{<<"command">>, Cmd}, {<<"payload">>, {Payload}}, {<<"version">>, Version}]}) ->
    {Cmd, Payload, Version};
parse(_Fail) ->
    error.

do(Data, State) ->
    try
        Json = jiffy:decode(Data),
        Parsed = parse(Json),
        case Parsed of
            {<<"files_get">>, Payload, _Version} ->
                files:get(Payload, State),
                {reply, nochange};
            {<<"job_add">>, Payload, _Version} ->
                files:job_add(Payload, State),
                {reply, nochange};
            {<<"job_finalize">>, Payload, _Version} ->
                files:job_finalize(Payload, State),
                {reply, nochange};
            {<<"account_signup">>, Payload, _Version} ->
                account:signup(Payload, State),
                {reply, nochange};
            {<<"account_login">>, Payload, _Version} ->
                NewState = account:login(Payload, State),
                {reply, NewState};
            {<<"account_logout">>, Payload, _Version} ->
                NewState = account:logout(Payload, State),
                {reply, NewState};
            {<<"camera_request">>, Payload, _Version} ->
                camera:request(Payload, State),
                {reply, nochange};
            {<<"camera_request_done">>, Payload, _Version} ->
                camera:request_done(Payload, State),
                {reply, nochange};
            {<<"broadcast">>, Payload, _Version} ->
                broadcast:send(Payload, State),
                {reply, nochange};
            {<<"camera_reinit">>, Payload, _Version} ->
                camera:reinit(Payload, State),
                {reply, nochange};
            error ->
                throw(metadata)
        end
    catch
        E1:E2 ->
            lager:info("parser error ~p ~p ~p", [E1, E2, Data]),
            {error, {parsing, E1, E2}}
    end.
