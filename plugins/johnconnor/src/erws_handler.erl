-module(erws_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-include("proto.hrl").
-include("common.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(Req, State) ->
    lager:debug("Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
    account:login(),
    {ok, Req, #state{ logged = true }}.

websocket_handle({text, Msg}, Req, State) ->
    case parser:do(Msg, State) of
        {reply, nochange} ->
            {reply, {text, <<"{\"reply\":\"ok\"}">>}, Req, State, hibernate};
        {reply, NewState} ->
            {reply, {text, <<"{\"reply\":\"ok\"}">>}, Req, NewState, hibernate};
        {error, _Error} ->
            {reply, {text, <<"{\"reply\":\"parsing_error\"}">>}, Req, State, hibernate}
    end;
websocket_handle(_Any, Req, State) ->
    {reply, {text, <<"whut?">>}, Req, State, hibernate}.

websocket_info({dispatch, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, Req, State) ->
    terminate(Reason, Req, State).

terminate(_Reason, _Req, #state{logged = Logged}) ->
    lager:info("terminate: ~p", [self()]),
    devices ! {logout, {self(), Logged}},
    ok.
