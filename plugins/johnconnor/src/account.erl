-module(account).

-export([login/0, logout/2]).

-include("common.hrl").

logout(_Payload, #state{ logged = Logged }) ->
    devices ! {logout, {self(), Logged}},
    utils:send(#proto_account_logout_reply{error = <<"ok">>}),
    #state{}.

login() ->
    devices ! {login, {self()}}.
