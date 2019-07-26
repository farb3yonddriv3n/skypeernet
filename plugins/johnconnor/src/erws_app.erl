-module(erws_app).
-behaviour(application).
-export([start/2, stop/1]).

-include("common.hrl").

mnesia_start() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    true = register(devices, spawn(devices, start, [])),
    true = register(pipe, spawn(pipe, read, [])),
    case mnesia:create_table(db_account,
                             [{attributes, record_info(fields, db_account)},
                              {disc_copies, [node()]},
                              {type, set}]) of
        Result when
        Result =:= {aborted, {already_exists, db_account}};
        Result =:= {atomic, ok} ->
            mnesia:change_config(extra_db_nodes, nodes()),
            ok;
        Err ->
            lager:debug("Database failed: ~p", [Err]),
            {error, db}
    end.

start(_StartType, _StartArgs) ->
    ok = mnesia_start(),
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/"             , cowboy_static, {priv_file, erws, "index.html"}},
        {"/assets/[...]" , cowboy_static, {priv_dir, erws,  "static/assets"}},
        {"/websocket"    , erws_handler,  []}
      ]}
        ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    erws_sup:start_link().

stop(_State) ->
    ok.
