-module(erlbank_flex_transfer_app).
-behaviour(application).
-export([start/2, stop/1]).

check_is_set(Var) ->
    case os:getenv(Var) of
        false ->
            io:format("Missing var ~s~n", [Var]),
            halt(1);
        _ -> ok
    end.

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/transfers/create", web_frontend, create_transfer}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->

    lager:info("Starting transfer-service: ~p~n", [node()]),

    check_is_set("ACCOUNTS_HOST"),
    AccountNode = list_to_atom("accounts@" ++ os:getenv("ACCOUNTS_HOST")),

    start_cowboy(),
    database:init_database(),
    events:init_events(),

    Res = erlbank_flex_transfer_sup:start_link(),
    transfer_feed:start_link(dontCare),
    transfer:start({account_feed, AccountNode}),
    lager:info("Started transfer feed: ~p~n", [node()]),
    Res.


stop(_State) ->
    %% database:destroy_tables().
    ok.

%% internal functions
