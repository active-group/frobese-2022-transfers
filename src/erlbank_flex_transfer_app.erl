-module(erlbank_flex_transfer_app).
-behaviour(application).
-export([start/2, stop/1]).


start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/transfers/create", web_frontend, create_transfer}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8001}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->

    lager:info("Starting transfer-service: ~p~n", [node()]),

    start_cowboy(),
    database:init_database(),

    Res = erlbank_flex_transfer_sup:start_link(),
    lager:info("Started transfer feed: ~p~n", [node()]),
    Res.


stop(_State) ->
    %% database:destroy_tables().
    ok.

%% internal functions
