-module(shoestring_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(NUM_ACCEPTORS,  100).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    Port = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(shoestring_http, ?NUM_ACCEPTORS, TransOpts, ProtoOpts),
    shoestring_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
    Pages = [
        {"/", index_handler, []}
    ],
    [{'_', Pages}].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
