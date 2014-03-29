-module(index_handler).

-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Ok.">>, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.
