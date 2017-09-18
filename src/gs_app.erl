-module(gs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/gs_user.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    DispatchHttps = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, gs, "index.html"}},
	        {"/static/[...]", cowboy_static, {priv_dir, gs, "static"}},
            {"/login", https_login_handler, #{}}
	    ]}
    ]),
    DispatchWs = cowboy_router:compile([
        {'_', [
            {"/websocket", ws_user_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_tls(https, [
        {port, 8080},
        {certfile, "priv/ssl/www.gsserver.com.crt"},
        {keyfile, "priv/ssl/device.key"}], #{
       	env => #{dispatch => DispatchHttps}
    }),
    {ok, _} = cowboy:start_tls(wss, [
        {port, 8081},
        {certfile, "priv/ssl/www.gsserver.com.crt"},
        {keyfile, "priv/ssl/device.key"}], #{
       	env => #{dispatch => DispatchWs},
        middlewares => [cowboy_router, gs_auth, cowboy_handler, ws_user_handler]
    }),
    % spawn(fun () -> observer:start() end),
    gs_sup:start_link().

stop(_State) ->
    ok.
