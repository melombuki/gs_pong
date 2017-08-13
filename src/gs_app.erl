%%%-------------------------------------------------------------------
%% @doc gs public API
%% @end
%%%-------------------------------------------------------------------

-module(gs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/gs_user.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", cowboy_static, {priv_file, gs, "index.html"}},
	       {"/websocket", ws_user_handler, #user{}},
	       {"/static/[...]", cowboy_static, {priv_dir, gs, "static"}}
	      ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
       	env => #{dispatch => Dispatch}
    }),
    % spawn(fun () -> observer:start() end),
    gs_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
