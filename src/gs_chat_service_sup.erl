-module(gs_chat_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
    Procs = [{gs_chat_service, 
             {gs_chat_service, start_link, []},
             permanent, 
             brutal_kill, 
             worker, 
             [gs_chat_service]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.
    