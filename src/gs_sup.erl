-module(gs_sup).

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
init([]) ->
    Procs = [{gs_game_sup, 
                {gs_game_sup, start_link, []},
                permanent, 
                infinity, 
                supervisor,
                [gs_game_sup]},
             {gs_rc_sup,
                {gs_rc_sup, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [gs_rc_sup]}],
    {ok, { {one_for_one, 10, 10}, Procs} }.
    