-module(gs_session_cleaner).

-behaviour(gen_server).

%% API
-export([stop/0]).

% gen_server callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TTL, 5000).

-record(state, {tref}).

%% API
stop() ->
    gen_server:call(stop).

%% gen_server callbacks
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    % {ok, TRef} = timer:send_interval(64, self(), ),
    % {ok, #state{tref = TRef}}.
    {ok, #state{}}.

%% TODO - should remove this from supervision tree??????
%% This will just spawn again?????
handle_call(stop, _From, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
clean_sessions() ->
    ok.