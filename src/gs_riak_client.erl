-module(gs_riak_client).

-behaviour(gen_server).

%% API
-export([get_bucket/1,
         get_session/1,
         put/1,
         list_keys/1,
         delete/2,
         clear_sessions/0]).

%% gen_server callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pid}).

%% API
get_bucket(Bucket) ->
    gen_server:call(?SERVER, {get_bucket, Bucket}).

get_session(SessionId) ->
    gen_server:call(?SERVER, {get_session, SessionId}).

put(Object) ->
    gen_server:call(?SERVER, {put, Object}).

list_keys(Bucket) ->
    gen_server:call(?SERVER, {list_keys, Bucket}).

delete(Bucket, Key) ->
    gen_server:call(?SERVER, {delete, Bucket, Key}).

clear_sessions() ->
    gen_server:call(?SERVER, clear_sessions).

%% gen_server callbacks
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    State = #state{pid = Pid},
    {ok, State}.


handle_call(clear_sessions, _From, State) ->
    Bucket = <<"session">>,
    {ok, Keys} =riakc_pb_socket:list_keys(State#state.pid, Bucket),
    Resp = [riakc_pb_socket:delete(State#state.pid, Bucket, Key) || Key <- Keys],
    {reply, Resp, State};
handle_call({delete, Bucket, Key}, _From, State) ->
    Resp = riakc_pb_socket:delete(State#state.pid, Bucket, Key),
    {reply, Resp, State};
handle_call({list_keys, Bucket}, _From, State) ->
    Resp = riakc_pb_socket:list_keys(State#state.pid, Bucket),
    {reply, Resp, State};
handle_call({put, Object}, _From, State) ->
    Resp = riakc_pb_socket:put(State#state.pid, Object),
    {reply, Resp, State};
handle_call({get_session, SessionId}, _From, State) ->
    Resp = riakc_pb_socket:get(State#state.pid, <<"session">>, SessionId),
    {reply, Resp, State};
handle_call({get_bucket, Bucket}, _From, State) ->
    io:format("~p~n", [Bucket]),
    Resp = riakc_pb_socket:get_bucket(State#state.pid, Bucket),
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.