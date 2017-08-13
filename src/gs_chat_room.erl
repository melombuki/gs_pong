-module(gs_chat_room).

-behaviour(gen_server).

% API
-export([new/2,
         broadcast/2,
         add/2,
         remove/2,
         delete/2]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {roomid, users = #{}, owner = <<>>}).

%%%===================================================================
%%% API
%%%===================================================================

new(RoomId, Owner) ->
    gen_server:start_link(?MODULE,  [RoomId, Owner], []).

broadcast(RoomId, Msg) ->
    gen_server:cast(RoomId, {broadcast, Msg}).

add(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {add, UserPid}).

remove(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove, UserPid}).

delete(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {delete, UserPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([RoomId, Owner]) ->
    {ok, #state{roomid=RoomId, owner=Owner}}.

handle_call({add, UPid}, _From, State) ->
    NewState = add_user(State, UPid),
    {reply, ok, NewState};

handle_call({remove, UPid}, _From, State) ->
    NewState = remove_user(State, UPid),
    {reply, ok, NewState};

handle_call({delete, SenderName}, _From, State) ->
    MsgBody = #{type => leave_room},
    MsgEach = fun (UserPid) ->
                    UserName = gs_chat_server:get_name(UserPid),
                    Msg = MsgBody#{identity => UserName},
                    gs_chat_server_out:send_message(UserPid, Msg)
    end,
    Users = maps:keys(State#state.users),
    lists:map(MsgEach, Users),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({broadcast, Msg}, State) ->
    send_all(State#state.users, Msg),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% TODO need to implement this. See repo for the blah_server_out module
send_all(UserMap, Message) ->
    % SendOne = fun (UPid, _) ->
    %         gs_chat_server_out:send_message(UPid, Message)
    %     end,
    % maps:map(SendOne, UserMap)
    io:format("Message: ~p~nUserMap: ~p~n", [Message, UserMap]),
    ok.

add_user(State, UserPid) ->
    NewUsers = maps:put(UserPid, true, State#state.users),
    State#state{users = NewUsers}.

remove_user(State, UserPid) ->
    NewUsers = maps:remove(UserPid, State#state.users),
    State#state{users = NewUsers}.