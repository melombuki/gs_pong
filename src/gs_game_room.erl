-module(gs_game_room).

-behaviour(gen_server).

% API
-export([new/2,
         chat_broadcast/2,
         start_game/1,
         stop_game/1,
         add/2,
         remove/2,
         delete/2,
         handle_input/3]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(mochijson2, [encode/1]).

-define(SERVER, ?MODULE).
-define(GAME_OBJECT, gs_game_object).
-define(MIN_Y, 0).
-define(MAX_Y, 200).
-define(STEP_Y, 4).

-record(state, {roomid, users = #{}, owner = <<>>, game_objects, tref}).
-record(player, {player_number, position}).

%%%===================================================================
%%% API
%%%===================================================================

new(RoomId, Owner) ->
    gen_server:start_link(?MODULE, [RoomId, Owner], []).

chat_broadcast(RoomId, Msg) ->
    gen_server:cast(RoomId, {chat_broadcast, Msg}).

get_game_objects(RoomId) ->
    gen_server:call(RoomId, get_game_objects).

handle_input(RoomId, UserPid, Key) ->
    gen_server:cast(RoomId, {handle_input, UserPid, Key}).

start_game(RoomId) ->
    gen_server:cast(RoomId, start_game).

stop_game(RoomId) ->
    gen_server:cast(RoomId, stop_game).

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
    case maps:size(NewState#state.users) of
        0 ->
            {stop, shutdown, ok, State};
        _ ->
            {reply, ok, NewState}
    end;

handle_call({delete, _SenderName}, _From, State) ->
    Msg = #{type => leave_room},
    send_all_msg(State#state.users, Msg),
    {stop, shutdown, ok, State};

handle_call(get_game_objects, _From, State) ->
    {reply, State#state.game_objects, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({chat_broadcast, Msg}, State) ->
    send_all_msg(State#state.users, Msg),
    {noreply, State};

handle_cast({handle_input, UserPid, Key}, State) ->
    Player = maps:get(UserPid, State#state.users),
    UpdatedY = case Key of
        <<"ArrowUp">> ->
            max(?MIN_Y, Player#player.position - ?STEP_Y);
        <<"ArrowDown">> ->
            min(?MAX_Y, Player#player.position + ?STEP_Y);
        _ ->
            0
    end,
    Users = State#state.users,
    Users1 = maps:update(UserPid, Player#player{position = UpdatedY}, Users),
    State1 = State#state{users = Users1},
    {ok, Paddle1} = apply(?GAME_OBJECT, new, [{"paddle1", 10, UpdatedY, 20, 100, "white", true, []}]),
    {ok, Paddle2} = apply(?GAME_OBJECT, new, [{"paddle2", 770, 0, 20, 100, "white", true, []}]),
    {ok, Ball}    = apply(?GAME_OBJECT, new, [{"ball", 400, 150, 10, 10, "blue", true, []}]),
    {ok, Root}    = apply(?GAME_OBJECT, new, [{"root", 0, 0, 0, 0, "black", false, [Paddle1, Paddle2, Ball]}]),
    {noreply, State1#state{game_objects = Root}};

handle_cast(start_game, State) ->
    {ok, Paddle1} = apply(?GAME_OBJECT, new, [{"paddle1", 10, 0, 20, 100, "white", true, []}]),
    {ok, Paddle2} = apply(?GAME_OBJECT, new, [{"paddle2", 770, 0, 20, 100, "white", true, []}]),
    {ok, Ball}    = apply(?GAME_OBJECT, new, [{"ball", 400, 150, 10, 10, "blue", true, []}]),
    {ok, Root}    = apply(?GAME_OBJECT, new, [{"root", 0, 0, 0, 0, "black", false, [Paddle1, Paddle2, Ball]}]),
    io:format("~p~n", ["Starting the game..."]),
    {ok, TRef} = timer:send_interval(16, self(), tick_game_state),
    {noreply, State#state{tref = TRef, game_objects = Root}};

handle_cast(stop_game, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    io:format("~p~n", ["Stoping the game..."]),
    {noreply, State#state{tref = undefined}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick_game_state, State) ->
    GameObjects = tick_game_state(State#state.users, State#state.game_objects),
    {noreply, State#state{game_objects = GameObjects}};

handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% internal functions
%%%===================================================================

add_user(State, UserPid) ->
    PlayerState = #player{player_number = maps:size(State#state.users),
                          position = 0},
    UpdatedUsers = maps:put(UserPid, PlayerState, State#state.users),
    State#state{users = UpdatedUsers}.

remove_user(State, UserPid) ->
    io:format("User ~p left the game~n", [UserPid]),
    UpdatedUsers = maps:remove(UserPid, State#state.users),
    State#state{users = UpdatedUsers}.

send_all_msg(UserMap, Message) ->
    SendOne = fun (UPid, _) ->
        UPid ! {chat_broadcast, Message}
    end,
    maps:map(SendOne, UserMap),
    ok.

send_all_game_objects(UserMap, GameObjects) ->
    SendOne = fun (UPid, _) ->
        UPid ! {game_objects, GameObjects}
    end,
    maps:map(SendOne, UserMap),
    ok.

update_user_position() ->
    ok.

tick_game_state(UserMap, GameObjects) ->
    send_all_game_objects(UserMap, GameObjects),
    GameObjects.
    